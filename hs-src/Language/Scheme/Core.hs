{-# LANGUAGE CPP #-}

{- |
Module      : Language.Scheme.Core
Copyright   : Justin Ethier
Licence     : MIT (see LICENSE in the distribution)

Maintainer  : github.com/justinethier
Stability   : experimental
Portability : portable

This module contains Core functionality, primarily Scheme expression evaluation.
-}

module Language.Scheme.Core
    (
    -- * Scheme code evaluation
      evalLisp
    , evalLisp'
    , evalString
    , evalAndPrint
    , apply
    , continueEval
    , runIOThrows 
    , runIOThrowsREPL 
    -- * Core data
    , nullEnvWithImport
    , primitiveBindings
    , r5rsEnv
    , r5rsEnv'
    , r7rsEnv
    , r7rsEnv'
    , r7rsTimeEnv
    , version
    -- * Utility functions
    , findFileOrLib
    , getDataFileFullPath
    , replaceAtIndex
    , registerExtensions
    , showBanner
    , showLispError
    , substr
    , updateList
    , updateVector
    , updateByteVector
    -- * Internal use only
    , meval
    ) where
import qualified Paths_husk_scheme as PHS (getDataFileName, version)
#ifdef UseFfi
import qualified Language.Scheme.FFI
#endif
import Language.Scheme.Environments
import Language.Scheme.Libraries
import qualified Language.Scheme.Macro
import Language.Scheme.Parser
import Language.Scheme.Primitives
import Language.Scheme.Types
import Language.Scheme.Util
import Language.Scheme.Variables
import Control.Monad.Error
import Data.Array
import qualified Data.ByteString as BS
import qualified Data.Map
import Data.Maybe (fromMaybe, isNothing)
import Data.Version as DV
import Data.Word
import qualified System.Exit
import qualified System.Info as SysInfo
-- import Debug.Trace

-- |Husk version number
version :: String
version = DV.showVersion PHS.version

-- |A utility function to display the husk console banner
showBanner :: IO ()
showBanner = do
  putStrLn "  _               _        __                 _                          "
  putStrLn " | |             | |       \\\\\\               | |                         "
  putStrLn " | |__  _   _ ___| | __     \\\\\\      ___  ___| |__   ___ _ __ ___   ___  "
  putStrLn " | '_ \\| | | / __| |/ /    //\\\\\\    / __|/ __| '_ \\ / _ \\ '_ ` _ \\ / _ \\ "
  putStrLn " | | | | |_| \\__ \\   <    /// \\\\\\   \\__ \\ (__| | | |  __/ | | | | |  __/ "
  putStrLn " |_| |_|\\__,_|___/_|\\_\\  ///   \\\\\\  |___/\\___|_| |_|\\___|_| |_| |_|\\___| "
  putStrLn "                                                                         "
  putStrLn " http://justinethier.github.io/husk-scheme                              "
  putStrLn " (c) 2010-2014 Justin Ethier                                             "
  putStrLn $ " Version " ++ (DV.showVersion PHS.version) ++ " "
  putStrLn "                                                                         "

getHuskFeatures :: IO [LispVal]
getHuskFeatures = do
    -- TODO: windows posix
    return [ Atom "r7rs"
           , Atom "husk"
           , Atom $ "husk-" ++ (DV.showVersion PHS.version)
           , Atom SysInfo.arch
           , Atom SysInfo.os
           , Atom "full-unicode"
           , Atom "complex"
           , Atom "ratios"
           ]

-- |Get the full path to a data file installed for husk
getDataFileFullPath :: String -> IO String
getDataFileFullPath = PHS.getDataFileName

-- Future use:
-- getDataFileFullPath' :: [LispVal] -> IOThrowsError LispVal
-- getDataFileFullPath' [String s] = do
--     path <- liftIO $ PHS.getDataFileName s
--     return $ String path
-- getDataFileFullPath' [] = throwError $ NumArgs (Just 1) []
-- getDataFileFullPath' args = throwError $ TypeMismatch "string" $ List args

-- |Attempts to find the file both in the current directory and in the husk
--  libraries. If the file is not found in the current directory but exists
--  as a husk library, return the full path to the file in the library.
--  Otherwise just return the given filename.
findFileOrLib :: String -> ErrorT LispError IO String
findFileOrLib filename = do
    fileAsLib <- liftIO $ getDataFileFullPath $ "lib/" ++ filename
    exists <- fileExists [String filename]
    existsLib <- fileExists [String fileAsLib]
    case (exists, existsLib) of
        (Bool False, Bool True) -> return fileAsLib
        _ -> return filename

libraryExists :: [LispVal] -> IOThrowsError LispVal
libraryExists [p@(Pointer _ _)] = do
    p' <- recDerefPtrs p
    libraryExists [p']
libraryExists [(String filename)] = do
    fileAsLib <- liftIO $ getDataFileFullPath $ "lib/" ++ filename
    Bool exists <- fileExists [String filename]
    Bool existsLib <- fileExists [String fileAsLib]
    return $ Bool $ exists || existsLib
libraryExists _ = return $ Bool False

-- |Register optional SRFI extensions
registerExtensions :: Env -> (FilePath -> IO FilePath) -> IO ()
registerExtensions env getDataFileName = do
  _ <- registerSRFI env getDataFileName 1
  _ <- registerSRFI env getDataFileName 2
  return ()

-- |Register the given SRFI
registerSRFI :: Env -> (FilePath -> IO FilePath) -> Integer -> IO ()
registerSRFI env getDataFileName num = do
 filename <- getDataFileName $ "lib/srfi/srfi-" ++ show num ++ ".scm"
 _ <- evalString env $ "(register-extension '(srfi " ++ show num ++ ") \"" ++ 
  (escapeBackslashes filename) ++ "\")"
 return ()

-- TODO: good news is I think this can be completely implemented in husk, no changes necessary to third party code. the bad news is that this guy needs to be called from the runIOThrows* code instead of show which means that code needs to be relocated (maybe to this module, if that is appropriate (not sure it is)...

-- |This is the recommended function to use to display a lisp error, instead
--  of just using show directly.
showLispError :: LispError -> IO String
showLispError (TypeMismatch str p@(Pointer _ e)) = do
  lv' <- evalLisp' e p 
  case lv' of
    Left _ -> showLispError $ TypeMismatch str $ Atom $ show p
    Right val -> showLispError $ TypeMismatch str val
showLispError (BadSpecialForm str p@(Pointer _ e)) = do
  lv' <- evalLisp' e p 
  case lv' of
    Left _ -> showLispError $ BadSpecialForm str $ Atom $ show p
    Right val -> showLispError $ BadSpecialForm str val
showLispError err = return $ show err

-- |Execute an IO action and return result or an error message.
--  This is intended for use by a REPL, where a result is always
--  needed regardless of type.
runIOThrowsREPL :: IOThrowsError String -> IO String
runIOThrowsREPL action = do
    runState <- runErrorT action
    case runState of
        Left err -> showLispError err
        Right val -> return val

-- |Execute an IO action and return error or Nothing if no error was thrown.
runIOThrows :: IOThrowsError String -> IO (Maybe String)
runIOThrows action = do
    runState <- runErrorT action
    case runState of
        Left err -> do
            disp <- showLispError err
            return $ Just disp
        Right _ -> return Nothing

{- |Evaluate a string containing Scheme code

@
env <- primitiveBindings

evalString env "(+ x x x)"
"3"

evalString env "(+ x x x (* 3 9))"
"30"

evalString env "(* 3 9)"
"27"
@
-}
evalString :: Env -> String -> IO String
evalString env expr = do
  runIOThrowsREPL $ liftM show $ liftThrows (readExpr expr) >>= evalLisp env

-- |Evaluate a string and print results to console
evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = evalString env expr >>= putStrLn

-- |Evaluate a lisp data structure and return a value for use by husk
evalLisp :: Env -> LispVal -> IOThrowsError LispVal
evalLisp env lisp = do
  v <- meval env (makeNullContinuation env) lisp
  safeRecDerefPtrs [] v

-- |Evaluate a lisp data structure and return the LispVal or LispError
--  result directly
-- 
-- @
--  result <- evalLisp' env $ List [Atom "/", Number 1, Number 0]
--  case result of
--    Left err -> putStrLn $ "Error: " ++ (show err)
--    Right val -> putStrLn $ show val
-- @
evalLisp' :: Env -> LispVal -> IO (ThrowsError LispVal)
evalLisp' env lisp = runErrorT (evalLisp env lisp)

-- |A wrapper for macroEval and eval
meval, mprepareApply :: Env -> LispVal -> LispVal -> IOThrowsError LispVal
meval env cont lisp = mfunc env cont lisp eval
mprepareApply env cont lisp = mfunc env cont lisp prepareApply
mfunc :: Env -> LispVal -> LispVal -> (Env -> LispVal -> LispVal -> IOThrowsError LispVal) -> IOThrowsError LispVal
mfunc env cont lisp func = do
  Language.Scheme.Macro.macroEval env lisp apply >>= (func env cont) 
{- OBSOLETE:
 old code for updating env's in the continuation chain (see below)
  if False --needToExtendEnv lisp
     then do
       expanded <- macroEval env lisp
       exEnv <- liftIO $ extendEnv env []
       -- Recursively replace env of nextCont with the extended env
       -- This is more expensive than I would like, but I think it should be straightforward enough...
       exCont <- updateContEnv exEnv cont
       func exEnv (trace ("extending Env") exCont) expanded
     else macroEval env lisp >>= (func env cont) 
-}
{- EXPERIMENTAL CODE FOR REPLACING ENV's in the continuation chain
   
   This is a difficult problem to solve and this code will likely just
   end up going away because we are not going with this approach...

updateContEnv :: Env -> LispVal -> IOThrowsError LispVal
updateContEnv env (Continuation _ curC (Just nextC) dwind) = do
    next <- updateContEnv env nextC
    return $ Continuation env curC (Just next) dwind
updateContEnv env (Continuation _ curC Nothing dwind) = do
    return $ Continuation env curC Nothing dwind
updateContEnv _ val = do
    return val
-}

{- |A support function for eval; eval calls into this function instead of 
    returning values directly. continueEval then uses the continuation 
    argument to manage program control flow.
 -}
continueEval :: Env     -- ^ Current environment
             -> LispVal -- ^ Current continuation
             -> LispVal -- ^ Value of previous computation
             -> Maybe [LispVal] -- ^ Extra arguments from previous computation
             -> IOThrowsError LispVal -- ^ Final value of computation

{- Passing a higher-order function as the continuation; just evaluate it. This is
 - done to enable an 'eval' function to be broken up into multiple sub-functions,
 - so that any of the sub-functions can be passed around as a continuation. 
 -
 - Carry extra args from the current continuation into the next, to support (call-with-values)
 -}
continueEval _
            (Continuation 
                cEnv 
                (Just (HaskellBody func funcArgs))
                (Just nCont@(Continuation {}))
                _)
             val 
             xargs = do
    let args = case funcArgs of
                    Nothing -> xargs
                    _ -> funcArgs
    func cEnv nCont val args
{-
 - No higher order function, so:
 -
 - If there is Scheme code to evaluate in the function body, we continue to evaluate it.
 -
 - Otherwise, if all code in the function has been executed, we 'unwind' to an outer
 - continuation (if there is one), or we just return the result. Yes technically with
 - CPS you are supposed to keep calling into functions and never return, but in this case
 - when the computation is complete, you have to return something. 
 -
 - NOTE: We use 'eval' below instead of 'meval' because macros are already expanded when
 -       a function is loaded the first time, so there is no need to test for this again here.
 -}
continueEval _ (Continuation cEnv (Just (SchemeBody cBody)) (Just cCont) dynWind) val extraArgs = do
--    case (trace ("cBody = " ++ show cBody) cBody) of
    case cBody of
        [] -> do
          case cCont of
            Continuation {contClosure = nEnv} -> 
              -- Pass extra args along if last expression of a function, to support (call-with-values)
              continueEval nEnv cCont val extraArgs 
            _ -> return val
        [lv] -> eval cEnv (Continuation cEnv (Just (SchemeBody [])) (Just cCont) dynWind) lv
        (lv : lvs) -> eval cEnv (Continuation cEnv (Just (SchemeBody lvs)) (Just cCont) dynWind) lv

-- No current continuation, but a next cont is available; call into it
continueEval _ (Continuation cEnv Nothing (Just cCont) _) val xargs = continueEval cEnv cCont val xargs

-- There is no continuation code, just return value
continueEval _ (Continuation _ Nothing Nothing _) val _ = return val
continueEval _ _ _ _ = throwError $ Default "Internal error in continueEval"

{- |Core eval function
Evaluate a scheme expression.
NOTE:  This function does not include macro support and should not be called directly. Instead, use 'evalLisp' -}
--
--
-- Implementation Notes:
--
-- Internally, this function is written in continuation passing style (CPS) to allow the Scheme language
-- itself to support first-class continuations. That is, at any point in the evaluation, call/cc may
-- be used to capture the current continuation. Thus this code must call into the next continuation point, eg: 
--
--  eval ... (makeCPS ...)
--
-- Instead of calling eval directly from within the same function, eg:
--
--  eval ...
--  eval ...
--
-- This can make the code harder to follow, however some coding conventions have been established to make the
-- code easier to follow. Whenever a single function has been broken into multiple ones for the purpose of CPS,
-- those additional functions are defined locally using 'where', and each has been given a 'cps' prefix.
--
eval :: Env -> LispVal -> LispVal -> IOThrowsError LispVal
eval env cont val@(Nil _) = continueEval env cont val Nothing
eval env cont val@(String _) = continueEval env cont val Nothing
eval env cont val@(Char _) = continueEval env cont val Nothing
eval env cont val@(Complex _) = continueEval env cont val Nothing
eval env cont val@(Float _) = continueEval env cont val Nothing
eval env cont val@(Rational _) = continueEval env cont val Nothing
eval env cont val@(Number _) = continueEval env cont val Nothing
eval env cont val@(Bool _) = continueEval env cont val Nothing
eval env cont val@(HashTable _) = continueEval env cont val Nothing
eval env cont val@(Vector _) = continueEval env cont val Nothing
eval env cont val@(ByteVector _) = continueEval env cont val Nothing
eval env cont val@(LispEnv _) = continueEval env cont val Nothing
eval env cont val@(Pointer _ _) = continueEval env cont val Nothing
eval env cont (Atom a) = do
  v <- getVar env a
  let val = case v of
-- TODO: this flag may go away on this branch; it may
--       not be practical with Pointer used everywhere now
#ifdef UsePointers
              List _ -> Pointer a env
              DottedList _ _ -> Pointer a env
              String _ -> Pointer a env
              Vector _ -> Pointer a env
              ByteVector _ -> Pointer a env
              HashTable _ -> Pointer a env
#endif
              _ -> v
  continueEval env cont val Nothing

-- Quote an expression by simply passing along the value
eval env cont (List [Atom "quote", val]) = continueEval env cont val Nothing

-- A special form to assist with debugging macros
eval env cont args@(List [Atom "expand" , _body]) = do
 bound <- liftIO $ isRecBound env "expand"
 if bound
  then prepareApply env cont args -- if bound to a variable in this scope; call into it
  else do
      value <- Language.Scheme.Macro.expand env False _body apply 
      continueEval env cont value Nothing
 
-- A rudimentary implementation of let-syntax
eval env cont args@(List (Atom "let-syntax" : List _bindings : _body)) = do
 bound <- liftIO $ isRecBound env "let-syntax"
 if bound
  then prepareApply env cont args -- if bound to a variable in this scope; call into it
  else do 
   bodyEnv <- liftIO $ extendEnv env []
   _ <- Language.Scheme.Macro.loadMacros env bodyEnv Nothing False _bindings
   -- Expand whole body as a single continuous macro, to ensure hygiene
   expanded <- Language.Scheme.Macro.expand bodyEnv False (List _body) apply
   case expanded of
     List e -> continueEval bodyEnv (Continuation bodyEnv (Just $ SchemeBody e) (Just cont) Nothing) (Nil "") Nothing 
     e -> continueEval bodyEnv cont e Nothing

eval env cont args@(List (Atom "letrec-syntax" : List _bindings : _body)) = do
 bound <- liftIO $ isRecBound env "letrec-syntax"
 if bound
  then prepareApply env cont args -- if bound to a variable in this scope; call into it
  else do 
   bodyEnv <- liftIO $ extendEnv env []
   -- A primitive means of implementing letrec, by simply assuming that each macro is defined in
   -- the letrec's environment, instead of the parent env. Not sure if this is 100% correct but it
   -- is good enough to pass the R5RS test case so it will be used as a rudimentary implementation 
   -- for now...
   _ <- Language.Scheme.Macro.loadMacros bodyEnv bodyEnv Nothing False _bindings
   -- Expand whole body as a single continuous macro, to ensure hygiene
   expanded <- Language.Scheme.Macro.expand bodyEnv False (List _body) apply
   case expanded of
     List e -> continueEval bodyEnv (Continuation bodyEnv (Just $ SchemeBody e) (Just cont) Nothing) (Nil "") Nothing
     e -> continueEval bodyEnv cont e Nothing

-- A non-standard way to rebind a macro to another keyword
eval env cont (List [Atom "define-syntax", 
                     Atom newKeyword,
                     Atom keyword]) = do
  bound <- getNamespacedVar' env macroNamespace keyword
  case bound of
    Just m -> do
        _ <- defineNamespacedVar env macroNamespace newKeyword m
        continueEval env cont (Nil "") Nothing
    Nothing -> throwError $ TypeMismatch "macro" $ Atom keyword

eval env cont args@(List [Atom "define-syntax", Atom keyword,
  (List [Atom "er-macro-transformer", 
    (List (Atom "lambda" : List fparams : fbody))])]) = do
 bound <- liftIO $ isRecBound env "define-syntax"
 if bound
  then prepareApply env cont args -- if bound to var in this scope; call it
  else do 
    -- TODO: ensure fparams is 3 atoms
    -- TODO: now just need to figure out initial entry point to the ER func
    --       for now can ignore complications of an ER found during syn-rules transformation
    _ <- validateFuncParams fparams (Just 3)
    f <- makeNormalFunc env fparams fbody 
    _ <- defineNamespacedVar env macroNamespace keyword $ SyntaxExplicitRenaming f
    continueEval env cont (Nil "") Nothing 

eval env cont args@(List [Atom "define-syntax", Atom keyword, 
    (List (Atom "syntax-rules" : Atom ellipsis : (List identifiers : rules)))]) = do
 bound <- liftIO $ isRecBound env "define-syntax"
 if bound
  then prepareApply env cont args -- if bound to a variable in this scope; call into it
  else do 
    _ <- defineNamespacedVar env macroNamespace keyword $ 
            Syntax (Just env) Nothing False ellipsis identifiers rules
    continueEval env cont (Nil "") Nothing

eval env cont args@(List [Atom "define-syntax", Atom keyword, 
    (List (Atom "syntax-rules" : (List identifiers : rules)))]) = do
 bound <- liftIO $ isRecBound env "define-syntax"
 if bound
  then prepareApply env cont args -- if bound to a variable in this scope; call into it
  else do 
  {-
   - FUTURE: Issue #15: there really ought to be some error checking of the syntax rules, 
   -                    since they could be malformed...
   - As it stands now, there is no checking until the code attempts to perform a macro transformation.
   - At a minimum, should check identifiers to make sure each is an atom (see findAtom) 
   -}
    -- 
    -- I think it seems to be a better solution to use this defEnv, but
    -- that causes problems when a var is changed via (define) or (set!) since most
    -- schemes interpret allow this change to propagate back to the point of definition
    -- (or at least, when modules are not in play). See:
    --
    -- http://stackoverflow.com/questions/7999084/scheme-syntax-rules-difference-in-variable-bindings-between-let-anddefine
    --
    -- Anyway, this may come back. But not using it for now...
    --
    --    defEnv <- liftIO $ copyEnv env
    _ <- defineNamespacedVar env macroNamespace keyword $ Syntax (Just env) Nothing False "..." identifiers rules
    continueEval env cont (Nil "") Nothing 

eval env cont args@(List [Atom "if", predic, conseq, alt]) = do
 bound <- liftIO $ isRecBound env "if"
 if bound
  then prepareApply env cont args -- if is bound to a variable in this scope; call into it
  else meval env (makeCPS env cont cps) predic
 where cps :: Env -> LispVal -> LispVal -> Maybe [LispVal] -> IOThrowsError LispVal
       cps e c result _ =
            case result of
              Bool False -> meval e c alt
              _ -> meval e c conseq

eval env cont args@(List [Atom "if", predic, conseq]) = do
 bound <- liftIO $ isRecBound env "if"
 if bound
  then prepareApply env cont args -- if is bound to a variable in this scope; call into it
  else meval env (makeCPS env cont cpsResult) predic
 where cpsResult :: Env -> LispVal -> LispVal -> Maybe [LispVal] -> IOThrowsError LispVal
       cpsResult e c result _ =
            case result of
              Bool False -> continueEval e c (Nil "") Nothing -- Unspecified return value per R5RS
              _ -> meval e c conseq

eval env cont args@(List [Atom "set!", Atom var, form]) = do
 bound <- liftIO $ isRecBound env "set!"
 if bound
  then prepareApply env cont args -- if is bound to a variable in this scope; call into it
  else meval env (makeCPS env cont cpsResult) form
 where cpsResult :: Env -> LispVal -> LispVal -> Maybe [LispVal] -> IOThrowsError LispVal
       cpsResult e c result _ = do
        value <- setVar e var result 
        continueEval e c value Nothing
eval env cont args@(List [Atom "set!", nonvar, _]) = do 
 bound <- liftIO $ isRecBound env "set!"
 if bound
  then prepareApply env cont args -- if is bound to a variable in this scope; call into it
  else throwError $ TypeMismatch "variable" nonvar
eval env cont fargs@(List (Atom "set!" : args)) = do
 bound <- liftIO $ isRecBound env "set!"
 if bound
  then prepareApply env cont fargs -- if is bound to a variable in this scope; call into it
  else throwError $ NumArgs (Just 2) args

eval env cont args@(List [Atom "define", Atom var, form]) = do
 bound <- liftIO $ isRecBound env "define"
 if bound
  then prepareApply env cont args -- if is bound to a variable in this scope; call into it
  else meval env (makeCPS env cont cpsResult) form
 where cpsResult :: Env -> LispVal -> LispVal -> Maybe [LispVal] -> IOThrowsError LispVal
       cpsResult e c result _ = do
        value <- defineVar e var result 
        continueEval e c value Nothing

eval env cont args@(List (Atom "define" : List (Atom var : fparams) : fbody )) = do
 bound <- liftIO $ isRecBound env "define"
 if bound
  then prepareApply env cont args -- if is bound to a variable in this scope; call into it
  else do 
      _ <- validateFuncParams fparams Nothing
      -- Cache macro expansions within function body
      ebody <- mapM (\ lisp -> Language.Scheme.Macro.macroEval env lisp apply) fbody
      result <- (makeNormalFunc env fparams ebody >>= defineVar env var)
      continueEval env cont result Nothing

eval env cont args@(List (Atom "define" : DottedList (Atom var : fparams) varargs : fbody)) = do
 bound <- liftIO $ isRecBound env "define"
 if bound
  then prepareApply env cont args -- if is bound to a variable in this scope; call into it
  else do 
      _ <- validateFuncParams (fparams ++ [varargs]) Nothing
      ebody <- mapM (\ lisp -> Language.Scheme.Macro.macroEval env lisp apply) fbody
      result <- (makeVarargs varargs env fparams ebody >>= defineVar env var)
      continueEval env cont result Nothing

eval env cont args@(List (Atom "lambda" : List fparams : fbody)) = do
 bound <- liftIO $ isRecBound env "lambda"
 if bound
  then prepareApply env cont args -- if is bound to a variable in this scope; call into it
  else do 
      _ <- validateFuncParams fparams Nothing
      ebody <- mapM (\ lisp -> Language.Scheme.Macro.macroEval env lisp apply) fbody
      result <- makeNormalFunc env fparams ebody
      continueEval env cont result Nothing

eval env cont args@(List (Atom "lambda" : DottedList fparams varargs : fbody)) = do
 bound <- liftIO $ isRecBound env "lambda"
 if bound
  then prepareApply env cont args -- if is bound to a variable in this scope; call into it
  else do 
      _ <- validateFuncParams (fparams ++ [varargs]) Nothing
      ebody <- mapM (\ lisp -> Language.Scheme.Macro.macroEval env lisp apply) fbody
      result <- makeVarargs varargs env fparams ebody
      continueEval env cont result Nothing

eval env cont args@(List (Atom "lambda" : varargs@(Atom _) : fbody)) = do
 bound <- liftIO $ isRecBound env "lambda"
 if bound
  then prepareApply env cont args -- if is bound to a variable in this scope; call into it
  else do 
      ebody <- mapM (\ lisp -> Language.Scheme.Macro.macroEval env lisp apply) fbody
      result <- makeVarargs varargs env [] ebody
      continueEval env cont result Nothing

eval env cont args@(List [Atom "string-set!", Atom var, i, character]) = do
 bound <- liftIO $ isRecBound env "string-set!"
 if bound
  then prepareApply env cont args -- if is bound to a variable in this scope; call into it
  else meval env (makeCPS env cont cpsChar) character
 where
        cpsChar :: Env -> LispVal -> LispVal -> Maybe [LispVal] -> IOThrowsError LispVal
        cpsChar e c chr _ = do
            meval e (makeCPSWArgs e c cpsStr [chr]) i

        cpsStr :: Env -> LispVal -> LispVal -> Maybe [LispVal] -> IOThrowsError LispVal
        cpsStr e c idx (Just [chr]) = do
            value <- getVar env var
            derefValue <- derefPtr value
            meval e (makeCPSWArgs e c cpsSubStr [idx, chr]) derefValue
        cpsStr _ _ _ _ = throwError $ InternalError "Unexpected case in cpsStr"

        cpsSubStr :: Env -> LispVal -> LispVal -> Maybe [LispVal] -> IOThrowsError LispVal
        cpsSubStr e c str (Just [idx, chr]) = do
            value <- substr (str, chr, idx) >>= updateObject e var 
            continueEval e c value Nothing
        cpsSubStr _ _ _ _ = throwError $ InternalError "Invalid argument to cpsSubStr"

eval env cont args@(List [Atom "string-set!" , nonvar , _ , _ ]) = do
 bound <- liftIO $ isRecBound env "string-set!"
 if bound
  then prepareApply env cont args -- if is bound to a variable in this scope; call into it
  else throwError $ TypeMismatch "variable" nonvar
eval env cont fargs@(List (Atom "string-set!" : args)) = do 
 bound <- liftIO $ isRecBound env "string-set!"
 if bound
  then prepareApply env cont fargs -- if is bound to a variable in this scope; call into it
  else throwError $ NumArgs (Just 3) args

eval env cont args@(List [Atom "set-car!", Atom var, argObj]) = do
 bound <- liftIO $ isRecBound env "set-car!"
 if bound
  then prepareApply env cont args -- if is bound to a variable in this scope; call into it
  else do
      value <- getVar env var
      continueEval env (makeCPS env cont cpsObj) value Nothing
 where
        cpsObj :: Env -> LispVal -> LispVal -> Maybe [LispVal] -> IOThrowsError LispVal
        cpsObj e c obj@(Pointer _ _) x = do
          o <- derefPtr obj
          cpsObj e c o x
        cpsObj _ _ obj@(List []) _ = throwError $ TypeMismatch "pair" obj
        cpsObj e c obj@(List (_ : _)) _ = meval e (makeCPSWArgs e c cpsSet [obj]) argObj
        cpsObj e c obj@(DottedList _ _) _ =  meval e (makeCPSWArgs e c cpsSet [obj]) argObj
        cpsObj _ _ obj _ = throwError $ TypeMismatch "pair" obj

        cpsSet :: Env -> LispVal -> LispVal -> Maybe [LispVal] -> IOThrowsError LispVal
        cpsSet e c obj (Just [List (_ : ls)]) = do
            value <- updateObject e var (List (obj : ls)) 
            continueEval e c value Nothing
        cpsSet e c obj (Just [DottedList (_ : ls) l]) = do
            value <- updateObject e var (DottedList (obj : ls) l) 
            continueEval e c value Nothing
        cpsSet _ _ _ _ = throwError $ InternalError "Unexpected argument to cpsSet"
eval env cont args@(List [Atom "set-car!" , nonvar , _ ]) = do
 bound <- liftIO $ isRecBound env "set-car!"
 if bound
  then prepareApply env cont args -- if is bound to a variable in this scope; call into it
  else throwError $ TypeMismatch "variable" nonvar
eval env cont fargs@(List (Atom "set-car!" : args)) = do
 bound <- liftIO $ isRecBound env "set-car!"
 if bound
  then prepareApply env cont fargs -- if is bound to a variable in this scope; call into it
  else throwError $ NumArgs (Just 2) args

eval env cont args@(List [Atom "set-cdr!", Atom var, argObj]) = do
 bound <- liftIO $ isRecBound env "set-cdr!"
 if bound
  then prepareApply env cont args -- if is bound to a variable in this scope; call into it
  else do
      value <- getVar env var
      derefValue <- derefPtr value
      continueEval env (makeCPS env cont cpsObj) derefValue Nothing
 where
        cpsObj :: Env -> LispVal -> LispVal -> Maybe [LispVal] -> IOThrowsError LispVal
        cpsObj _ _ pair@(List []) _ = throwError $ TypeMismatch "pair" pair
        cpsObj e c pair@(List (_ : _)) _ = meval e (makeCPSWArgs e c cpsSet [pair]) argObj
        cpsObj e c pair@(DottedList _ _) _ = meval e (makeCPSWArgs e c cpsSet [pair]) argObj
        cpsObj _ _ pair _ = throwError $ TypeMismatch "pair" pair

        updateCdr e c obj l = do
            l' <- recDerefPtrs l
            obj' <- recDerefPtrs obj
            value <- (cons [l', obj']) >>= updateObject e var 
            continueEval e c value Nothing

        cpsSet :: Env -> LispVal -> LispVal -> Maybe [LispVal] -> IOThrowsError LispVal
        cpsSet e c obj (Just [List (l : _)]) = updateCdr e c obj l
        cpsSet e c obj (Just [DottedList (l : _) _]) = updateCdr e c obj l
        cpsSet _ _ _ _ = throwError $ InternalError "Unexpected argument to cpsSet"
eval env cont args@(List [Atom "set-cdr!" , nonvar , _ ]) = do
 bound <- liftIO $ isRecBound env "set-cdr!"
 if bound
  then prepareApply env cont args -- if is bound to a variable in this scope; call into it
  else do
      -- TODO: eval nonvar, then can process it if we get a list
      throwError $ TypeMismatch "variable" nonvar
eval env cont fargs@(List (Atom "set-cdr!" : args)) = do
 bound <- liftIO $ isRecBound env "set-cdr!"
 if bound
  then prepareApply env cont fargs -- if is bound to a variable in this scope; call into it
  else throwError $ NumArgs (Just 2) args

eval env cont args@(List [Atom "list-set!", Atom var, i, object]) = do
 bound <- liftIO $ isRecBound env "list-set!"
 if bound
  then prepareApply env cont args -- if is bound to a variable in this scope; call into it
  else meval env (makeCPS env cont $ createObjSetCPS var object updateList) i

eval env cont args@(List [Atom "list-set!" , nonvar , _ , _]) = do 
 bound <- liftIO $ isRecBound env "list-set!"
 if bound
  then prepareApply env cont args -- if is bound to a variable in this scope; call into it
  else throwError $ TypeMismatch "variable" nonvar
eval env cont fargs@(List (Atom "list-set!" : args)) = do 
 bound <- liftIO $ isRecBound env "list-set!"
 if bound
  then prepareApply env cont fargs -- if is bound to a variable in this scope; call into it
  else throwError $ NumArgs (Just 3) args

eval env cont args@(List [Atom "vector-set!", Atom var, i, object]) = do
 bound <- liftIO $ isRecBound env "vector-set!"
 if bound
  then prepareApply env cont args -- if is bound to a variable in this scope; call into it
  else meval env (makeCPS env cont $ createObjSetCPS var object updateVector) i
eval env cont args@(List [Atom "vector-set!" , nonvar , _ , _]) = do 
 bound <- liftIO $ isRecBound env "vector-set!"
 if bound
  then prepareApply env cont args -- if is bound to a variable in this scope; call into it
  else throwError $ TypeMismatch "variable" nonvar
eval env cont fargs@(List (Atom "vector-set!" : args)) = do 
 bound <- liftIO $ isRecBound env "vector-set!"
 if bound
  then prepareApply env cont fargs -- if is bound to a variable in this scope; call into it
  else throwError $ NumArgs (Just 3) args

eval env cont args@(List [Atom "bytevector-u8-set!", Atom var, i, object]) = do
 bound <- liftIO $ isRecBound env "bytevector-u8-set!"
 if bound
  then prepareApply env cont args -- if is bound to a variable in this scope; call into it
  else meval env (makeCPS env cont $ createObjSetCPS var object updateByteVector) i

eval env cont args@(List [Atom "bytevector-u8-set!" , nonvar , _ , _]) = do 
 bound <- liftIO $ isRecBound env "bytevector-u8-set!"
 if bound
  then prepareApply env cont args -- if is bound to a variable in this scope; call into it
  else throwError $ TypeMismatch "variable" nonvar
eval env cont fargs@(List (Atom "bytevector-u8-set!" : args)) = do 
 bound <- liftIO $ isRecBound env "bytevector-u8-set!"
 if bound
  then prepareApply env cont fargs -- if is bound to a variable in this scope; call into it
  else throwError $ NumArgs (Just 3) args

eval env cont args@(List [Atom "hash-table-set!", Atom var, rkey, rvalue]) = do
 bound <- liftIO $ isRecBound env "hash-table-set!"
 if bound
  then prepareApply env cont args -- if is bound to a variable in this scope; call into it
  else meval env (makeCPS env cont cpsValue) rkey
 where
        cpsValue :: Env -> LispVal -> LispVal -> Maybe [LispVal] -> IOThrowsError LispVal
        cpsValue e c key _ = meval e (makeCPSWArgs e c cpsH [key]) rvalue

        cpsH :: Env -> LispVal -> LispVal -> Maybe [LispVal] -> IOThrowsError LispVal
        cpsH e c value (Just [key]) = do
          v <- getVar e var
          derefVar <- derefPtr v
          meval e (makeCPSWArgs e c cpsEvalH [key, value]) derefVar
        cpsH _ _ _ _ = throwError $ InternalError "Invalid argument to cpsH"

        cpsEvalH :: Env -> LispVal -> LispVal -> Maybe [LispVal] -> IOThrowsError LispVal
        cpsEvalH e c h (Just [key, value]) = do
            case h of
                HashTable ht -> do
                  updateObject env var (HashTable $ Data.Map.insert key value ht) >>= meval e c
                other -> throwError $ TypeMismatch "hash-table" other
        cpsEvalH _ _ _ _ = throwError $ InternalError "Invalid argument to cpsEvalH"
eval env cont args@(List [Atom "hash-table-set!" , nonvar , _ , _]) = do
 bound <- liftIO $ isRecBound env "hash-table-set!"
 if bound
  then prepareApply env cont args -- if is bound to a variable in this scope; call into it
  else throwError $ TypeMismatch "variable" nonvar
eval env cont fargs@(List (Atom "hash-table-set!" : args)) = do
 bound <- liftIO $ isRecBound env "hash-table-set!"
 if bound
  then prepareApply env cont fargs -- if is bound to a variable in this scope; call into it
  else throwError $ NumArgs (Just 3) args

eval env cont args@(List [Atom "hash-table-delete!", Atom var, rkey]) = do
 bound <- liftIO $ isRecBound env "hash-table-delete!"
 if bound
  then prepareApply env cont args -- if is bound to a variable in this scope; call into it
  else meval env (makeCPS env cont cpsH) rkey
 where
        cpsH :: Env -> LispVal -> LispVal -> Maybe [LispVal] -> IOThrowsError LispVal
        cpsH e c key _ = do
            value <- getVar e var
            derefValue <- derefPtr value
            meval e (makeCPSWArgs e c cpsEvalH $ [key]) derefValue

        cpsEvalH :: Env -> LispVal -> LispVal -> Maybe [LispVal] -> IOThrowsError LispVal
        cpsEvalH e c h (Just [key]) = do
            case h of
                HashTable ht -> do
                  updateObject env var (HashTable $ Data.Map.delete key ht) >>= meval e c
                other -> throwError $ TypeMismatch "hash-table" other
        cpsEvalH _ _ _ _ = throwError $ InternalError "Invalid argument to cpsEvalH"
eval env cont args@(List [Atom "hash-table-delete!" , nonvar , _]) = do
 bound <- liftIO $ isRecBound env "hash-table-delete!"
 if bound
  then prepareApply env cont args -- if is bound to a variable in this scope; call into it
  else throwError $ TypeMismatch "variable" nonvar
eval env cont fargs@(List (Atom "hash-table-delete!" : args)) = do
 bound <- liftIO $ isRecBound env "hash-table-delete!"
 if bound
  then prepareApply env cont fargs -- if is bound to a variable in this scope; call into it
  else throwError $ NumArgs (Just 2) args

eval env cont args@(List (_ : _)) = mprepareApply env cont args
eval _ _ badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

-- |A helper function for the special form /(string-set!)/
substr :: (LispVal, LispVal, LispVal) -> IOThrowsError LispVal 
substr (String str, Char char, Number ii) = do
                      return $ String $ (take (fromInteger ii) . drop 0) str ++
                               [char] ++
                               (take (length str) . drop (fromInteger ii + 1)) str
substr (String _, Char _, n) = throwError $ TypeMismatch "number" n
substr (String _, c, _) = throwError $ TypeMismatch "character" c
substr (s, _, _) = throwError $ TypeMismatch "string" s

-- |Replace a list element, by index. Taken from:
--  http://stackoverflow.com/questions/10133361/haskell-replace-element-in-list
replaceAtIndex :: forall a. Int -> a -> [a] -> [a]
replaceAtIndex n item ls = a ++ (item:b) where (a, (_:b)) = splitAt n ls

-- |A helper function for /(list-set!)/
updateList :: LispVal -> LispVal -> LispVal -> IOThrowsError LispVal
updateList (List list) (Number idx) obj = do
    return $ List $ replaceAtIndex (fromInteger idx) obj list
updateList ptr@(Pointer _ _) i obj = do
  list <- derefPtr ptr
  updateList list i obj
updateList l _ _ = throwError $ TypeMismatch "list" l

-- |A helper function for the special form /(vector-set!)/
updateVector :: LispVal -> LispVal -> LispVal -> IOThrowsError LispVal
updateVector (Vector vec) (Number idx) obj = return $ Vector $ vec // [(fromInteger idx, obj)]
updateVector ptr@(Pointer _ _) i obj = do
  vec <- derefPtr ptr
  updateVector vec i obj
updateVector v _ _ = throwError $ TypeMismatch "vector" v

-- |A helper function for the special form /(bytevector-u8-set!)/
updateByteVector :: LispVal -> LispVal -> LispVal -> IOThrowsError LispVal
updateByteVector (ByteVector vec) (Number idx) obj = 
    case obj of
        Number byte -> do
-- TODO: error checking
           let (h, t) = BS.splitAt (fromInteger idx) vec
           return $ ByteVector $ BS.concat [h, BS.pack [fromInteger byte :: Word8], BS.tail t]
        badType -> throwError $ TypeMismatch "byte" badType
updateByteVector ptr@(Pointer _ _) i obj = do
  vec <- derefPtr ptr
  updateByteVector vec i obj
updateByteVector v _ _ = throwError $ TypeMismatch "bytevector" v

-- |Helper function to perform CPS for vector-set! and similar forms
createObjSetCPS :: String
                   -> LispVal
                   -> (LispVal -> LispVal -> LispVal -> ErrorT LispError IO LispVal)
                   -> Env
                   -> LispVal
                   -> LispVal
                   -> Maybe [LispVal]
                   -> IOThrowsError LispVal
createObjSetCPS var object updateFnc = cpsIndex
  where
    -- Update data structure at given index, with given object
    cpsUpdateStruct :: Env -> LispVal -> LispVal -> Maybe [LispVal] -> IOThrowsError LispVal
    cpsUpdateStruct e c struct (Just [idx, obj]) = do
        value <- updateFnc struct idx obj >>= updateObject e var
        continueEval e c value Nothing
    cpsUpdateStruct _ _ _ _ = throwError $ InternalError "Invalid argument to cpsUpdateStruct"

    -- Receive index/object, retrieve variable containing data structure
    cpsGetVar :: Env -> LispVal -> LispVal -> Maybe [LispVal] -> IOThrowsError LispVal
    cpsGetVar e c obj (Just [idx]) = (meval e (makeCPSWArgs e c cpsUpdateStruct [idx, obj]) =<< getVar e var)
    cpsGetVar _ _ _ _ = throwError $ InternalError "Invalid argument to cpsGetVar"

    -- Receive and pass index
    cpsIndex :: Env -> LispVal -> LispVal -> Maybe [LispVal] -> IOThrowsError LispVal
    cpsIndex e c idx _ = meval e (makeCPSWArgs e c cpsGetVar [idx]) object


{- Prepare for apply by evaluating each function argument,
   and then execute the function via 'apply' -}
prepareApply :: Env -> LispVal -> LispVal -> IOThrowsError LispVal
prepareApply env cont (List (function : functionArgs)) = do
  eval env (makeCPSWArgs env cont cpsPrepArgs functionArgs) function
 where cpsPrepArgs :: Env -> LispVal -> LispVal -> Maybe [LispVal] -> IOThrowsError LispVal
       cpsPrepArgs e c func args' = do
-- case (trace ("prep eval of args: " ++ show args) args) of
          let args = case args' of
                          Just as -> as
                          Nothing -> []
          case args of
            [] -> apply c func [] -- No args, immediately apply the function
            [a] -> meval env (makeCPSWArgs e c cpsEvalArgs [func, List [], List []]) a
            (a : as) -> meval env (makeCPSWArgs e c cpsEvalArgs [func, List [], List as]) a
        {- Store value of previous argument, evaluate the next arg until all are done
        parg - Previous argument that has now been evaluated
        state - List containing the following, in order:
        - Function to apply when args are ready
        - List of evaluated parameters
        - List of parameters awaiting evaluation -}
       cpsEvalArgs :: Env -> LispVal -> LispVal -> Maybe [LispVal] -> IOThrowsError LispVal
       cpsEvalArgs e c evaledArg (Just [func, List argsEvaled, List argsRemaining]) =
          case argsRemaining of
            [] -> apply c func (argsEvaled ++ [evaledArg])
            [a] -> meval e (makeCPSWArgs e c cpsEvalArgs [func, List (argsEvaled ++ [evaledArg]), List []]) a
            (a : as) -> meval e (makeCPSWArgs e c cpsEvalArgs [func, List (argsEvaled ++ [evaledArg]), List as]) a

       cpsEvalArgs _ _ _ (Just a) = throwError $ Default $ "Unexpected error in function application (1) " ++ show a
       cpsEvalArgs _ _ _ Nothing = throwError $ Default "Unexpected error in function application (2)"
prepareApply _ _ _ = throwError $ Default "Unexpected error in prepareApply"

-- |Call into a Scheme function
apply :: LispVal  -- ^ Current continuation
      -> LispVal  -- ^ Function or continuation to execute
      -> [LispVal] -- ^ Arguments
      -> IOThrowsError LispVal -- ^ Final value of computation
apply _ cont@(Continuation env _ _ ndynwind) args = do
-- case (trace ("calling into continuation. dynWind = " ++ show ndynwind) ndynwind) of
  case ndynwind of
    -- Call into dynWind.before if it exists...
    Just [DynamicWinders beforeFunc _] -> apply (makeCPS env cont cpsApply) beforeFunc []
    _ -> doApply env cont
 where
   cpsApply :: Env -> LispVal -> LispVal -> Maybe [LispVal] -> IOThrowsError LispVal
   cpsApply e c _ _ = doApply e c
   doApply e c = do
      case (toInteger $ length args) of
        0 -> throwError $ NumArgs (Just 1) []
        1 -> continueEval e c (head args) Nothing
        _ ->  -- Pass along additional arguments, so they are available to (call-with-values)
             continueEval e cont (head args) (Just $ tail args)
apply cont (IOFunc func) args = do
  result <- func args
  case cont of
    Continuation cEnv _ _ _ -> continueEval cEnv cont result Nothing
    _ -> return result
apply cont (CustFunc func) args = do
  List dargs <- recDerefPtrs $ List args -- Deref any pointers
  result <- func dargs
  case cont of
    Continuation cEnv _ _ _ -> continueEval cEnv cont result Nothing
    _ -> return result
apply cont (EvalFunc func) args = do
    -- An EvalFunc extends the evaluator so it needs access to the current 
    -- continuation, so pass it as the first argument.
  func (cont : args)
apply cont (PrimitiveFunc func) args = do
  -- OK not to deref ptrs here because primitives only operate on
  -- non-objects, and the error handler execs in the I/O monad and
  -- handles ptrs just fine
  result <- liftThrows $ func args
  case cont of
    Continuation cEnv _ _ _ -> continueEval cEnv cont result Nothing
    _ -> return result
apply cont (Func aparams avarargs abody aclosure) args =
  if num aparams /= num args && isNothing avarargs
     then throwError $ NumArgs (Just (num aparams)) args
     else liftIO (extendEnv aclosure $ zip (map ((,) varNamespace) aparams) args) >>= bindVarArgs avarargs >>= (evalBody abody)
  where remainingArgs = drop (length aparams) args
        num = toInteger . length
        --
        -- Continue evaluation within the body, preserving the outer continuation.
        --
        {- This link was helpful for implementing this, and has a *lot* of other useful information:
        http://icem-www.folkwang-hochschule.de/~finnendahl/cm_kurse/doc/schintro/schintro_73.html#SEC80 -}
        --
        {- What we are doing now is simply not saving a continuation for tail calls. For now this may
        be good enough, although it may need to be enhanced in the future in order to properly
        detect all tail calls. -}
        --
        -- See: http://icem-www.folkwang-hochschule.de/~finnendahl/cm_kurse/doc/schintro/schintro_142.html#SEC294
        --
        evalBody evBody env = case cont of
            Continuation _ (Just (SchemeBody cBody)) (Just cCont) cDynWind -> if null cBody
                then continueWCont env evBody cCont cDynWind
-- else continueWCont env (evBody) cont (trace ("cDynWind = " ++ show cDynWind) cDynWind) -- Might be a problem, not fully optimizing
                else continueWCont env evBody cont cDynWind -- Might be a problem, not fully optimizing
            Continuation _ _ _ cDynWind -> continueWCont env evBody cont cDynWind
            _ -> continueWCont env evBody cont Nothing

        -- Shortcut for calling continueEval
        continueWCont cwcEnv cwcBody cwcCont cwcDynWind =
            continueEval cwcEnv (Continuation cwcEnv (Just (SchemeBody cwcBody)) (Just cwcCont) cwcDynWind) (Nil "") Nothing 

        bindVarArgs arg env = case arg of
          Just argName -> liftIO $ extendEnv env [((varNamespace, argName), List remainingArgs)]
          Nothing -> return env
apply cont (HFunc aparams avarargs abody aclosure) args =
  if num aparams /= num args && isNothing avarargs
     then throwError $ NumArgs (Just (num aparams)) args
     else liftIO (extendEnv aclosure $ zip (map ((,) varNamespace) aparams) args) >>= bindVarArgs avarargs >>= (evalBody abody)
  where remainingArgs = drop (length aparams) args
        num = toInteger . length
        evalBody evBody env = evBody env cont (Nil "") (Just [])
{- TODO: may need to handle cases from Func, such as dynamic winders
        case cont of
            Continuation _ (Just (SchemeBody cBody)) (Just cCont) _ cDynWind -> if length cBody == 0
                then continueWCont env (evBody) cCont cDynWind
                else continueWCont env (evBody) cont cDynWind -- Might be a problem, not fully optimizing
            Continuation _ _ _ _ cDynWind -> continueWCont env (evBody) cont cDynWind
            _ -> continueWCont env (evBody) cont Nothing

        -- Shortcut for calling continueEval
        continueWCont cwcEnv cwcBody cwcCont cwcDynWind =
            continueEval cwcEnv (Continuation cwcEnv (Just (SchemeBody cwcBody)) (Just cwcCont) Nothing cwcDynWind) $ Nil ""-}

        bindVarArgs arg env = case arg of
          Just argName -> liftIO $ extendEnv env [((varNamespace, argName), List $ remainingArgs)]
          Nothing -> return env
apply _ func args = do
  List [func'] <- recDerefPtrs $ List [func] -- Deref any pointers
  List args' <- recDerefPtrs $ List args
  throwError $ BadSpecialForm "Unable to evaluate form" $ List (func' : args')

-- |Environment containing the primitive forms that are built into the Scheme 
--  language. This function only includes forms that are implemented in Haskell; 
--  derived forms implemented in Scheme (such as let, list, etc) are available
--  in the standard library which must be pulled into the environment using /(load)/
--
--  For the purposes of using husk as an extension language, /r5rsEnv/ will
--  probably be more useful.
primitiveBindings :: IO Env
primitiveBindings = nullEnv >>= 
    flip extendEnv  ( map (domakeFunc IOFunc) ioPrimitives
                   ++ map (domakeFunc EvalFunc) evalFunctions
                   ++ map (domakeFunc PrimitiveFunc) primitives)
  where domakeFunc constructor (var, func) = 
            ((varNamespace, var), constructor func)

--baseBindings :: IO Env
--baseBindings = nullEnv >>= 
--    (flip extendEnv $ map (domakeFunc EvalFunc) evalFunctions)
--  where domakeFunc constructor (var, func) = 
--            ((varNamespace, var), constructor func)

-- |An empty environment with the %import function. This is presently
--  just intended for internal use by the compiler.
nullEnvWithImport :: IO Env
nullEnvWithImport = nullEnv >>= 
  (flip extendEnv [
    ((varNamespace, "%import"), EvalFunc evalfuncImport),
    ((varNamespace, "hash-table-ref"), IOFunc $ wrapHashTbl hashTblRef)])

-- |Load the standard r5rs environment, including libraries
r5rsEnv :: IO Env
r5rsEnv = do
  env <- r5rsEnv'
  -- Bit of a hack to load (import)
  _ <- evalLisp' env $ List [Atom "%bootstrap-import"]

  return env

-- |Load the standard r5rs environment, including libraries,
--  but do not create the (import) binding
r5rsEnv' :: IO Env
r5rsEnv' = do
  env <- primitiveBindings
  stdlib <- PHS.getDataFileName "lib/stdlib.scm"
  srfi55 <- PHS.getDataFileName "lib/srfi/srfi-55.scm" -- (require-extension)
  
  -- Load standard library
  features <- getHuskFeatures
  _ <- evalString env $ "(define *features* '" ++ show (List features) ++ ")"
  _ <- evalString env $ "(load \"" ++ (escapeBackslashes stdlib) ++ "\")" 

  -- Load (require-extension), which can be used to load other SRFI's
  _ <- evalString env $ "(load \"" ++ (escapeBackslashes srfi55) ++ "\")"
  registerExtensions env PHS.getDataFileName

#ifdef UseLibraries
  -- Load module meta-language 
  metalib <- PHS.getDataFileName "lib/modules.scm"
  metaEnv <- nullEnvWithParent env -- Load env as parent of metaenv
  _ <- evalString metaEnv $ "(load \"" ++ (escapeBackslashes metalib) ++ "\")"
  -- Load meta-env so we can find it later
  _ <- evalLisp' env $ List [Atom "define", Atom "*meta-env*", LispEnv metaEnv]
  -- Load base primitives
  _ <- evalLisp' metaEnv $ List [Atom "add-module!", List [Atom "quote", List [Atom "scheme"]], List [Atom "make-module", Bool False, LispEnv env {-baseEnv-}, List [Atom "quote", List []]]]
--  _ <- evalString metaEnv
--         "(add-module! '(scheme r5rs) (make-module #f (interaction-environment) '()))"
  timeEnv <- liftIO $ r7rsTimeEnv
  _ <- evalLisp' metaEnv $ List [Atom "add-module!", List [Atom "quote", List [Atom "scheme", Atom "time", Atom "posix"]], List [Atom "make-module", Bool False, LispEnv timeEnv, List [Atom "quote", List []]]]

  _ <- evalLisp' metaEnv $ List [
    Atom "define", 
    Atom "library-exists?",
    List [Atom "quote", 
          IOFunc libraryExists]]
#endif

  return env

-- |Load the standard r7rs environment, including libraries
--
--  Note that the only difference between this and the r5rs equivalent is that
--  slightly less Scheme code is loaded initially.
r7rsEnv :: IO Env
r7rsEnv = do
  env <- r7rsEnv'
  -- Bit of a hack to load (import)
  _ <- evalLisp' env $ List [Atom "%bootstrap-import"]

  return env
-- |Load the standard r7rs environment
--
r7rsEnv' :: IO Env
r7rsEnv' = do
  -- TODO: longer term, will need r7rs bindings instead of these
  -- basically want to limit the base bindings to the absolute minimum, but
  -- need enough to get the meta language working
  env <- primitiveBindings --baseBindings
--  baseEnv <- primitiveBindings

  -- Load necessary libraries
  -- Unfortunately this adds them in the top-level environment (!!)
  features <- getHuskFeatures
  _ <- evalString env $ "(define *features* '" ++ show (List features) ++ ")"
  cxr <- PHS.getDataFileName "lib/cxr.scm"
  _ <- evalString env {-baseEnv-} $ "(load \"" ++ (escapeBackslashes cxr) ++ "\")" 
  core <- PHS.getDataFileName "lib/core.scm"
  _ <- evalString env {-baseEnv-} $ "(load \"" ++ (escapeBackslashes core) ++ "\")" 

-- TODO: probably will have to load some scheme libraries for modules.scm to work
--  maybe the 'base' libraries from (scheme base) would be good enough?

#ifdef UseLibraries
  -- Load module meta-language 
  metalib <- PHS.getDataFileName "lib/modules.scm"
  metaEnv <- nullEnvWithParent env -- Load env as parent of metaenv
  _ <- evalString metaEnv $ "(load \"" ++ (escapeBackslashes metalib) ++ "\")"
  -- Load meta-env so we can find it later
  _ <- evalLisp' env $ List [Atom "define", Atom "*meta-env*", LispEnv metaEnv]
  -- Load base primitives
  _ <- evalLisp' metaEnv $ List [Atom "add-module!", List [Atom "quote", List [Atom "scheme"]], List [Atom "make-module", Bool False, LispEnv env {-baseEnv-}, List [Atom "quote", List []]]]

  timeEnv <- liftIO $ r7rsTimeEnv
  _ <- evalLisp' metaEnv $ List [Atom "add-module!", List [Atom "quote", List [Atom "scheme", Atom "time", Atom "posix"]], List [Atom "make-module", Bool False, LispEnv timeEnv, List [Atom "quote", List []]]]

  _ <- evalLisp' metaEnv $ List [
    Atom "define", 
    Atom "library-exists?",
    List [Atom "quote", 
          IOFunc libraryExists]]
#endif

  return env

-- | Load haskell bindings used for the r7rs time library
r7rsTimeEnv :: IO Env
r7rsTimeEnv = do
    nullEnv >>= 
     (flip extendEnv 
           [ ((varNamespace, "current-second"), IOFunc currentTimestamp)])

-- Functions that extend the core evaluator, but that can be defined separately.
--
{- These functions have access to the current environment via the
current continuation, which is passed as the first LispVal argument. -}
--
evalfuncExitSuccess, evalfuncExitFail, evalfuncApply, evalfuncDynamicWind, 
  evalfuncEval, evalfuncLoad, evalfuncCallCC, evalfuncCallWValues,
  evalfuncMakeEnv, evalfuncNullEnv, evalfuncUseParentEnv,
  evalfuncInteractionEnv, evalfuncImport :: [LispVal] -> IOThrowsError LispVal

{-
 - A (somewhat) simplified implementation of dynamic-wind
 -
 - The implementation must obey these 4 rules:
 -
 - 1) The dynamic extent is entered when execution of the body of the called procedure begins.
 - 2) The dynamic extent is also entered when execution is not within the dynamic extent and a continuation is invoked that was captured (using call-with-current-continuation) during the dynamic extent.
 - 3) It is exited when the called procedure returns.
 - 4) It is also exited when execution is within the dynamic extent and a continuation is invoked that was captured while not within the dynamic extent.
 -
 - Basically (before) must be called either when thunk is called into, or when a continuation captured
 - during (thunk) is called into.
 - And (after) must be called either when thunk returns *or* a continuation is called into during (thunk).
 - FUTURE:
 -   At this point dynamic-wind works well enough now to pass all tests, although I am not convinced the implementation
 -   is 100% correct since a stack is not directly used to hold the winders. I think there must still be edge
 -   cases that are not handled properly...
 -}
evalfuncDynamicWind [cont@(Continuation env _ _ _), beforeFunc, thunkFunc, afterFunc] = do
  apply (makeCPS env cont cpsThunk) beforeFunc []
 where
   cpsThunk, cpsAfter :: Env -> LispVal -> LispVal -> Maybe [LispVal] -> IOThrowsError LispVal
   cpsThunk e (Continuation ce cc cnc _ {- FUTURE: cwindrz -} ) _ _ = apply (Continuation e (Just (HaskellBody cpsAfter Nothing))
                                            (Just (Continuation ce cc cnc
                                                                Nothing))
                                             (Just [DynamicWinders beforeFunc afterFunc])) -- FUTURE: append if existing winders
                               thunkFunc []
   cpsThunk _ _ _ _ = throwError $ Default "Unexpected error in cpsThunk during (dynamic-wind)"
   cpsAfter _ c value _ = do
    let cpsRetVals :: Env -> LispVal -> LispVal -> Maybe [LispVal] -> IOThrowsError LispVal
        cpsRetVals e cc _ xargs = continueEval e cc value xargs
    apply (makeCPS env c cpsRetVals) afterFunc [] -- FUTURE: remove dynamicWinder from above from the list before calling after
evalfuncDynamicWind (_ : args) = throwError $ NumArgs (Just 3) args -- Skip over continuation argument
evalfuncDynamicWind _ = throwError $ NumArgs (Just 3) []

evalfuncCallWValues [cont@(Continuation env _ _ _), producer, consumer] = do
  apply (makeCPS env cont cpsEval) producer [] -- Call into prod to get values
 where
   cpsEval :: Env -> LispVal -> LispVal -> Maybe [LispVal] -> IOThrowsError LispVal
   cpsEval _ c@(Continuation _ _ _ _) value (Just xargs) = apply c consumer (value : xargs)
   cpsEval _ c value _ = apply c consumer [value]
evalfuncCallWValues (_ : args) = throwError $ NumArgs (Just 2) args -- Skip over continuation argument
evalfuncCallWValues _ = throwError $ NumArgs (Just 2) []

--evalfuncApply [cont@(Continuation _ _ _ _ _), func, List args] = apply cont func args
evalfuncApply (cont@(Continuation {}) : func : args) = do
  let aRev = reverse args

  if null args
     then throwError $ NumArgs (Just 2) args
     else applyArgs $ head aRev
 where 
  applyArgs aRev = do
    case aRev of
      List aLastElems -> do
        apply cont func $ (init args) ++ aLastElems
      Pointer _ _ -> do
        derefPtr aRev >>= applyArgs
      other -> throwError $ TypeMismatch "List" other
evalfuncApply (_ : args) = throwError $ NumArgs (Just 2) args -- Skip over continuation argument
evalfuncApply _ = throwError $ NumArgs (Just 2) []


evalfuncMakeEnv (cont@(Continuation env _ _ _) : _) = do
    e <- liftIO nullEnv
    continueEval env cont (LispEnv e) Nothing
evalfuncMakeEnv _ = throwError $ NumArgs (Just 1) []

evalfuncNullEnv [cont@(Continuation env _ _ _), Number _] = do
    nilEnv <- liftIO primitiveBindings
    continueEval env cont (LispEnv nilEnv) Nothing
evalfuncNullEnv (_ : args) = throwError $ NumArgs (Just 1) args -- Skip over continuation argument
evalfuncNullEnv _ = throwError $ NumArgs (Just 1) []

evalfuncInteractionEnv (cont@(Continuation env _ _ _) : _) = do
    continueEval env cont (LispEnv env) Nothing
evalfuncInteractionEnv _ = throwError $ InternalError ""

evalfuncUseParentEnv ((Continuation env a b c) : _) = do
    let parEnv = fromMaybe env (parentEnv env)
    continueEval parEnv (Continuation parEnv a b c) (LispEnv parEnv) Nothing
evalfuncUseParentEnv _ = throwError $ InternalError ""

evalfuncImport [
    cont@(Continuation env a b c), 
    toEnv,
    LispEnv fromEnv, 
    imports,
    _] = do
    LispEnv toEnv' <- 
        case toEnv of
            LispEnv _ -> return toEnv
            Bool False -> do
                -- A hack to load imports into the main env, which
                -- in modules.scm is the parent env
                case parentEnv env of
                    Just env' -> return $ LispEnv env'
                    Nothing -> throwError $ InternalError "import into empty env"
            _ -> throwError $ InternalError ""
    case imports of
        List [Bool False] -> do -- Export everything
            exportAll toEnv'
        Bool False -> do -- Export everything
            exportAll toEnv'
        p@(Pointer _ _) -> do
            -- TODO: need to do this in a safer way
            List i <- derefPtr p -- Dangerous, but list is only expected obj
            result <- moduleImport toEnv' fromEnv i
            continueEval env cont result Nothing
        List i -> do
            result <- moduleImport toEnv' fromEnv i
            continueEval env cont result Nothing
        _ -> throwError $ InternalError ""
 where 
   exportAll toEnv' = do
     newEnv <- liftIO $ importEnv toEnv' fromEnv
     continueEval
         env 
        (Continuation env a b c) 
        (LispEnv newEnv)
        Nothing

-- This is just for debugging purposes:
evalfuncImport ((Continuation {} ) : cs) = do
    throwError $ TypeMismatch "import fields" $ List cs
evalfuncImport _ = throwError $ InternalError ""

-- |Load import into the main environment
bootstrapImport :: [LispVal] -> ErrorT LispError IO LispVal
bootstrapImport [cont@(Continuation env _ _ _)] = do
    LispEnv me <- getVar env "*meta-env*"
    ri <- getNamespacedVar me macroNamespace "repl-import"
    renv <- defineNamespacedVar env macroNamespace "import" ri
    continueEval env cont renv Nothing
bootstrapImport _ = throwError $ InternalError ""

evalfuncLoad (cont : p@(Pointer _ _) : lvs) = do
    lv <- derefPtr p
    evalfuncLoad (cont : lv : lvs)

evalfuncLoad [(Continuation _ a b c), String filename, LispEnv env] = do
    evalfuncLoad [Continuation env a b c, String filename]

evalfuncLoad [cont@(Continuation env _ _ _), String filename] = do
    filename' <- findFileOrLib filename
    results <- load filename' >>= mapM (meval env (makeNullContinuation env))
    if not (null results)
       then do result <- return . last $ results
               continueEval env cont result Nothing
       else return $ Nil "" -- Empty, unspecified value

evalfuncLoad (_ : args) = throwError $ NumArgs (Just 1) args -- Skip over continuation argument
evalfuncLoad _ = throwError $ NumArgs (Just 1) []

-- |Evaluate an expression.
evalfuncEval [cont@(Continuation env _ _ _), val] = do -- Current env
    v <- derefPtr val -- Must deref ptrs for macro subsystem
    meval env cont v
evalfuncEval [cont@(Continuation {}), val, LispEnv env] = do -- Env parameter
    v <- derefPtr val -- Must deref ptrs for macro subsystem
    meval env cont v
evalfuncEval (_ : args) = throwError $ NumArgs (Just 1) args -- Skip over continuation argument
evalfuncEval _ = throwError $ NumArgs (Just 1) []

evalfuncCallCC [cont@(Continuation {}), func] = do
   case func of
     Continuation {} -> apply cont func [cont] 
     PrimitiveFunc f -> do
         result <- liftThrows $ f [cont]
         case cont of
             Continuation cEnv _ _ _ -> continueEval cEnv cont result Nothing
             _ -> return result
     Func _ (Just _) _ _ -> apply cont func [cont] -- Variable # of args (pair). Just call into cont
     Func aparams _ _ _ ->
       if toInteger (length aparams) == 1
         then apply cont func [cont]
         else throwError $ NumArgs (Just (toInteger $ length aparams)) [cont]
     HFunc _ (Just _) _ _ -> apply cont func [cont] -- Variable # of args (pair). Just call into cont  
     HFunc aparams _ _ _ ->
       if toInteger (length aparams) == 1
         then apply cont func [cont]
         else throwError $ NumArgs (Just (toInteger $ length aparams)) [cont]
     other -> throwError $ TypeMismatch "procedure" other
evalfuncCallCC (_ : args) = throwError $ NumArgs (Just 1) args -- Skip over continuation argument
evalfuncCallCC _ = throwError $ NumArgs (Just 1) []

evalfuncExitFail _ = do
  _ <- liftIO System.Exit.exitFailure
  return $ Nil ""
evalfuncExitSuccess _ = do
  _ <- liftIO System.Exit.exitSuccess
  return $ Nil ""

{- Primitive functions that extend the core evaluator -}
evalFunctions :: [(String, [LispVal] -> IOThrowsError LispVal)]
evalFunctions =  [  ("apply", evalfuncApply)
                  , ("call-with-current-continuation", evalfuncCallCC)
                  , ("call-with-values", evalfuncCallWValues)
                  , ("dynamic-wind", evalfuncDynamicWind)
                  , ("eval", evalfuncEval)
                  , ("load", evalfuncLoad)
                  , ("null-environment", evalfuncNullEnv)
                  , ("current-environment", evalfuncInteractionEnv)
                  , ("interaction-environment", evalfuncInteractionEnv)
                  , ("make-environment", evalfuncMakeEnv)

               -- Non-standard extensions
#ifdef UseFfi
                  , ("load-ffi", Language.Scheme.FFI.evalfuncLoadFFI)
#endif
#ifdef UseLibraries
                  , ("%import", evalfuncImport)
                  , ("%bootstrap-import", bootstrapImport)
#endif
                  , ("%husk-switch-to-parent-environment", evalfuncUseParentEnv)

                  , ("exit-fail", evalfuncExitFail)
                  , ("exit-success", evalfuncExitSuccess)
                ]
