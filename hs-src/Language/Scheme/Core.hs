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
    -- * Core data
    , primitiveBindings
    , r5rsEnv
    , version
    -- * Utility functions
    , getDataFileFullPath
    , registerExtensions
    , showBanner
    , substr
    , updateVector
    , updateByteVector
    ) where
import qualified Paths_husk_scheme as PHS (getDataFileName)
#ifdef UseFfi
import qualified Language.Scheme.FFI
#endif
import Language.Scheme.Libraries
import qualified Language.Scheme.Macro
import Language.Scheme.Numerical
import Language.Scheme.Parser
import Language.Scheme.Primitives
import Language.Scheme.Types
import Language.Scheme.Util
import Language.Scheme.Variables
import Control.Monad.Error
import Data.Array
import qualified Data.ByteString as BS
import qualified Data.Char
import qualified Data.Map
import Data.Word
import qualified System.Exit
import System.IO
-- import Debug.Trace

-- |husk version number
version :: String
version = "3.7"

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
  putStrLn " http://justinethier.github.com/husk-scheme                              "
  putStrLn " (c) 2010-2013 Justin Ethier                                             "
  putStrLn $ " Version " ++ version ++ " "
  putStrLn "                                                                         "

-- |Get the full path to a data file installed for husk
getDataFileFullPath :: String -> IO String
getDataFileFullPath s = PHS.getDataFileName s

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
  runIOThrowsREPL $ liftM show $ (liftThrows $ readExpr expr) >>= evalLisp env

-- |Evaluate a string and print results to console
evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = evalString env expr >>= putStrLn

-- |Evaluate a lisp data structure and return a value for use by husk
evalLisp :: Env -> LispVal -> IOThrowsError LispVal
evalLisp env lisp = do
  v <- meval env (makeNullContinuation env) lisp
  recDerefPtrs v

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
updateContEnv env (Continuation _ curC (Just nextC) xargs dwind) = do
    next <- updateContEnv env nextC
    return $ Continuation env curC (Just next) xargs dwind
updateContEnv env (Continuation _ curC Nothing xargs dwind) = do
    return $ Continuation env curC Nothing xargs dwind
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
             -> IOThrowsError LispVal -- ^ Final value of computation

{- Passing a higher-order function as the continuation; just evaluate it. This is
 - done to enable an 'eval' function to be broken up into multiple sub-functions,
 - so that any of the sub-functions can be passed around as a continuation. 
 -
 - Carry extra args from the current continuation into the next, to support (call-with-values)
 -}
continueEval _
            (Continuation cEnv (Just (HaskellBody func funcArgs))
                               (Just (Continuation cce cnc ccc _ cdynwind))
                                xargs _) -- rather sloppy, should refactor code so this is not necessary
             val = func cEnv (Continuation cce cnc ccc xargs cdynwind) val funcArgs
{-
 - No higher order function, so:
 -
 - If there is Scheme code to evaluate in the function body, we continue to evaluate it.
 -
 - Otherwise, if all code in the function has been executed, we 'unwind' to an outer
 - continuation (if there is one), or we just return the result. Yes technically with
 - CPS you are supposed to keep calling into functions and never return, but in this case
 - when the computation is complete, you have to return something. 
 -}
continueEval _ (Continuation cEnv (Just (SchemeBody cBody)) (Just cCont) extraArgs dynWind) val = do
--    case (trace ("cBody = " ++ show cBody) cBody) of
    case cBody of
        [] -> do
          case cCont of
            Continuation nEnv ncCont nnCont _ nDynWind ->
              -- Pass extra args along if last expression of a function, to support (call-with-values)
              continueEval nEnv (Continuation nEnv ncCont nnCont extraArgs nDynWind) val
            _ -> return (val)
        [lv] -> meval cEnv (Continuation cEnv (Just (SchemeBody [])) (Just cCont) Nothing dynWind) lv
        (lv : lvs) -> meval cEnv (Continuation cEnv (Just (SchemeBody lvs)) (Just cCont) Nothing dynWind) lv

-- No current continuation, but a next cont is available; call into it
continueEval _ (Continuation cEnv Nothing (Just cCont) _ _) val = continueEval cEnv cCont val

-- There is no continuation code, just return value
continueEval _ (Continuation _ Nothing Nothing _ _) val = return val
continueEval _ _ _ = throwError $ Default "Internal error in continueEval"

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
eval env cont val@(Nil _) = continueEval env cont val
eval env cont val@(String _) = continueEval env cont val
eval env cont val@(Char _) = continueEval env cont val
eval env cont val@(Complex _) = continueEval env cont val
eval env cont val@(Float _) = continueEval env cont val
eval env cont val@(Rational _) = continueEval env cont val
eval env cont val@(Number _) = continueEval env cont val
eval env cont val@(Bool _) = continueEval env cont val
eval env cont val@(HashTable _) = continueEval env cont val
eval env cont val@(Vector _) = continueEval env cont val
eval env cont val@(ByteVector _) = continueEval env cont val
eval env cont val@(LispEnv _) = continueEval env cont val
eval env cont val@(Pointer _ _) = continueEval env cont val
eval env cont (Atom a) = do
  v <- getVar env a
  val <- return $ case v of
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
  continueEval env cont val

-- Quote an expression by simply passing along the value
eval env cont (List [Atom "quote", val]) = continueEval env cont val

-- A special form to assist with debugging macros
eval env cont args@(List [Atom "expand" , _body]) = do
 bound <- liftIO $ isRecBound env "expand"
 if bound
  then prepareApply env cont args -- if bound to a variable in this scope; call into it
  else Language.Scheme.Macro.expand env False _body apply >>= continueEval env cont
 
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
     List e -> continueEval bodyEnv (Continuation bodyEnv (Just $ SchemeBody e) (Just cont) Nothing Nothing) $ Nil "" 
     e -> continueEval bodyEnv cont e

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
     List e -> continueEval bodyEnv (Continuation bodyEnv (Just $ SchemeBody e) (Just cont) Nothing Nothing) $ Nil "" 
     e -> continueEval bodyEnv cont e

-- A non-standard way to rebind a macro to another keyword
eval env cont args@(List [Atom "define-syntax", 
                          Atom newKeyword,
                          Atom keyword]) = do
  bound <- liftIO $ isNamespacedRecBound env macroNamespace keyword
  if bound
     then do
       m <- getNamespacedVar env macroNamespace keyword
       defineNamespacedVar env macroNamespace newKeyword m
     else throwError $ TypeMismatch "macro" $ Atom keyword

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
    f <- makeNormalFunc env fparams fbody 
    _ <- defineNamespacedVar env macroNamespace keyword $ SyntaxExplicitRenaming f
    continueEval env cont $ Nil "" 

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
    _ <- defineNamespacedVar env macroNamespace keyword $ Syntax (Just env) Nothing False identifiers rules
    continueEval env cont $ Nil "" 

eval env cont args@(List [Atom "if", predic, conseq, alt]) = do
 bound <- liftIO $ isRecBound env "if"
 if bound
  then prepareApply env cont args -- if is bound to a variable in this scope; call into it
  else meval env (makeCPS env cont cps) predic
 where cps :: Env -> LispVal -> LispVal -> Maybe [LispVal] -> IOThrowsError LispVal
       cps e c result _ =
            case (result) of
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
              Bool False -> continueEval e c $ Nil "" -- Unspecified return value per R5RS
              _ -> meval e c conseq

eval env cont args@(List [Atom "set!", Atom var, form]) = do
 bound <- liftIO $ isRecBound env "set!"
 if bound
  then prepareApply env cont args -- if is bound to a variable in this scope; call into it
  else meval env (makeCPS env cont cpsResult) form
 where cpsResult :: Env -> LispVal -> LispVal -> Maybe [LispVal] -> IOThrowsError LispVal
       cpsResult e c result _ = setVar e var result >>= continueEval e c
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
       cpsResult e c result _ = defineVar e var result >>= continueEval e c

eval env cont args@(List (Atom "define" : List (Atom var : fparams) : fbody )) = do
 bound <- liftIO $ isRecBound env "define"
 if bound
  then prepareApply env cont args -- if is bound to a variable in this scope; call into it
  else do 
      -- Experimenting with macro expansion of body of function
      ebody <- mapM (\ lisp -> Language.Scheme.Macro.macroEval env lisp apply) fbody
      result <- (makeNormalFunc env fparams ebody >>= defineVar env var)
      continueEval env cont result

eval env cont args@(List (Atom "define" : DottedList (Atom var : fparams) varargs : fbody)) = do
 bound <- liftIO $ isRecBound env "define"
 if bound
  then prepareApply env cont args -- if is bound to a variable in this scope; call into it
  else do 
      ebody <- mapM (\ lisp -> Language.Scheme.Macro.macroEval env lisp apply) fbody
      result <- (makeVarargs varargs env fparams ebody >>= defineVar env var)
      continueEval env cont result

eval env cont args@(List (Atom "lambda" : List fparams : fbody)) = do
 bound <- liftIO $ isRecBound env "lambda"
 if bound
  then prepareApply env cont args -- if is bound to a variable in this scope; call into it
  else do 
      ebody <- mapM (\ lisp -> Language.Scheme.Macro.macroEval env lisp apply) fbody
      result <- makeNormalFunc env fparams ebody
      continueEval env cont result

eval env cont args@(List (Atom "lambda" : DottedList fparams varargs : fbody)) = do
 bound <- liftIO $ isRecBound env "lambda"
 if bound
  then prepareApply env cont args -- if is bound to a variable in this scope; call into it
  else do 
      ebody <- mapM (\ lisp -> Language.Scheme.Macro.macroEval env lisp apply) fbody
      result <- makeVarargs varargs env fparams ebody
      continueEval env cont result

eval env cont args@(List (Atom "lambda" : varargs@(Atom _) : fbody)) = do
 bound <- liftIO $ isRecBound env "lambda"
 if bound
  then prepareApply env cont args -- if is bound to a variable in this scope; call into it
  else do 
      ebody <- mapM (\ lisp -> Language.Scheme.Macro.macroEval env lisp apply) fbody
      result <- makeVarargs varargs env [] ebody
      continueEval env cont result

eval env cont args@(List [Atom "string-set!", Atom var, i, character]) = do
 bound <- liftIO $ isRecBound env "string-set!"
 if bound
  then prepareApply env cont args -- if is bound to a variable in this scope; call into it
  else meval env (makeCPS env cont cpsStr) i
 where
        cpsStr :: Env -> LispVal -> LispVal -> Maybe [LispVal] -> IOThrowsError LispVal
        cpsStr e c idx _ = do
            value <- getVar env var
            derefValue <- derefPtr value
            meval e (makeCPSWArgs e c cpsSubStr $ [idx]) derefValue

        cpsSubStr :: Env -> LispVal -> LispVal -> Maybe [LispVal] -> IOThrowsError LispVal
        cpsSubStr e c str (Just [idx]) =
            substr (str, character, idx) >>= updateObject e var >>= continueEval e c
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
      continueEval env (makeCPS env cont cpsObj) value
 where
        cpsObj :: Env -> LispVal -> LispVal -> Maybe [LispVal] -> IOThrowsError LispVal
        cpsObj e c obj@(Pointer _ _) x = do
          o <- derefPtr obj
          cpsObj e c o x
        cpsObj _ _ obj@(List []) _ = throwError $ TypeMismatch "pair" obj
        cpsObj e c obj@(List (_ : _)) _ = meval e (makeCPSWArgs e c cpsSet $ [obj]) argObj
        cpsObj e c obj@(DottedList _ _) _ =  meval e (makeCPSWArgs e c cpsSet $ [obj]) argObj
        cpsObj _ _ obj _ = throwError $ TypeMismatch "pair" obj

        cpsSet :: Env -> LispVal -> LispVal -> Maybe [LispVal] -> IOThrowsError LispVal
        cpsSet e c obj (Just [List (_ : ls)]) = updateObject e var (List (obj : ls)) >>= continueEval e c -- Wrong constructor? Should it be DottedList?
        cpsSet e c obj (Just [DottedList (_ : ls) l]) = updateObject e var (DottedList (obj : ls) l) >>= continueEval e c
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
      continueEval env (makeCPS env cont cpsObj) derefValue
 where
        cpsObj :: Env -> LispVal -> LispVal -> Maybe [LispVal] -> IOThrowsError LispVal
        cpsObj _ _ pair@(List []) _ = throwError $ TypeMismatch "pair" pair
        cpsObj e c pair@(List (_ : _)) _ = meval e (makeCPSWArgs e c cpsSet $ [pair]) argObj
        cpsObj e c pair@(DottedList _ _) _ = meval e (makeCPSWArgs e c cpsSet $ [pair]) argObj
        cpsObj _ _ pair _ = throwError $ TypeMismatch "pair" pair

        cpsSet :: Env -> LispVal -> LispVal -> Maybe [LispVal] -> IOThrowsError LispVal
        cpsSet e c obj (Just [List (l : _)]) = do
            l' <- recDerefPtrs l
            obj' <- recDerefPtrs obj
            (cons [l', obj']) >>= updateObject e var >>= continueEval e c
        cpsSet e c obj (Just [DottedList (l : _) _]) = do
            l' <- recDerefPtrs l
            obj' <- recDerefPtrs obj
            (cons [l', obj']) >>= updateObject e var >>= continueEval e c
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

eval env cont args@(List [Atom "vector-set!", Atom var, i, object]) = do
 bound <- liftIO $ isRecBound env "vector-set!"
 if bound
  then prepareApply env cont args -- if is bound to a variable in this scope; call into it
  else meval env (makeCPS env cont cpsObj) i
 where
        cpsObj :: Env -> LispVal -> LispVal -> Maybe [LispVal] -> IOThrowsError LispVal
        cpsObj e c idx _ = meval e (makeCPSWArgs e c cpsVec $ [idx]) object

        cpsVec :: Env -> LispVal -> LispVal -> Maybe [LispVal] -> IOThrowsError LispVal
        cpsVec e c obj (Just [idx]) = (meval e (makeCPSWArgs e c cpsUpdateVec $ [idx, obj]) =<< getVar e var)
        cpsVec _ _ _ _ = throwError $ InternalError "Invalid argument to cpsVec"

        cpsUpdateVec :: Env -> LispVal -> LispVal -> Maybe [LispVal] -> IOThrowsError LispVal
        cpsUpdateVec e c vec (Just [idx, obj]) =
            updateVector vec idx obj >>= updateObject e var >>= continueEval e c
        cpsUpdateVec _ _ _ _ = throwError $ InternalError "Invalid argument to cpsUpdateVec"

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
  else meval env (makeCPS env cont cpsObj) i
 where
        cpsObj :: Env -> LispVal -> LispVal -> Maybe [LispVal] -> IOThrowsError LispVal
        cpsObj e c idx _ = meval e (makeCPSWArgs e c cpsVec $ [idx]) object

        cpsVec :: Env -> LispVal -> LispVal -> Maybe [LispVal] -> IOThrowsError LispVal
        cpsVec e c obj (Just [idx]) = (meval e (makeCPSWArgs e c cpsUpdateVec $ [idx, obj]) =<< getVar e var)
        cpsVec _ _ _ _ = throwError $ InternalError "Invalid argument to cpsVec"

        cpsUpdateVec :: Env -> LispVal -> LispVal -> Maybe [LispVal] -> IOThrowsError LispVal
        cpsUpdateVec e c vec (Just [idx, obj]) =
            updateByteVector vec idx obj >>= updateObject e var >>= continueEval e c
        cpsUpdateVec _ _ _ _ = throwError $ InternalError "Invalid argument to cpsUpdateVec"

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
        cpsValue e c key _ = meval e (makeCPSWArgs e c cpsH $ [key]) rvalue

        cpsH :: Env -> LispVal -> LispVal -> Maybe [LispVal] -> IOThrowsError LispVal
        cpsH e c value (Just [key]) = do
          v <- getVar e var
          derefVar <- derefPtr v
          meval e (makeCPSWArgs e c cpsEvalH $ [key, value]) derefVar
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
           return $ ByteVector $ BS.concat [h, BS.pack $ [fromInteger byte :: Word8], BS.tail t]
        badType -> throwError $ TypeMismatch "byte" badType
updateByteVector ptr@(Pointer _ _) i obj = do
  vec <- derefPtr ptr
  updateByteVector vec i obj
updateByteVector v _ _ = throwError $ TypeMismatch "bytevector" v

{- Prepare for apply by evaluating each function argument,
   and then execute the function via 'apply' -}
prepareApply :: Env -> LispVal -> LispVal -> IOThrowsError LispVal
prepareApply env cont (List (function : functionArgs)) = do
  eval env (makeCPSWArgs env cont cpsPrepArgs $ functionArgs) function
 where cpsPrepArgs :: Env -> LispVal -> LispVal -> Maybe [LispVal] -> IOThrowsError LispVal
       cpsPrepArgs e c func (Just args) =
-- case (trace ("prep eval of args: " ++ show args) args) of
          case (args) of
            [] -> apply c func [] -- No args, immediately apply the function
            [a] -> meval env (makeCPSWArgs e c cpsEvalArgs $ [func, List [], List []]) a
            (a : as) -> meval env (makeCPSWArgs e c cpsEvalArgs $ [func, List [], List as]) a
       cpsPrepArgs _ _ _ Nothing = throwError $ Default "Unexpected error in function application (1)"
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
            [a] -> meval e (makeCPSWArgs e c cpsEvalArgs $ [func, List (argsEvaled ++ [evaledArg]), List []]) a
            (a : as) -> meval e (makeCPSWArgs e c cpsEvalArgs $ [func, List (argsEvaled ++ [evaledArg]), List as]) a

       cpsEvalArgs _ _ _ (Just _) = throwError $ Default "Unexpected error in function application (1)"
       cpsEvalArgs _ _ _ Nothing = throwError $ Default "Unexpected error in function application (2)"
prepareApply _ _ _ = throwError $ Default "Unexpected error in prepareApply"

-- |Call into a Scheme function
apply :: LispVal  -- ^ Current continuation
      -> LispVal  -- ^ Function or continuation to execute
      -> [LispVal] -- ^ Arguments
      -> IOThrowsError LispVal -- ^ Final value of computation
apply _ cont@(Continuation env ccont ncont _ ndynwind) args = do
-- case (trace ("calling into continuation. dynWind = " ++ show ndynwind) ndynwind) of
  case ndynwind of
    -- Call into dynWind.before if it exists...
    Just ([DynamicWinders beforeFunc _]) -> apply (makeCPS env cont cpsApply) beforeFunc []
    _ -> doApply env cont
 where
   cpsApply :: Env -> LispVal -> LispVal -> Maybe [LispVal] -> IOThrowsError LispVal
   cpsApply e c _ _ = doApply e c
   doApply e c = do
      case (toInteger $ length args) of
        0 -> throwError $ NumArgs (Just 1) []
        1 -> continueEval e c $ head args
        _ ->  -- Pass along additional arguments, so they are available to (call-with-values)
             continueEval e (Continuation env ccont ncont (Just $ tail args) ndynwind) $ head args
apply cont (IOFunc func) args = do
  result <- func args
  case cont of
    Continuation cEnv _ _ _ _ -> continueEval cEnv cont result
    _ -> return result
apply cont (CustFunc func) args = do
  List dargs <- recDerefPtrs $ List args -- Deref any pointers
  result <- func dargs
  case cont of
    Continuation cEnv _ _ _ _ -> continueEval cEnv cont result
    _ -> return result
apply cont (EvalFunc func) args = do
    -- An EvalFunc extends the evaluator so it needs access to the current 
    -- continuation, so pass it as the first argument.
  func (cont : args)
apply cont (PrimitiveFunc func) args = do
-- TODO: 
--  how to report errors that could contain ptr args (perhaps a new error type?)
--  - any other complications?
  --List dargs <- recDerefPtrs $ List args -- Deref any pointers
  result <- liftThrows $ func args
  case cont of
    Continuation cEnv _ _ _ _ -> continueEval cEnv cont result
    _ -> return result
apply cont (Func aparams avarargs abody aclosure) args =
  if num aparams /= num args && avarargs == Nothing
     then throwError $ NumArgs (Just (num aparams)) args
     else (liftIO $ extendEnv aclosure $ zip (map ((,) varNamespace) aparams) args) >>= bindVarArgs avarargs >>= (evalBody abody)
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
            Continuation _ (Just (SchemeBody cBody)) (Just cCont) _ cDynWind -> if length cBody == 0
                then continueWCont env (evBody) cCont cDynWind
-- else continueWCont env (evBody) cont (trace ("cDynWind = " ++ show cDynWind) cDynWind) -- Might be a problem, not fully optimizing
                else continueWCont env (evBody) cont cDynWind -- Might be a problem, not fully optimizing
            Continuation _ _ _ _ cDynWind -> continueWCont env (evBody) cont cDynWind
            _ -> continueWCont env (evBody) cont Nothing

        -- Shortcut for calling continueEval
        continueWCont cwcEnv cwcBody cwcCont cwcDynWind =
            continueEval cwcEnv (Continuation cwcEnv (Just (SchemeBody cwcBody)) (Just cwcCont) Nothing cwcDynWind) $ Nil ""

        bindVarArgs arg env = case arg of
          Just argName -> liftIO $ extendEnv env [((varNamespace, argName), List $ remainingArgs)]
          Nothing -> return env
apply cont (HFunc aparams avarargs abody aclosure) args =
  if num aparams /= num args && avarargs == Nothing
     then throwError $ NumArgs (Just (num aparams)) args
     else (liftIO $ extendEnv aclosure $ zip (map ((,) varNamespace) aparams) args) >>= bindVarArgs avarargs >>= (evalBody abody)
  where remainingArgs = drop (length aparams) args
        num = toInteger . length
        evalBody evBody env = evBody env cont (Nil "") Nothing 
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

{- |Environment containing the primitive forms that are built into the Scheme language. Note that this only includes
forms that are implemented in Haskell; derived forms implemented in Scheme (such as let, list, etc) are available
in the standard library which must be pulled into the environment using /(load)/. -}
primitiveBindings :: IO Env
primitiveBindings = nullEnv >>= (flip extendEnv $ map (domakeFunc IOFunc) ioPrimitives
                                               ++ map (domakeFunc EvalFunc) evalFunctions
                                               ++ map (domakeFunc PrimitiveFunc) primitives)
  where domakeFunc constructor (var, func) = ((varNamespace, var), constructor func)

-- |Load the standard r5rs environment, including libraries
r5rsEnv :: IO Env
r5rsEnv = do
  env <- primitiveBindings
  stdlib <- PHS.getDataFileName "lib/stdlib.scm"
  srfi55 <- PHS.getDataFileName "lib/srfi/srfi-55.scm" -- (require-extension)
  
  -- Load standard library
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
  -- Bit of a hack to load (import)
  _ <- evalLisp' env $ List [Atom "%bootstrap-import"]
  -- Load (r5rs base)
  _ <- evalString metaEnv
         "(add-module! '(r5rs) (make-module #f (interaction-environment) '()))"
#endif

  return env

-- Functions that extend the core evaluator, but that can be defined separately.
--
{- These functions have access to the current environment via the
current continuation, which is passed as the first LispVal argument. -}
--
evalfuncExitSuccess, evalfuncExitFail, evalfuncApply, evalfuncDynamicWind, 
  evalfuncEval, evalfuncLoad, evalfuncCallCC, evalfuncCallWValues,
  evalfuncMakeEnv, evalfuncNullEnv, evalfuncInteractionEnv, evalfuncImport :: [LispVal] -> IOThrowsError LispVal

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
evalfuncDynamicWind [cont@(Continuation env _ _ _ _), beforeFunc, thunkFunc, afterFunc] = do
  apply (makeCPS env cont cpsThunk) beforeFunc []
 where
   cpsThunk, cpsAfter :: Env -> LispVal -> LispVal -> Maybe [LispVal] -> IOThrowsError LispVal
   cpsThunk e (Continuation ce cc cnc ca _ {- FUTURE: cwindrz -} ) _ _ = apply (Continuation e (Just (HaskellBody cpsAfter Nothing))
                                            (Just (Continuation ce cc cnc ca
                                                                Nothing))
                                             Nothing
                                             (Just ([DynamicWinders beforeFunc afterFunc]))) -- FUTURE: append if existing winders
                               thunkFunc []
   cpsThunk _ _ _ _ = throwError $ Default "Unexpected error in cpsThunk during (dynamic-wind)"
   cpsAfter _ c _ _ = apply c afterFunc [] -- FUTURE: remove dynamicWinder from above from the list before calling after
evalfuncDynamicWind (_ : args) = throwError $ NumArgs (Just 3) args -- Skip over continuation argument
evalfuncDynamicWind _ = throwError $ NumArgs (Just 3) []

evalfuncCallWValues [cont@(Continuation env _ _ _ _), producer, consumer] = do
  apply (makeCPS env cont cpsEval) producer [] -- Call into prod to get values
 where
   cpsEval :: Env -> LispVal -> LispVal -> Maybe [LispVal] -> IOThrowsError LispVal
   cpsEval _ c@(Continuation _ _ _ (Just xargs) _) value _ = apply c consumer (value : xargs)
   cpsEval _ c value _ = apply c consumer [value]
evalfuncCallWValues (_ : args) = throwError $ NumArgs (Just 2) args -- Skip over continuation argument
evalfuncCallWValues _ = throwError $ NumArgs (Just 2) []

--evalfuncApply [cont@(Continuation _ _ _ _ _), func, List args] = apply cont func args
evalfuncApply (cont@(Continuation _ _ _ _ _) : func : args) = do
  let aRev = reverse args

  if null args
     then throwError $ NumArgs (Just 2) args
     else applyArgs $ head aRev
 where 
  applyArgs aRev = do
    case aRev of
      List aLastElems -> do
        apply cont func $ (init args) ++ aLastElems
      Pointer pVar pEnv -> do
        derefPtr aRev >>= applyArgs
      other -> throwError $ TypeMismatch "List" other
evalfuncApply (_ : args) = throwError $ NumArgs (Just 2) args -- Skip over continuation argument
evalfuncApply _ = throwError $ NumArgs (Just 2) []


evalfuncMakeEnv (cont@(Continuation env _ _ _ _) : _) = do
    e <- liftIO $ nullEnv
    continueEval env cont $ LispEnv e

evalfuncNullEnv [cont@(Continuation env _ _ _ _), Number version] = do
    nullEnv <- liftIO $ primitiveBindings
    continueEval env cont $ LispEnv nullEnv
evalfuncNullEnv (_ : args) = throwError $ NumArgs (Just 1) args -- Skip over continuation argument
evalfuncNullEnv _ = throwError $ NumArgs (Just 1) []

evalfuncInteractionEnv (cont@(Continuation env _ _ _ _) : _) = do
    continueEval env cont $ LispEnv env

evalfuncImport [
    cont@(Continuation env a b c d), 
    toEnv,
    LispEnv fromEnv, 
    imports,
    _] = do
    LispEnv toEnv' <- 
        case toEnv of
            LispEnv e -> return toEnv
            Bool False -> do
                -- A hack to load imports into the main env, which
                -- in modules.scm is the grandparent env
                case parentEnv env of
                    Just (Environment (Just gp) _ _) -> 
                        return $ LispEnv gp
                    Just (Environment Nothing _ _ ) -> throwError $ InternalError "import into empty parent env"
                    Nothing -> throwError $ InternalError "import into empty env"
    case imports of
        p@(Pointer _ _) -> do
            -- TODO: need to do this in a safer way
            List i <- derefPtr p -- Dangerous, but list is only expected obj
            result <- moduleImport toEnv' fromEnv i
            continueEval env cont result
        List i -> do
            result <- moduleImport toEnv' fromEnv i
            continueEval env cont result
        Bool False -> do -- Export everything
            newEnv <- liftIO $ importEnv toEnv' fromEnv
            continueEval
                env 
               (Continuation env a b c d) 
               (LispEnv newEnv)

-- This is just for debugging purposes:
evalfuncImport (cont@(Continuation env _ _ _ _ ) : cs) = do
    continueEval env cont $ Nil ""

-- |Load import into the main environment
bootstrapImport [cont@(Continuation env _ _ _ _)] = do
    LispEnv me <- getVar env "*meta-env*"
    ri <- getNamespacedVar me macroNamespace "repl-import"
    renv <- defineNamespacedVar env macroNamespace "import" ri
    continueEval env cont renv


evalfuncLoad (cont : p@(Pointer _ _) : lvs) = do
    lv <- derefPtr p
    evalfuncLoad (cont : lv : lvs)

evalfuncLoad [cont@(Continuation _ a b c d), String filename, LispEnv env] = do
    evalfuncLoad [Continuation env a b c d, String filename]

evalfuncLoad [cont@(Continuation env _ _ _ _), String filename] = do
{-
-- It would be nice to use CPS style below.
--
-- This code mostly works, but causes looping problems in t-cont. need to test to see if
-- those are an artifact of this change or a code problem in that test suite:
  code <- load filename
  if not (null code)
     then continueEval env (Continuation env (Just $ SchemeBody code) (Just cont) Nothing Nothing) $ Nil "" 
     else return $ Nil "" -- Empty, unspecified value
-}
    results <- load filename >>= mapM (evaluate env (makeNullContinuation env))
    if not (null results)
       then do result <- return . last $ results
               continueEval env cont result
       else return $ Nil "" -- Empty, unspecified value
  where evaluate env2 cont2 val2 = meval env2 cont2 val2

evalfuncLoad (_ : args) = throwError $ NumArgs (Just 1) args -- Skip over continuation argument
evalfuncLoad _ = throwError $ NumArgs (Just 1) []

-- Evaluate an expression in the current environment
--
-- Assumption is any macro transform is already performed
-- prior to this step.
--
-- FUTURE: consider allowing env to be specified, per R5RS
--
evalfuncEval [cont@(Continuation env _ _ _ _), val] = do
    v <- derefPtr val -- Must deref ptrs for macro subsystem
    meval env cont v
evalfuncEval [cont@(Continuation _ _ _ _ _), val, LispEnv env] = do
    v <- derefPtr val -- Must deref ptrs for macro subsystem
    meval env cont v
evalfuncEval (_ : args) = throwError $ NumArgs (Just 1) args -- Skip over continuation argument
evalfuncEval _ = throwError $ NumArgs (Just 1) []

evalfuncCallCC [cont@(Continuation _ _ _ _ _), func] = do
   case func of
     Continuation _ _ _ _ _ -> apply cont func [cont] 
     PrimitiveFunc f -> do
         result <- liftThrows $ f [cont]
         case cont of
             Continuation cEnv _ _ _ _ -> continueEval cEnv cont result
             _ -> return result
     Func _ (Just _) _ _ -> apply cont func [cont] -- Variable # of args (pair). Just call into cont
     Func aparams _ _ _ ->
       if (toInteger $ length aparams) == 1
         then apply cont func [cont]
         else throwError $ NumArgs (Just (toInteger $ length aparams)) [cont]
     HFunc _ (Just _) _ _ -> apply cont func [cont] -- Variable # of args (pair). Just call into cont  
     HFunc aparams _ _ _ ->
       if (toInteger $ length aparams) == 1
         then apply cont func [cont]
         else throwError $ NumArgs (Just (toInteger $ length aparams)) [cont]
     other -> throwError $ TypeMismatch "procedure" other
evalfuncCallCC (_ : args) = throwError $ NumArgs (Just 1) args -- Skip over continuation argument
evalfuncCallCC _ = throwError $ NumArgs (Just 1) []

evalfuncExitFail _ = do
  _ <- liftIO $ System.Exit.exitFailure
  return $ Nil ""
evalfuncExitSuccess _ = do
  _ <- liftIO $ System.Exit.exitSuccess
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
                  , ("exit-fail", evalfuncExitFail)
                  , ("exit-success", evalfuncExitSuccess)
                ]

{- I/O primitives
Primitive functions that execute within the IO monad -}
ioPrimitives :: [(String, [LispVal] -> IOThrowsError LispVal)]
ioPrimitives = [("open-input-file", makePort ReadMode),
                ("open-output-file", makePort WriteMode),
                ("close-input-port", closePort),
                ("close-output-port", closePort),
                ("input-port?", isInputPort),
                ("output-port?", isOutputPort),
                ("char-ready?", isCharReady),

               -- The following optional procedures are NOT implemented:
               --
               {- with-input-from-file
               with-output-from-file
               transcript-on
               transcript-off -}
               --
               {- Consideration may be given in a future release, but keep in mind
               the impact to the other I/O functions. -}

                ("current-input-port", currentInputPort),
                ("current-output-port", currentOutputPort),
                ("read", readProc),
                ("read-char", readCharProc hGetChar),
                ("peek-char", readCharProc hLookAhead),
                ("write", writeProc (\ port obj -> hPrint port obj)),
                ("write-char", writeCharProc),
                ("display", writeProc (\ port obj -> do
                  case obj of
                    String str -> hPutStr port str
                    _ -> hPutStr port $ show obj)),

              ("string=?", strBoolBinop (==)),
              ("string<?", strBoolBinop (<)),
              ("string>?", strBoolBinop (>)),
              ("string<=?", strBoolBinop (<=)),
              ("string>=?", strBoolBinop (>=)),
              ("string-ci=?", stringCIEquals),
              ("string-ci<?", stringCIBoolBinop (<)),
              ("string-ci>?", stringCIBoolBinop (>)),
              ("string-ci<=?", stringCIBoolBinop (<=)),
              ("string-ci>=?", stringCIBoolBinop (>=)),
              ("string->symbol", string2Symbol),

              ("car", car),
              ("cdr", cdr),
              ("cons", cons),

            -- TODO: these need to be rewritten to actually be in 
            -- the IO monad, tmpWrap is just temporary
              ("eq?",    tmpWrap eqv),
              ("eqv?",   tmpWrap eqv),
              ("equal?", tmpWrap equal),

              ("pair?", isDottedList),
              ("list?", unaryOp' isList),
              ("vector?", unaryOp' isVector),
              ("null?", isNull),
              ("string?", isString),

              ("string-length", stringLength),
              ("string-ref", stringRef),
              ("substring", substring),
              ("string-append", stringAppend),
              ("string->number", stringToNumber),
              ("string->list", stringToList),
              ("list->string", listToString),
              ("string-copy", stringCopy),
              ("string->utf8", byteVectorStr2Utf),

              ("bytevector?", unaryOp' isByteVector),
              ("bytevector-length", byteVectorLength),
              ("bytevector-u8-ref", byteVectorRef),
              ("bytevector-append", byteVectorAppend),
              ("bytevector-copy", byteVectorCopy),
              ("utf8->string", byteVectorUtf2Str),

              ("vector-length",wrapLeadObj vectorLength),
              ("vector-ref",   wrapLeadObj vectorRef),
              ("vector->list", wrapLeadObj vectorToList),
              ("list->vector", wrapLeadObj listToVector),

              ("hash-table?",       wrapHashTbl isHashTbl),
              ("hash-table-exists?",wrapHashTbl hashTblExists),
              ("hash-table-ref",    wrapHashTbl hashTblRef),
              ("hash-table-size",   wrapHashTbl hashTblSize),
              ("hash-table->alist", wrapHashTbl hashTbl2List),
              ("hash-table-keys",   wrapHashTbl hashTblKeys),
              ("hash-table-values", wrapHashTbl hashTblValues),
              ("hash-table-copy",   wrapHashTbl hashTblCopy),

                -- From SRFI 96
                ("file-exists?", fileExists),
                ("delete-file", deleteFile),

                -- Other I/O functions
                ("print-env", printEnv'),
                ("env-exports", exportsFromEnv'),
                ("read-contents", readContents),
                ("read-all", readAll),
                ("find-module-file", findModuleFile),
                ("gensym", gensym)]

-- TODO:
-- This function is a temporary stopgap that will go
-- away before this branch is merged
tmpWrap :: ([LispVal] -> ThrowsError LispVal) -> [LispVal] -> IOThrowsError LispVal
tmpWrap fnc lvs = do
    List result <- recDerefPtrs $ List lvs 
    liftThrows $ fnc result

printEnv' :: [LispVal] -> IOThrowsError LispVal
printEnv' [LispEnv env] = do
    result <- liftIO $ printEnv env
    return $ String result
printEnv' [] = throwError $ NumArgs (Just 1) []
printEnv' args = throwError $ TypeMismatch "env" $ List args

exportsFromEnv' :: [LispVal] -> IOThrowsError LispVal
exportsFromEnv' [LispEnv env] = do
    result <- liftIO $ exportsFromEnv env
    return $ List result
exportsFromEnv' err = return $ List []

{- "Pure" primitive functions -}
primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [("+", numAdd),
              ("-", numSub),
              ("*", numMul),
              ("/", numDiv),
              ("modulo", numMod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem),
              ("rationalize", numRationalize),

              ("round", numRound),
              ("floor", numFloor),
              ("ceiling", numCeiling),
              ("truncate", numTruncate),

              ("numerator", numNumerator),
              ("denominator", numDenominator),

              ("exp", numExp),
              ("log", numLog),
              ("sin", numSin),
              ("cos", numCos),
              ("tan", numTan),
              ("asin", numAsin),
              ("acos", numAcos),
              ("atan", numAtan),

              ("sqrt", numSqrt),
              ("expt", numExpt),

              ("make-rectangular", numMakeRectangular),
              ("make-polar", numMakePolar),
              ("real-part", numRealPart ),
              ("imag-part", numImagPart),
              ("magnitude", numMagnitude),
              ("angle", numAngle ),

              ("exact->inexact", numExact2Inexact),
              ("inexact->exact", numInexact2Exact),

              ("number->string", num2String),

              ("=", numBoolBinopEq),
              (">", numBoolBinopGt),
              (">=", numBoolBinopGte),
              ("<", numBoolBinopLt),
              ("<=", numBoolBinopLte),

              ("&&", boolBoolBinop (&&)),
              ("||", boolBoolBinop (||)),

              ("char=?",  charBoolBinop (==)),
              ("char<?",  charBoolBinop (<)),
              ("char>?",  charBoolBinop (>)),
              ("char<=?", charBoolBinop (<=)),
              ("char>=?", charBoolBinop (>=)),
              ("char-ci=?",  charCIBoolBinop (==)),
              ("char-ci<?",  charCIBoolBinop (<)),
              ("char-ci>?",  charCIBoolBinop (>)),
              ("char-ci<=?", charCIBoolBinop (<=)),
              ("char-ci>=?", charCIBoolBinop (>=)),
              ("char-alphabetic?", charPredicate Data.Char.isAlpha),
              ("char-numeric?", charPredicate Data.Char.isNumber),
              ("char-whitespace?", charPredicate Data.Char.isSpace),
              ("char-upper-case?", charPredicate Data.Char.isUpper),
              ("char-lower-case?", charPredicate Data.Char.isLower),
              ("char->integer", char2Int),
              ("integer->char", int2Char),
              ("char-upper", charUpper),
              ("char-lower", charLower),

              ("procedure?", isProcedure),
              ("number?", isNumber),
              ("complex?", isComplex),
              ("real?", isReal),
              ("rational?", isRational),
              ("integer?", isInteger),
              ("eof-object?", isEOFObject),
              ("symbol?", isSymbol),
              ("symbol->string", symbol2String),
              ("char?", isChar),

              ("make-vector", makeVector),
              ("vector", buildVector),

              ("make-bytevector", makeByteVector),
              ("bytevector", byteVector),

              ("make-hash-table", hashTblMake),
              ("string", buildString),
              ("make-string", makeString),

              ("boolean?", isBoolean)]

