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
    , evalString
    , evalAndPrint
    , apply
    , continueEval
    -- * Core data
    , primitiveBindings
    , version
    -- * Utility functions
    , escapeBackslashes
    , showBanner
    , substr
    , updateVector
    ) where
#ifdef UseFfi
import qualified Language.Scheme.FFI
#endif
import qualified Language.Scheme.Macro
import Language.Scheme.Numerical
import Language.Scheme.Parser
import Language.Scheme.Primitives
import Language.Scheme.Types
import Language.Scheme.Variables
import Control.Monad.Error
import Data.Array
import qualified Data.Char
import qualified Data.Map
import qualified System.Exit
import System.IO
--import Debug.Trace

-- |husk version number
version :: String
version = "3.5.7"

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
  putStrLn " (c) 2010-2012 Justin Ethier                                             "
  putStrLn $ " Version " ++ version ++ " "
  putStrLn "                                                                         "

-- |A utility function to escape backslashes in the given string
escapeBackslashes :: String -> String
escapeBackslashes s = foldr step [] s
  where step x xs  | x == '\\'  = '\\' : '\\' : xs
                   | otherwise =  x : xs 


{- |Evaluate a string containing Scheme code.

    For example:

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

-- |Evaluate lisp code that has already been loaded into haskell
evalLisp :: Env -> LispVal -> IOThrowsError LispVal
evalLisp env lisp = do
  v <- meval env (makeNullContinuation env) lisp
  recDerefPtrs v

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
eval env cont val@(Pointer _ _) = continueEval env cont val
eval env cont (Atom a) = do
  v <- getVar env a
  val <- return $ case v of
    List _ -> Pointer a env
    DottedList _ _ -> Pointer a env
    String _ -> Pointer a env
    Vector _ -> Pointer a env
    HashTable _ -> Pointer a env
    _ -> v
  -- TODO: only return a pointer for some types? 
  -- for example, can a number just be returned directly?
  continueEval env cont val

-- Quote an expression by simply passing along the value
eval env cont (List [Atom "quote", val]) = continueEval env cont val

{- Unquote an expression; unquoting is different than quoting in that
it may also be inter-spliced with code that is meant to be evaluated. -}
--
--
{- FUTURE: Issue #8 - https://github.com/justinethier/husk-scheme/issues/#issue/8
need to take nesting of ` into account, as per spec: -}
--
{- - Quasiquote forms may be nested.
- Substitutions are made only for unquoted components appearing at the
same nesting level as the outermost backquote.
- The nesting level increases by one inside each successive quasiquotation,
and decreases by one inside each unquotation. -}
--
{- So the upshoot is that a new nesting level var needs to be threaded through,
and used to determine whether or not to evaluate an unquote. -}
--
eval envi cont (List [Atom "quasiquote", value]) = cpsUnquote envi cont value Nothing
  where cpsUnquote :: Env -> LispVal -> LispVal -> Maybe [LispVal] -> IOThrowsError LispVal
        cpsUnquote e c val _ = do
          case val of
            List [Atom "unquote", vval] -> meval e c vval
            List (_ : _) -> doCpsUnquoteList e c val
            DottedList xs x -> do
              doCpsUnquoteList e (makeCPSWArgs e c cpsUnquotePair $ [x] ) $ List xs
            Vector vec -> do
              let len = length (elems vec)
              if len > 0
                 then doCpsUnquoteList e (makeCPS e c cpsUnquoteVector) $ List $ elems vec
                 else continueEval e c $ Vector $ listArray (0, -1) []
            _ -> meval e c (List [Atom "quote", val])  -- Behave like quote if there is nothing to "unquote"...

        {- Unquote a pair
        This must be started by unquoting the "left" hand side of the pair,
        then pass a continuation to this function to unquote the right-hand side (RHS).
        This function does the RHS and then calls into a continuation to finish the pair. -}
        cpsUnquotePair :: Env -> LispVal -> LispVal -> Maybe [LispVal] -> IOThrowsError LispVal
        cpsUnquotePair e c (List rxs) (Just [rx]) = do
          cpsUnquote e (makeCPSWArgs e c cpsUnquotePairFinish $ [List rxs]) rx Nothing
        cpsUnquotePair _ _ _ _ = throwError $ InternalError "Unexpected parameters to cpsUnquotePair"

        -- Finish unquoting a pair by combining both of the unquoted left/right hand sides.
        cpsUnquotePairFinish :: Env -> LispVal -> LispVal -> Maybe [LispVal] -> IOThrowsError LispVal
        cpsUnquotePairFinish e c rx (Just [List rxs]) = do
            case rx of
              List [] -> continueEval e c $ List rxs
              List rxlst -> continueEval e c $ List $ rxs ++ rxlst
              DottedList rxlst rxlast -> continueEval e c $ DottedList (rxs ++ rxlst) rxlast
              _ -> continueEval e c $ DottedList rxs rx
        cpsUnquotePairFinish _ _ _ _ = throwError $ InternalError "Unexpected parameters to cpsUnquotePairFinish"

        -- Unquote a vector
        cpsUnquoteVector :: Env -> LispVal -> LispVal -> Maybe [LispVal] -> IOThrowsError LispVal
        cpsUnquoteVector e c (List vList) _ = continueEval e c (Vector $ listArray (0, (length vList - 1)) vList)
        cpsUnquoteVector _ _ _ _ = throwError $ InternalError "Unexpected parameters to cpsUnquoteVector"

        -- Front-end to cpsUnquoteList, to encapsulate default values in the call
        doCpsUnquoteList :: Env -> LispVal -> LispVal -> IOThrowsError LispVal
        doCpsUnquoteList e c (List (x : xs)) = cpsUnquoteList e c x $ Just ([List xs, List []])
        doCpsUnquoteList _ _ _ = throwError $ InternalError "Unexpected parameters to doCpsUnquoteList"

        -- Unquote a list
        cpsUnquoteList :: Env -> LispVal -> LispVal -> Maybe [LispVal] -> IOThrowsError LispVal
        cpsUnquoteList e c val (Just ([List unEvaled, List acc])) = do
            case val of
                List [Atom "unquote-splicing", vvar] -> do
                    meval e (makeCPSWArgs e c cpsUnquoteSplicing $ [List unEvaled, List acc]) vvar
                _ -> cpsUnquote e (makeCPSWArgs e c cpsUnquoteFld $ [List unEvaled, List acc]) val Nothing
        cpsUnquoteList _ _ _ _ = throwError $ InternalError "Unexpected parameters to cpsUnquoteList"

        -- Evaluate an expression instead of quoting it
        cpsUnquoteSplicing :: Env -> LispVal -> LispVal -> Maybe [LispVal] -> IOThrowsError LispVal
        cpsUnquoteSplicing e c val (Just ([List unEvaled, List acc])) = do
                    case val of
                        List v -> case unEvaled of
                                    [] -> continueEval e c $ List $ acc ++ v
                                    _ -> cpsUnquoteList e c (head unEvaled) (Just [List (tail unEvaled), List $ acc ++ v ])
                        _ -> throwError $ TypeMismatch "proper list" val
        cpsUnquoteSplicing _ _ _ _ = throwError $ InternalError "Unexpected parameters to cpsUnquoteSplicing"

        -- Unquote processing for single field of a list
        cpsUnquoteFld :: Env -> LispVal -> LispVal -> Maybe [LispVal] -> IOThrowsError LispVal
        cpsUnquoteFld e c val (Just ([List unEvaled, List acc])) = do
          case unEvaled of
            [] -> continueEval e c $ List $ acc ++ [val]
            _ -> cpsUnquoteList e c (head unEvaled) (Just [List (tail unEvaled), List $ acc ++ [val] ])
        cpsUnquoteFld _ _ _ _ = throwError $ InternalError "Unexpected parameters to cpsUnquoteFld"

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
  else throwError $ NumArgs 2 args

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
  else do result <- (makeNormalFunc env fparams fbody >>= defineVar env var)
          continueEval env cont result

eval env cont args@(List (Atom "define" : DottedList (Atom var : fparams) varargs : fbody)) = do
 bound <- liftIO $ isRecBound env "define"
 if bound
  then prepareApply env cont args -- if is bound to a variable in this scope; call into it
  else do result <- (makeVarargs varargs env fparams fbody >>= defineVar env var)
          continueEval env cont result

eval env cont args@(List (Atom "lambda" : List fparams : fbody)) = do
 bound <- liftIO $ isRecBound env "lambda"
 if bound
  then prepareApply env cont args -- if is bound to a variable in this scope; call into it
  else do result <- makeNormalFunc env fparams fbody
          continueEval env cont result

eval env cont args@(List (Atom "lambda" : DottedList fparams varargs : fbody)) = do
 bound <- liftIO $ isRecBound env "lambda"
 if bound
  then prepareApply env cont args -- if is bound to a variable in this scope; call into it
  else do result <- makeVarargs varargs env fparams fbody
          continueEval env cont result

eval env cont args@(List (Atom "lambda" : varargs@(Atom _) : fbody)) = do
 bound <- liftIO $ isRecBound env "lambda"
 if bound
  then prepareApply env cont args -- if is bound to a variable in this scope; call into it
  else do result <- makeVarargs varargs env [] fbody
          continueEval env cont result

eval env cont args@(List [Atom "string-set!", Atom var, i, character]) = do
 bound <- liftIO $ isRecBound env "string-set!"
 if bound
  then prepareApply env cont args -- if is bound to a variable in this scope; call into it
  else meval env (makeCPS env cont cpsStr) i
 where
        cpsStr :: Env -> LispVal -> LispVal -> Maybe [LispVal] -> IOThrowsError LispVal
        cpsStr e c idx _ = (meval e (makeCPSWArgs e c cpsSubStr $ [idx]) =<< getVar e var) 

        cpsSubStr :: Env -> LispVal -> LispVal -> Maybe [LispVal] -> IOThrowsError LispVal
        cpsSubStr e c str (Just [idx]) =
            substr (str, character, idx) >>= setVar e var >>= continueEval e c
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
  else throwError $ NumArgs 3 args

eval env cont args@(List [Atom "set-car!", Atom var, argObj]) = do
 bound <- liftIO $ isRecBound env "set-car!"
 if bound
  then prepareApply env cont args -- if is bound to a variable in this scope; call into it
  else continueEval env (makeCPS env cont cpsObj) =<< getVar env var
 where
        cpsObj :: Env -> LispVal -> LispVal -> Maybe [LispVal] -> IOThrowsError LispVal
        cpsObj _ _ obj@(List []) _ = throwError $ TypeMismatch "pair" obj
        cpsObj e c obj@(List (_ : _)) _ = meval e (makeCPSWArgs e c cpsSet $ [obj]) argObj
        cpsObj e c obj@(DottedList _ _) _ =  meval e (makeCPSWArgs e c cpsSet $ [obj]) argObj
        cpsObj _ _ obj _ = throwError $ TypeMismatch "pair" obj

        cpsSet :: Env -> LispVal -> LispVal -> Maybe [LispVal] -> IOThrowsError LispVal
        cpsSet e c obj (Just [List (_ : ls)]) = setVar e var (List (obj : ls)) >>= continueEval e c -- Wrong constructor? Should it be DottedList?
        cpsSet e c obj (Just [DottedList (_ : ls) l]) = setVar e var (DottedList (obj : ls) l) >>= continueEval e c
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
  else throwError $ NumArgs 2 args


-- TODO: There is too much redundancy in this case vs the next one
--  they need to be combined
{-
eval env cont args@(List [Atom "set-cdr!", Pointer pVar pEnv, argObj]) = do
 bound <- liftIO $ isRecBound env "set-cdr!"
 if bound
  then prepareApply env cont args -- if is bound to a variable in this scope; call into it
  else continueEval env (makeCPS env cont cpsObj) =<< getVar pEnv pVar
 where
        cpsObj :: Env -> LispVal -> LispVal -> Maybe [LispVal] -> IOThrowsError LispVal
        cpsObj _ _ pair@(List []) _ = throwError $ TypeMismatch "pair 1" pair
        cpsObj e c pair@(List (_ : _)) _ = meval e (makeCPSWArgs e c cpsSet $ [pair]) argObj
        cpsObj e c pair@(DottedList _ _) _ = meval e (makeCPSWArgs e c cpsSet $ [pair]) argObj
        cpsObj e c pair@(Pointer _ _) _ = do
          value <- recDerefPtrs pair
          meval e (makeCPSWArgs e c cpsSet $ [value]) argObj
        cpsObj _ _ pair _ = throwError $ TypeMismatch "pair 2" pair

        cpsSet :: Env -> LispVal -> LispVal -> Maybe [LispVal] -> IOThrowsError LispVal
        cpsSet e c obj (Just [List (l : _)]) = (liftThrows $ cons [l, obj]) >>= setVar pEnv pVar >>= continueEval e c
        cpsSet e c obj (Just [DottedList (l : _) _]) = (liftThrows $ cons [l, obj]) >>= setVar pEnv pVar >>= continueEval e c
        cpsSet _ _ _ _ = throwError $ InternalError "Unexpected argument to cpsSet"
-}
eval env cont args@(List [Atom "set-cdr!", Atom var, argObj]) = do
 bound <- liftIO $ isRecBound env "set-cdr!"
 if bound
  then prepareApply env cont args -- if is bound to a variable in this scope; call into it
  else continueEval env (makeCPS env cont cpsObj) =<< getVar env var
 where
        cpsObj :: Env -> LispVal -> LispVal -> Maybe [LispVal] -> IOThrowsError LispVal
        cpsObj _ _ pair@(List []) _ = throwError $ TypeMismatch "pair" pair
        cpsObj e c pair@(List (_ : _)) _ = meval e (makeCPSWArgs e c cpsSet $ [pair]) argObj
        cpsObj e c pair@(DottedList _ _) _ = meval e (makeCPSWArgs e c cpsSet $ [pair]) argObj
        cpsObj _ _ pair _ = throwError $ TypeMismatch "pair" pair

        cpsSet :: Env -> LispVal -> LispVal -> Maybe [LispVal] -> IOThrowsError LispVal
        cpsSet e c obj (Just [List (l : _)]) = (liftThrows $ cons [l, obj]) >>= setVar e var >>= continueEval e c
        cpsSet e c obj (Just [DottedList (l : _) _]) = (liftThrows $ cons [l, obj]) >>= setVar e var >>= continueEval e c
        cpsSet _ _ _ _ = throwError $ InternalError "Unexpected argument to cpsSet"
eval env cont args@(List [Atom "set-cdr!" , nonvar , _ ]) = do
 bound <- liftIO $ isRecBound env "set-cdr!"
 if bound
  then prepareApply env cont args -- if is bound to a variable in this scope; call into it
  else throwError $ TypeMismatch "variable" nonvar
eval env cont fargs@(List (Atom "set-cdr!" : args)) = do
 bound <- liftIO $ isRecBound env "set-cdr!"
 if bound
  then prepareApply env cont fargs -- if is bound to a variable in this scope; call into it
  else throwError $ NumArgs 2 args

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
            updateVector vec idx obj >>= setVar e var >>= continueEval e c
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
  else throwError $ NumArgs 3 args

eval env cont args@(List [Atom "hash-table-set!", Atom var, rkey, rvalue]) = do
 bound <- liftIO $ isRecBound env "hash-table-set!"
 if bound
  then prepareApply env cont args -- if is bound to a variable in this scope; call into it
  else meval env (makeCPS env cont cpsValue) rkey
 where
        cpsValue :: Env -> LispVal -> LispVal -> Maybe [LispVal] -> IOThrowsError LispVal
        cpsValue e c key _ = meval e (makeCPSWArgs e c cpsH $ [key]) rvalue

        cpsH :: Env -> LispVal -> LispVal -> Maybe [LispVal] -> IOThrowsError LispVal
        cpsH e c value (Just [key]) = (meval e (makeCPSWArgs e c cpsEvalH $ [key, value]) =<< getVar e var) 
        cpsH _ _ _ _ = throwError $ InternalError "Invalid argument to cpsH"

        cpsEvalH :: Env -> LispVal -> LispVal -> Maybe [LispVal] -> IOThrowsError LispVal
        cpsEvalH e c h (Just [key, value]) = do
            case h of
                HashTable ht -> do
                  setVar env var (HashTable $ Data.Map.insert key value ht) >>= meval e c
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
  else throwError $ NumArgs 3 args

eval env cont args@(List [Atom "hash-table-delete!", Atom var, rkey]) = do
 bound <- liftIO $ isRecBound env "hash-table-delete!"
 if bound
  then prepareApply env cont args -- if is bound to a variable in this scope; call into it
  else meval env (makeCPS env cont cpsH) rkey
 where
        cpsH :: Env -> LispVal -> LispVal -> Maybe [LispVal] -> IOThrowsError LispVal
        cpsH e c key _ = (meval e (makeCPSWArgs e c cpsEvalH $ [key]) =<< getVar e var) 

        cpsEvalH :: Env -> LispVal -> LispVal -> Maybe [LispVal] -> IOThrowsError LispVal
        cpsEvalH e c h (Just [key]) = do
            case h of
                HashTable ht -> do
                  setVar env var (HashTable $ Data.Map.delete key ht) >>= meval e c
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
  else throwError $ NumArgs 2 args

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
updateVector v _ _ = throwError $ TypeMismatch "vector" v

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
apply :: LispVal -> LispVal -> [LispVal] -> IOThrowsError LispVal
apply _ cont@(Continuation env ccont ncont _ ndynwind) args = do
-- case (trace ("calling into continuation. dynWind = " ++ show ndynwind) ndynwind) of
  case ndynwind of
    -- Call into dynWind.before if it exists...
    Just ([DynamicWinders beforeFunc _]) -> apply (makeCPS env cont cpsApply) beforeFunc []
    _ -> doApply env cont
 where
   cpsApply :: Env -> LispVal -> LispVal -> Maybe [LispVal] -> IOThrowsError LispVal
   cpsApply e c _ _ = doApply e c
   doApply e c =
      case (toInteger $ length args) of
        0 -> throwError $ NumArgs 1 []
        1 -> continueEval e c $ head args
        _ ->  -- Pass along additional arguments, so they are available to (call-with-values)
             continueEval e (Continuation env ccont ncont (Just $ tail args) ndynwind) $ head args
apply cont (IOFunc func) args = do
  List dargs <- recDerefPtrs $ List args -- Deref any pointers
  result <- func dargs
  case cont of
    Continuation cEnv _ _ _ _ -> continueEval cEnv cont result
    _ -> return result
apply cont (EvalFunc func) args = do
    {- An EvalFunc extends the evaluator so it needs access to the current continuation;
    pass it as the first argument. -}
    func (cont : args)
apply cont (PrimitiveFunc func) args = do
  List dargs <- recDerefPtrs $ List args -- Deref any pointers
  result <- liftThrows $ func dargs
  case cont of
    Continuation cEnv _ _ _ _ -> continueEval cEnv cont result
    _ -> return result
apply cont (Func aparams avarargs abody aclosure) args =
  if num aparams /= num args && avarargs == Nothing
     then throwError $ NumArgs (num aparams) args
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
     then throwError $ NumArgs (num aparams) args
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
apply _ func args = throwError $ BadSpecialForm "Unable to evaluate form" $ List (func : args)

{- |Environment containing the primitive forms that are built into the Scheme language. Note that this only includes
forms that are implemented in Haskell; derived forms implemented in Scheme (such as let, list, etc) are available
in the standard library which must be pulled into the environment using /(load)/. -}
primitiveBindings :: IO Env
primitiveBindings = nullEnv >>= (flip extendEnv $ map (domakeFunc IOFunc) ioPrimitives
                                               ++ map (domakeFunc EvalFunc) evalFunctions
                                               ++ map (domakeFunc PrimitiveFunc) primitives)
  where domakeFunc constructor (var, func) = ((varNamespace, var), constructor func)

-- Functions that extend the core evaluator, but that can be defined separately.
--
{- These functions have access to the current environment via the
current continuation, which is passed as the first LispVal argument. -}
--
evalfuncExitSuccess, evalfuncExitFail, evalfuncApply, evalfuncDynamicWind, evalfuncEval, evalfuncLoad, evalfuncCallCC, evalfuncCallWValues :: [LispVal] -> IOThrowsError LispVal

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
evalfuncDynamicWind (_ : args) = throwError $ NumArgs 3 args -- Skip over continuation argument
evalfuncDynamicWind _ = throwError $ NumArgs 3 []

evalfuncCallWValues [cont@(Continuation env _ _ _ _), producer, consumer] = do
  apply (makeCPS env cont cpsEval) producer [] -- Call into prod to get values
 where
   cpsEval :: Env -> LispVal -> LispVal -> Maybe [LispVal] -> IOThrowsError LispVal
   cpsEval _ c@(Continuation _ _ _ (Just xargs) _) value _ = apply c consumer (value : xargs)
   cpsEval _ c value _ = apply c consumer [value]
evalfuncCallWValues (_ : args) = throwError $ NumArgs 2 args -- Skip over continuation argument
evalfuncCallWValues _ = throwError $ NumArgs 2 []

--evalfuncApply [cont@(Continuation _ _ _ _ _), func, List args] = apply cont func args
evalfuncApply (cont@(Continuation _ _ _ _ _) : func : args) = do
  let aRev = reverse args

  if null args
     then throwError $ NumArgs 2 args
     else case head aRev of
            List aLastElems -> do
              apply cont func $ (init args) ++ aLastElems
            other -> throwError $ TypeMismatch "List" other
evalfuncApply (_ : args) = throwError $ NumArgs 2 args -- Skip over continuation argument
evalfuncApply _ = throwError $ NumArgs 2 []

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

evalfuncLoad (_ : args) = throwError $ NumArgs 1 args -- Skip over continuation argument
evalfuncLoad _ = throwError $ NumArgs 1 []

-- Evaluate an expression in the current environment
--
-- Assumption is any macro transform is already performed
-- prior to this step.
--
-- FUTURE: consider allowing env to be specified, per R5RS
--
evalfuncEval [cont@(Continuation env _ _ _ _), val] = meval env cont val
evalfuncEval (_ : args) = throwError $ NumArgs 1 args -- Skip over continuation argument
evalfuncEval _ = throwError $ NumArgs 1 []

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
         else throwError $ NumArgs (toInteger $ length aparams) [cont]
     HFunc _ (Just _) _ _ -> apply cont func [cont] -- Variable # of args (pair). Just call into cont  
     HFunc aparams _ _ _ ->
       if (toInteger $ length aparams) == 1
         then apply cont func [cont]
         else throwError $ NumArgs (toInteger $ length aparams) [cont]
     other -> throwError $ TypeMismatch "procedure" other
evalfuncCallCC (_ : args) = throwError $ NumArgs 1 args -- Skip over continuation argument
evalfuncCallCC _ = throwError $ NumArgs 1 []

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
               -- Non-standard extensions
#ifdef UseFfi
                  , ("load-ffi", Language.Scheme.FFI.evalfuncLoadFFI)
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

                -- From SRFI 96
                ("file-exists?", fileExists),
                ("delete-file", deleteFile),

                -- Other I/O functions
                ("read-contents", readContents),
                ("read-all", readAll),
                ("gensym", gensym)]

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

              ("car", car),
              ("cdr", cdr),
              ("cons", cons),
              ("eq?", eqv),
              ("eqv?", eqv),
              ("equal?", equal),

              ("pair?", isDottedList),
              ("procedure?", isProcedure),
              ("number?", isNumber),
              ("complex?", isComplex),
              ("real?", isReal),
              ("rational?", isRational),
              ("integer?", isInteger),
              ("list?", unaryOp isList),
              ("null?", isNull),
              ("eof-object?", isEOFObject),
              ("symbol?", isSymbol),
              ("symbol->string", symbol2String),
              ("string->symbol", string2Symbol),
              ("char?", isChar),

              ("vector?", unaryOp isVector),
              ("make-vector", makeVector),
              ("vector", buildVector),
              ("vector-length", vectorLength),
              ("vector-ref", vectorRef),
              ("vector->list", vectorToList),
              ("list->vector", listToVector),

              ("make-hash-table", hashTblMake),
              ("hash-table?", isHashTbl),
              ("hash-table-exists?", hashTblExists),
              ("hash-table-ref", hashTblRef),
              ("hash-table-size", hashTblSize),
              ("hash-table->alist", hashTbl2List),
              ("hash-table-keys", hashTblKeys),
              ("hash-table-values", hashTblValues),
              ("hash-table-copy", hashTblCopy),

              ("string?", isString),
              ("string", buildString),
              ("make-string", makeString),
              ("string-length", stringLength),
              ("string-ref", stringRef),
              ("substring", substring),
              ("string-append", stringAppend),
              ("string->number", stringToNumber),
              ("string->list", stringToList),
              ("list->string", listToString),
              ("string-copy", stringCopy),

              ("boolean?", isBoolean)]

