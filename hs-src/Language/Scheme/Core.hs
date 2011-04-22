{-
 - husk scheme interpreter
 -
 - A lightweight dialect of R5RS scheme.
 - This file contains Core functionality, primarily Scheme expression evaluation.
 -
 - @author Justin Ethier
 -
 - -}

module Language.Scheme.Core 
    (
      eval
    , evalLisp
    , evalString
    , evalAndPrint
    , primitiveBindings -- FUTURE: this is a bad idea.
                        -- There should be an interface to inject custom functions written in Haskell.
                        --
                        -- Probably any new func should be added as an EvalFunc or IOFunc
                        --
                        -- If so, need to ensure that apply handles them properly, and that continuations are
                        -- captured properly.
    ) where
import Language.Scheme.Macro
import Language.Scheme.Numerical
import Language.Scheme.Parser
import Language.Scheme.Types
import Language.Scheme.Variables
import Control.Monad.Error
import Char
import Data.Array
import qualified Data.Map
import Maybe
import List
import IO hiding (try)
import System.Directory (doesFileExist)
import System.IO.Error
--import Debug.Trace

{-| Evaluate a string containing Scheme code.

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
evalString env expr = runIOThrows $ liftM show $ (liftThrows $ readExpr expr) >>= macroEval env >>= (eval env (makeNullContinuation env))

-- |Evaluate a string and print results to console
evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = evalString env expr >>= putStrLn

-- |Evaluate lisp code that has already been loaded into haskell
--
--  FUTURE: code example for this, via ghci and/or a custom Haskell program.
evalLisp :: Env -> LispVal -> IOThrowsError LispVal
evalLisp env lisp = macroEval env lisp >>= (eval env (makeNullContinuation env))


{- continueEval is a support function for eval, below.
 -
 - Transformed eval section into CPS by calling into this instead of returning from eval.
 - This function uses the cont argument to determine whether to keep going or to finally
 - return a result.
 - -}
continueEval :: Env -> LispVal -> LispVal -> IOThrowsError LispVal

-- Passing a higher-order function as the continuation; just evaluate it. This is 
-- done to enable an 'eval' function to be broken up into multiple sub-functions,
-- so that any of the sub-functions can be passed around as a continuation.
--
-- Carry extra args from the current continuation into the next, to support (call-with-values)
continueEval _
            (Continuation cEnv (Just (HaskellBody func funcArgs)) 
                               (Just (Continuation cce cnc ccc _ cdynwind)) 
                                xargs _) 
             val = func cEnv (Continuation cce cnc ccc xargs cdynwind) val funcArgs

-- No higher order function, so:
--
-- If there is Scheme code to evaluate in the function body, we continue to evaluate it.
--
-- Otherwise, if all code in the function has been executed, we 'unwind' to an outer
-- continuation (if there is one), or we just return the result. Yes technically with
-- CPS you are supposed to keep calling into functions and never return, but eventually
-- when the computation is complete, you have to return something.
continueEval _ (Continuation cEnv (Just (SchemeBody cBody)) (Just cCont) extraArgs dynWind) val = do
    case cBody of
        [] -> do
          case cCont of
            Continuation nEnv ncCont nnCont _ nDynWind -> 
              -- Pass extra args along if last expression of a function, to support (call-with-values)
              continueEval nEnv (Continuation nEnv ncCont nnCont extraArgs nDynWind) val 
            _ -> return (val)
        [lv] -> eval cEnv (Continuation cEnv (Just (SchemeBody [])) (Just cCont) Nothing dynWind) (lv)
        (lv : lvs) -> eval cEnv (Continuation cEnv (Just (SchemeBody lvs)) (Just cCont) Nothing dynWind) (lv)

-- No current continuation, but a next cont is available; call into it
continueEval _ (Continuation cEnv Nothing (Just cCont) _ _) val = continueEval cEnv cCont val

-- There is no continuation code, just return value
continueEval _ (Continuation _ Nothing Nothing _ _) val = return val
continueEval _ _ _ = throwError $ Default "Internal error in continueEval"

-- |Core eval function
--  Evaluate a scheme expression. 
--  NOTE:  This function does not include macro support and should not be called directly. Instead, use 'evalLisp'
--
--
--  Implementation Notes:
--
--  Internally, this function is written in continuation passing style (CPS) to allow the Scheme language
--  itself to support first-class continuations. That is, at any point in the evaluation, call/cc may
--  be used to capture the current continuation. Thus this code must call into the next continuation point, eg:
--
--    eval ... (makeCPS ...)
--
--  Instead of calling eval directly from within the same function, eg:
--
--    eval ...
--    eval ...
--
--  This can make the code harder to follow, however some coding conventions have been established to make the
--  code easier to follow. Whenever a single function has been broken into multiple ones for the purpose of CPS,
--  those additional functions are defined locally using 'where', and each has been given a 'cps' prefix.
--
eval :: Env -> LispVal -> LispVal -> IOThrowsError LispVal
eval env cont val@(Nil _)       = continueEval env cont val
eval env cont val@(String _)    = continueEval env cont val
eval env cont val@(Char _)      = continueEval env cont val
eval env cont val@(Complex _)   = continueEval env cont val
eval env cont val@(Float _)     = continueEval env cont val
eval env cont val@(Rational _)  = continueEval env cont val
eval env cont val@(Number _)    = continueEval env cont val
eval env cont val@(Bool _)      = continueEval env cont val
eval env cont val@(HashTable _) = continueEval env cont val
eval env cont val@(Vector _)    = continueEval env cont val
eval env cont (Atom a)          = continueEval env cont =<< getVar env a

-- Quote an expression by simply passing along the value
eval env cont (List [Atom "quote", val])         = continueEval env cont val

-- Unquote an expression; unquoting is different than quoting in that
-- it may also be inter-spliced with code that is meant to be evaluated.
--
--
-- FUTURE: Issue #8 - https://github.com/justinethier/husk-scheme/issues/#issue/8
--   need to take nesting of ` into account, as per spec:
-- 
-- * Quasiquote forms may be nested. 
-- * Substitutions are made only for unquoted components appearing at the 
--   same nesting level as the outermost backquote. 
-- * The nesting level increases by one inside each successive quasiquotation, 
--   and decreases by one inside each unquotation.
--
-- So the upshoot is that a new nesting level var needs to be threaded through,
-- and used to determine whether or not to evaluate an unquote.
--
eval envi cont (List [Atom "quasiquote", value]) = cpsUnquote envi cont value Nothing
  where cpsUnquote :: Env -> LispVal -> LispVal -> Maybe [LispVal] -> IOThrowsError LispVal
        cpsUnquote e c val _ = do 
          case val of
            List [Atom "unquote", vval] -> eval e c vval
            List (_ : _) -> doCpsUnquoteList e c val
            DottedList xs x -> do
              doCpsUnquoteList e (makeCPSWArgs e c cpsUnquotePair $ [x] ) $ List xs
            Vector vec -> do
              let len = length (elems vec)
              if len > 0
                 then doCpsUnquoteList e (makeCPS e c cpsUnquoteVector) $ List $ elems vec
                 else continueEval e c $ Vector $ listArray (0, -1) []
            _ -> eval e c  (List [Atom "quote", val]) -- Behave like quote if there is nothing to "unquote"...

        -- Unquote a pair
        --  This must be started by unquoting the "left" hand side of the pair,
        --  then pass a continuation to this function to unquote the right-hand side (RHS).
        --  This function does the RHS and then calls into a continuation to finish the pair.
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
        doCpsUnquoteList e c (List (x:xs)) = cpsUnquoteList e c x $ Just ([List xs, List []])
        doCpsUnquoteList _ _ _ = throwError $ InternalError "Unexpected parameters to doCpsUnquoteList"

        -- Unquote a list
        cpsUnquoteList :: Env -> LispVal -> LispVal -> Maybe [LispVal] -> IOThrowsError LispVal
        cpsUnquoteList e c val (Just ([List unEvaled, List acc])) = do
            case val of
                List [Atom "unquote-splicing", vvar] -> do
                    eval e (makeCPSWArgs e c cpsUnquoteSplicing $ [List unEvaled, List acc]) vvar
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

eval env cont (List [Atom "if", predic, conseq, alt]) = do
  eval env (makeCPS env cont cps) (predic)
  where   cps :: Env -> LispVal -> LispVal -> Maybe [LispVal] -> IOThrowsError LispVal
          cps e c result _ = 
            case (result) of
              Bool False -> eval e c alt
              _ -> eval e c conseq

eval env cont (List [Atom "if", predic, conseq]) = 
    eval env (makeCPS env cont cpsResult) predic
    where cpsResult :: Env -> LispVal -> LispVal -> Maybe [LispVal] -> IOThrowsError LispVal
          cpsResult e c result _ = 
            case result of
              Bool True -> eval e c conseq
              _ -> continueEval e c $ Nil "" -- Unspecified return value per R5RS

-- FUTURE: convert cond to a derived form (scheme macro)
eval env cont (List (Atom "cond" : clauses)) = 
  if length clauses == 0
   then throwError $ BadSpecialForm "No matching clause" $ String "cond"
   else do
       case (clauses !! 0) of
         List [test, Atom "=>", expr] -> eval env (makeCPSWArgs env cont cpsAlt [test]) expr
         List (Atom "else" : _) -> eval env (makeCPSWArgs env cont cpsResult clauses) $ Bool True
         List (cond : _) -> eval env (makeCPSWArgs env cont cpsResult clauses) cond
         badType -> throwError $ TypeMismatch "clause" badType 
  where
        -- If a condition is true, evaluate that condition's expressions.
        -- Otherwise just pick up at the next condition...
        cpsResult :: Env -> LispVal -> LispVal -> Maybe [LispVal] -> IOThrowsError LispVal
        cpsResult e cnt result (Just (c:cs)) = 
            case result of
              Bool True -> evalCond e cnt c
              _ -> eval env cnt $ List $ (Atom "cond" : cs)
        cpsResult _ _ _ _ = throwError $ Default "Unexpected error in cond"

        -- Helper function for evaluating 'cond'
        evalCond :: Env -> LispVal -> LispVal -> IOThrowsError LispVal
        evalCond e c (List [_, expr]) = eval e c expr
        evalCond e c (List (_ : expr)) = eval e c $ List (Atom "begin" : expr)
        evalCond _ _ badForm = throwError $ BadSpecialForm "evalCond: Unrecognized special form" badForm

        -- Alternate "=>" form: expr was evaluated, now eval test
        cpsAlt :: Env -> LispVal -> LispVal -> Maybe [LispVal] -> IOThrowsError LispVal
        cpsAlt e c expr (Just [test]) = eval e (makeCPSWArgs e c cpsAltEvaled [expr]) test
        cpsAlt _ _ _ _ = throwError $ Default "Unexpected error in cond"

        -- Alternate "=>" form: both test/expr are evaluated, now eval the form itself
        cpsAltEvaled :: Env -> LispVal -> LispVal -> Maybe [LispVal] -> IOThrowsError LispVal
        cpsAltEvaled _ c test (Just [expr]) = apply c expr [test]
        cpsAltEvaled _ _ _ _ = throwError $ Default "Unexpected error in cond"

eval env cont (List (Atom "begin" : funcs)) = 
  if length funcs == 0
     then eval env cont $ Nil ""
     else if length funcs == 1
             then eval env cont (head funcs)
             else eval env (makeCPSWArgs env cont cpsRest $ tail funcs) (head funcs)
  where cpsRest :: Env -> LispVal -> LispVal -> Maybe [LispVal] -> IOThrowsError LispVal
        cpsRest e c _ args = 
          case args of
            Just fArgs -> eval e c $ List (Atom "begin" : fArgs)
            Nothing -> throwError $ Default "Unexpected error in begin"

eval env cont (List [Atom "set!", Atom var, form]) = do 
  eval env (makeCPS env cont cpsResult) form
 where cpsResult :: Env -> LispVal -> LispVal -> Maybe [LispVal] -> IOThrowsError LispVal
       cpsResult e c result _ = setVar e var result >>= continueEval e c
eval _ _ (List [Atom "set!", nonvar, _]) = throwError $ TypeMismatch "variable" nonvar 
eval _ _ (List (Atom "set!" : args)) = throwError $ NumArgs 2 args

eval env cont (List [Atom "define", Atom var, form]) = do 
  eval env (makeCPS env cont cpsResult) form
 where cpsResult :: Env -> LispVal -> LispVal -> Maybe [LispVal] -> IOThrowsError LispVal
       cpsResult e c result _ = defineVar e var result >>= continueEval e c

eval env cont (List (Atom "define" : List (Atom var : fparams) : fbody )) = do
  result <- (makeNormalFunc env fparams fbody >>= defineVar env var)
  continueEval env cont result

eval env cont (List (Atom "define" : DottedList (Atom var : fparams) varargs : fbody)) = do
  result <- (makeVarargs varargs env fparams fbody >>= defineVar env var)
  continueEval env cont result

eval env cont (List (Atom "lambda" : List fparams : fbody)) = do
  result <- makeNormalFunc env fparams fbody
  continueEval env cont result

eval env cont (List (Atom "lambda" : DottedList fparams varargs : fbody)) = do
  result <- makeVarargs varargs env fparams fbody
  continueEval env cont result

eval env cont (List (Atom "lambda" : varargs@(Atom _) : fbody)) = do
  result <- makeVarargs varargs env [] fbody
  continueEval env cont result

eval env cont (List [Atom "string-set!", Atom var, i, character]) = do 
  eval env (makeCPS env cont cpsStr) i
  where 
        cpsStr :: Env -> LispVal -> LispVal -> Maybe [LispVal] -> IOThrowsError LispVal
        cpsStr e c idx _ = eval e (makeCPSWArgs e c cpsSubStr $ [idx]) =<< getVar e var

        cpsSubStr :: Env -> LispVal -> LispVal -> Maybe [LispVal] -> IOThrowsError LispVal
        cpsSubStr e c str (Just [idx]) = 
            substr(str, character, idx) >>= setVar e var >>= continueEval e c
        cpsSubStr _ _ _ _ = throwError $ InternalError "Invalid argument to cpsSubStr" 

        substr (String str, Char char, Number ii) = do
                              return $ String $ (take (fromInteger ii) . drop 0) str ++ 
                                       [char] ++
                                       (take (length str) . drop (fromInteger ii + 1)) str
        substr (String _, Char _, n) = throwError $ TypeMismatch "number" n
        substr (String _, c, _) = throwError $ TypeMismatch "character" c
        substr (s, _, _) = throwError $ TypeMismatch "string" s
eval _ _ (List [Atom "string-set!" , nonvar , _ , _ ]) = throwError $ TypeMismatch "variable" nonvar 
eval _ _ (List (Atom "string-set!" : args)) = throwError $ NumArgs 3 args

eval env cont (List [Atom "set-car!", Atom var, argObj]) = do
  continueEval env (makeCPS env cont cpsObj) =<< getVar env var
  where 
        cpsObj :: Env -> LispVal -> LispVal -> Maybe [LispVal] -> IOThrowsError LispVal
        cpsObj _ _ obj@(List []) _ = throwError $ TypeMismatch "pair" obj
        cpsObj e c obj@(List (_:_)) _ = eval e (makeCPSWArgs e c cpsSet $ [obj]) argObj
        cpsObj e c obj@(DottedList _ _) _ = eval e (makeCPSWArgs e c cpsSet $ [obj]) argObj
        cpsObj _ _ obj _ = throwError $ TypeMismatch "pair" obj 

        cpsSet :: Env -> LispVal -> LispVal -> Maybe [LispVal] -> IOThrowsError LispVal
        cpsSet e c obj (Just [List (_ : ls)]) = setVar e var (List (obj : ls)) >>= continueEval e c -- Wrong constructor? Should it be DottedList?
        cpsSet e c obj (Just [DottedList (_ : ls) l]) = setVar e var (DottedList (obj : ls) l) >>= continueEval e c
        cpsSet _ _ _ _ = throwError $ InternalError "Unexpected argument to cpsSet" 
eval _ _ (List [Atom "set-car!" , nonvar , _ ]) = throwError $ TypeMismatch "variable" nonvar 
eval _ _ (List (Atom "set-car!" : args)) = throwError $ NumArgs 2 args

eval env cont (List [Atom "set-cdr!", Atom var, argObj]) = do
  continueEval env (makeCPS env cont cpsObj) =<< getVar env var
  where 
        cpsObj :: Env -> LispVal -> LispVal -> Maybe [LispVal] -> IOThrowsError LispVal
        cpsObj _ _ pair@(List []) _ = throwError $ TypeMismatch "pair" pair 
        cpsObj e c pair@(List (_:_)) _ = eval e (makeCPSWArgs e c cpsSet $ [pair]) argObj
        cpsObj e c pair@(DottedList _ _) _ = eval e (makeCPSWArgs e c cpsSet $ [pair]) argObj
        cpsObj _ _ pair _ = throwError $ TypeMismatch "pair" pair 

        cpsSet :: Env -> LispVal -> LispVal -> Maybe [LispVal] -> IOThrowsError LispVal
        cpsSet e c obj (Just [List (l : _)]) = setVar e var (DottedList [l] obj) >>= continueEval e c
        cpsSet e c obj (Just [DottedList (l : _) _]) = setVar e var (DottedList [l] obj) >>= continueEval e c
        cpsSet _ _ _ _ = throwError $ InternalError "Unexpected argument to cpsSet" 
eval _ _ (List [Atom "set-cdr!" , nonvar , _ ]) = throwError $ TypeMismatch "variable" nonvar 
eval _ _ (List (Atom "set-cdr!" : args)) = throwError $ NumArgs 2 args

eval env cont (List [Atom "vector-set!", Atom var, i, object]) = do 
  eval env (makeCPS env cont cpsObj) i
  where
        cpsObj :: Env -> LispVal -> LispVal -> Maybe [LispVal] -> IOThrowsError LispVal
        cpsObj e c idx _ = eval e (makeCPSWArgs e c cpsVec $ [idx]) object

        cpsVec :: Env -> LispVal -> LispVal -> Maybe [LispVal] -> IOThrowsError LispVal
        cpsVec e c obj (Just [idx]) = eval e (makeCPSWArgs e c cpsUpdateVec $ [idx, obj]) =<< getVar e var
        cpsVec _ _ _ _ = throwError $ InternalError "Invalid argument to cpsVec"

        cpsUpdateVec :: Env -> LispVal -> LispVal -> Maybe [LispVal] -> IOThrowsError LispVal
        cpsUpdateVec e c vec (Just [idx, obj]) = 
            updateVector vec idx obj >>= setVar e var >>= continueEval e c
        cpsUpdateVec _ _ _ _ = throwError $ InternalError "Invalid argument to cpsUpdateVec"

        updateVector :: LispVal -> LispVal -> LispVal -> IOThrowsError LispVal
        updateVector (Vector vec) (Number idx) obj = return $ Vector $ vec//[(fromInteger idx, obj)]
        updateVector v _ _ = throwError $ TypeMismatch "vector" v
eval _ _ (List [Atom "vector-set!" , nonvar , _ , _]) = throwError $ TypeMismatch "variable" nonvar 
eval _ _ (List (Atom "vector-set!" : args)) = throwError $ NumArgs 3 args

eval env cont (List [Atom "hash-table-set!", Atom var, rkey, rvalue]) = do 
  eval env (makeCPS env cont cpsValue) rkey
  where
        cpsValue :: Env -> LispVal -> LispVal -> Maybe [LispVal] -> IOThrowsError LispVal
        cpsValue e c key _ = eval e (makeCPSWArgs e c cpsH $ [key]) rvalue
        
        cpsH :: Env -> LispVal -> LispVal -> Maybe [LispVal] -> IOThrowsError LispVal
        cpsH e c value (Just [key]) = eval e (makeCPSWArgs e c cpsEvalH $ [key, value]) =<< getVar e var
        cpsH _ _ _ _ = throwError $ InternalError "Invalid argument to cpsH" 

        cpsEvalH :: Env -> LispVal -> LispVal -> Maybe [LispVal] -> IOThrowsError LispVal
        cpsEvalH e c h (Just [key, value]) = do 
            case h of
                HashTable ht -> do
                  setVar env var (HashTable $ Data.Map.insert key value ht) >>= eval e c
                other -> throwError $ TypeMismatch "hash-table" other
        cpsEvalH _ _ _ _ = throwError $ InternalError "Invalid argument to cpsEvalH"
eval _ _ (List [Atom "hash-table-set!" , nonvar , _ , _]) = throwError $ TypeMismatch "variable" nonvar 
eval _ _ (List (Atom "hash-table-set!" : args)) = throwError $ NumArgs 3 args

eval env cont (List [Atom "hash-table-delete!", Atom var, rkey]) = do 
  eval env (makeCPS env cont cpsH) rkey
  where
        cpsH :: Env -> LispVal -> LispVal -> Maybe [LispVal] -> IOThrowsError LispVal
        cpsH e c key _ = eval e (makeCPSWArgs e c cpsEvalH $ [key]) =<< getVar e var

        cpsEvalH :: Env -> LispVal -> LispVal -> Maybe [LispVal] -> IOThrowsError LispVal
        cpsEvalH e c h (Just [key]) = do 
            case h of
                HashTable ht -> do
                  setVar env var (HashTable $ Data.Map.delete key ht) >>= eval e c
                other -> throwError $ TypeMismatch "hash-table" other
        cpsEvalH _ _ _ _ = throwError $ InternalError "Invalid argument to cpsEvalH"
eval _ _ (List [Atom "hash-table-delete!" , nonvar , _]) = throwError $ TypeMismatch "variable" nonvar 
eval _ _ (List (Atom "hash-table-delete!" : args)) = throwError $ NumArgs 2 args

-- Call a function by evaluating its arguments and then 
-- executing it via 'apply'.
eval env cont (List (function : functionArgs)) = do 
  eval env (makeCPSWArgs env cont cpsPrepArgs $ functionArgs) function
 where cpsPrepArgs :: Env -> LispVal -> LispVal -> Maybe [LispVal] -> IOThrowsError LispVal
       cpsPrepArgs e c func (Just args) = 
--          case (trace ("prep eval of args: " ++ show args) args) of
          case (args) of
            [] -> apply c func [] -- No args, immediately apply the function
            [a] -> eval env (makeCPSWArgs e c cpsEvalArgs $ [func, List [], List []]) a
            (a:as) -> eval env (makeCPSWArgs e c cpsEvalArgs $ [func, List [], List as]) a
       cpsPrepArgs _ _ _ Nothing = throwError $ Default "Unexpected error in function application (1)"
        -- Store value of previous argument, evaluate the next arg until all are done
        -- parg - Previous argument that has now been evaluated
        -- state - List containing the following, in order:
        --         - Function to apply when args are ready
        --         - List of evaluated parameters
        --         - List of parameters awaiting evaluation
       cpsEvalArgs :: Env -> LispVal -> LispVal -> Maybe [LispVal] -> IOThrowsError LispVal
       cpsEvalArgs e c evaledArg (Just [func, List argsEvaled, List argsRemaining]) = 
          case argsRemaining of
            [] -> apply c func (argsEvaled ++ [evaledArg])
            [a] -> eval e (makeCPSWArgs e c cpsEvalArgs $ [func, List (argsEvaled ++ [evaledArg]), List []]) a
            (a:as) -> eval e (makeCPSWArgs e c cpsEvalArgs $ [func, List (argsEvaled ++ [evaledArg]), List as]) a

       cpsEvalArgs _ _ _ (Just _) = throwError $ Default "Unexpected error in function application (1)"
       cpsEvalArgs _ _ _ Nothing = throwError $ Default "Unexpected error in function application (2)"

eval _ _ badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

makeFunc :: --forall (m :: * -> *).
            (Monad m) =>
            Maybe String -> Env -> [LispVal] -> [LispVal] -> m LispVal
makeFunc varargs env fparams fbody = return $ Func (map showVal fparams) varargs fbody env
makeNormalFunc :: (Monad m) => Env
               -> [LispVal]
               -> [LispVal]
               -> m LispVal
makeNormalFunc = makeFunc Nothing
makeVarargs :: (Monad m) => LispVal  -> Env
                        -> [LispVal]
                        -> [LispVal]
                        -> m LispVal
makeVarargs = makeFunc . Just . showVal

-- Call into a Scheme function
apply :: LispVal -> LispVal -> [LispVal] -> IOThrowsError LispVal
apply _ cont@(Continuation env ccont ncont _ ndynwind) args = do
{-  case ndynwind of
    -- Call into dynWind.before if it exists...
    Just (DynamicWind [beforeFunc]) -> apply (makeCPS env cont cpsApply) beforeFunc []
    _ -> -} doApply env cont
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
  result <- func args
  case cont of
    Continuation cEnv _ _ _ _ -> continueEval cEnv cont result
    _ -> return result
apply cont (EvalFunc func) args = do
    -- An EvalFunc extends the evaluator so it needs access to the current continuation;
    -- pass it as the first argument.
    func (cont : args)
apply cont (PrimitiveFunc func) args = do
  result <- liftThrows $ func args
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
        -- This link was helpful for implementing this, and has a *lot* of other useful information:
        -- http://icem-www.folkwang-hochschule.de/~finnendahl/cm_kurse/doc/schintro/schintro_73.html#SEC80
        --
        -- What we are doing now is simply not saving a continuation for tail calls. For now this may
        -- be good enough, although it may need to be enhanced in the future in order to properly
        -- detect all tail calls. 
        --
        -- See: http://icem-www.folkwang-hochschule.de/~finnendahl/cm_kurse/doc/schintro/schintro_142.html#SEC294
        --
        evalBody evBody env = case cont of
            Continuation _ (Just (SchemeBody cBody)) (Just cCont) _ cDynWind -> if length cBody == 0
                then continueWCont env (evBody) cCont cDynWind
                else continueWCont env (evBody) cont cDynWind -- Might be a problem, not fully optimizing
            _ -> continueWCont env (evBody) cont Nothing

        -- Shortcut for calling continueEval
        continueWCont cwcEnv cwcBody cwcCont cwcDynWind = 
            continueEval cwcEnv (Continuation cwcEnv (Just (SchemeBody cwcBody)) (Just cwcCont) Nothing cwcDynWind) $ Nil ""

        bindVarArgs arg env = case arg of
          Just argName -> liftIO $ extendEnv env [((varNamespace, argName), List $ remainingArgs)]
          Nothing -> return env
apply _ func args = throwError $ BadSpecialForm "Unable to evaluate form" $ List (func : args)

-- |Environment containing the primitive forms that are built into the Scheme language. Note that this only includes
--  forms that are implemented in Haskell; derived forms implemented in Scheme (such as let, list, etc) are available
--  in the standard library which must be pulled into the environment using (load).
primitiveBindings :: IO Env
primitiveBindings = nullEnv >>= (flip extendEnv $ map (domakeFunc IOFunc) ioPrimitives
                                               ++ map (domakeFunc EvalFunc) evalFunctions
                                               ++ map (domakeFunc PrimitiveFunc) primitives)
  where domakeFunc constructor (var, func) = ((varNamespace, var), constructor func)

-- Functions that extend the core evaluator, but that can be defined separately.
--
-- These functions have access to the current environment via the
-- current continuation, which is passed as the first LispVal argument.
--
evalFunctions :: [(String, [LispVal] -> IOThrowsError LispVal)]
evalFunctions = [
                    ("apply", evalfuncApply)
                  , ("call-with-current-continuation", evalfuncCallCC)
                  , ("call-with-values", evalfuncCallWValues)
                  , ("dynamic-wind", evalfuncDynamicWind)
                  , ("eval", evalfuncEval)
                  , ("load", evalfuncLoad)
                ]
evalfuncApply, evalfuncDynamicWind, evalfuncEval, evalfuncLoad, evalfuncCallCC, evalfuncCallWValues :: [LispVal] -> IOThrowsError LispVal

-- A simplified version of dynamic-wind that was not (immediately) intended to take all rules into account
--
-- The implementation must take these 4 rules into account:
--
-- 1) The dynamic extent is entered when execution of the body of the called procedure begins.
-- 2) The dynamic extent is also entered when execution is not within the dynamic extent and a continuation is invoked that was captured (using call-with-current-continuation) during the dynamic extent.
-- 3) It is exited when the called procedure returns.
-- 4) It is also exited when execution is within the dynamic extent and a continuation is invoked that was captured while not within the dynamic extent.
--
-- Basically (before) must be called either when thunk is called into, or when a continuation captured during (thunk) is called into.
-- And (after) must be called either when thunk returns *or* a continuation is called into during (thunk)
--
-- TODO: so far only (1) and (3) are handled.
--
evalfuncDynamicWind [cont@(Continuation env _ _ _ _), beforeFunc, thunkFunc, afterFunc] = do 
  apply (makeCPS env cont cpsThunk) beforeFunc []
 where
   cpsThunk, cpsAfter :: Env -> LispVal -> LispVal -> Maybe [LispVal] -> IOThrowsError LispVal
   cpsThunk e c _ _ = apply (Continuation e (Just (HaskellBody cpsAfter Nothing)) 
                                            (Just c) 
                                             Nothing 
                                            (Just ([DynamicWinders beforeFunc afterFunc])))
                               thunkFunc []
   cpsAfter _ c _ _ = apply c afterFunc []
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

evalfuncApply [cont@(Continuation _ _ _ _ _), func, List args] = apply cont func args
evalfuncApply (_ : args) = throwError $ NumArgs 2 args -- Skip over continuation argument
evalfuncApply _ = throwError $ NumArgs 2 []

evalfuncLoad [cont@(Continuation env _ _ _ _), String filename] = do
     result <- load filename >>= liftM last . mapM (evaluate env (makeNullContinuation env))
     continueEval env cont result
	 where evaluate env2 cont2 val2 = macroEval env2 val2 >>= eval env2 cont2
evalfuncLoad (_ : args) = throwError $ NumArgs 1 args -- Skip over continuation argument
evalfuncLoad _ = throwError $ NumArgs 1 []

-- Evaluate an expression in the current environment
--
-- Assumption is any macro transform is already performed
-- prior to this step.
--
-- FUTURE: consider allowing env to be specified, per R5RS
--
evalfuncEval [cont@(Continuation env _ _ _ _), val] = eval env cont val
evalfuncEval (_ : args) = throwError $ NumArgs 1 args -- Skip over continuation argument
evalfuncEval _ = throwError $ NumArgs 1 []

evalfuncCallCC [cont@(Continuation _ _ _ _ _), func] = do
   case func of
     PrimitiveFunc f -> do
         result <- liftThrows $ f [cont]
         case cont of 
             Continuation cEnv _ _ _ _ -> continueEval cEnv cont result
             _ -> return result
     Func aparams _ _ _ ->
       if (toInteger $ length aparams) == 1 
         then apply cont func [cont] 
         else throwError $ NumArgs (toInteger $ length aparams) [cont] 
     other -> throwError $ TypeMismatch "procedure" other
evalfuncCallCC (_ : args) = throwError $ NumArgs 1 args -- Skip over continuation argument
evalfuncCallCC _ = throwError $ NumArgs 1 []

-- I/O primitives
-- Primitive functions that execute within the IO monad
ioPrimitives :: [(String, [LispVal] -> IOThrowsError LispVal)]
ioPrimitives = [("open-input-file", makePort ReadMode),
                ("open-output-file", makePort WriteMode),
                ("close-input-port", closePort),
                ("close-output-port", closePort),
                ("input-port?", isInputPort),
                ("output-port?", isOutputPort),

               -- The following optional procedures are NOT implemented:
               -- 
               --  with-input-from-file
               --  with-output-from-file
               --  transcript-on
               --  transcript-off
               --
               --  Consideration may be given in a future release, but keep in mind
               --  the impact to the other I/O functions.

-- TODO: not currently supported: char-ready?

                ("current-input-port", currentInputPort),
                ("current-output-port", currentOutputPort),
                ("read", readProc),
                ("read-char", readCharProc hGetChar),
                ("peek-char", readCharProc hLookAhead),
                ("write", writeProc (\port obj -> hPrint port obj)),
                ("write-char", writeCharProc),
                ("display", writeProc (\port obj -> case obj of
                                                        String str -> hPutStr port str
                                                        _ -> hPutStr port $ show obj)),
                ("read-contents", readContents),
                ("read-all", readAll)]

makePort :: IOMode -> [LispVal] -> IOThrowsError LispVal
makePort mode [String filename] = liftM Port $ liftIO $ openFile filename mode
makePort _ [] = throwError $ NumArgs 1 []
makePort _ args@(_ : _) = throwError $ NumArgs 1 args

closePort :: [LispVal] -> IOThrowsError LispVal
closePort [Port port] = liftIO $ hClose port >> (return $ Bool True)
closePort _ = return $ Bool False

currentInputPort, currentOutputPort :: [LispVal] -> IOThrowsError LispVal
-- FUTURE: For now, these are just hardcoded to the standard i/o ports.
--         a future implementation that includes with-*put-from-file
--         would require a more involved implementation here as well as
--         other I/O functions hooking into these instead of std*
currentInputPort _ = return $ Port stdin
currentOutputPort _ = return $ Port stdout

isInputPort, isOutputPort :: [LispVal] -> IOThrowsError LispVal
isInputPort [Port port] = liftM Bool $ liftIO $ hIsReadable port
isInputPort _ = return $ Bool False

isOutputPort [Port port] = liftM Bool $ liftIO $ hIsWritable port
isOutputPort _ = return $ Bool False

readProc :: [LispVal] -> IOThrowsError LispVal
readProc [] = readProc [Port stdin]
readProc [Port port] = do
    input <-  liftIO $ try (liftIO $ hGetLine port)
    case input of
        Left e -> if isEOFError e
                     then return $ EOF
                     else throwError $ Default "I/O error reading from port" -- FUTURE: ioError e
        Right inpStr -> do 
            liftThrows $ readExpr inpStr 
readProc args@(_ : _) = throwError $ BadSpecialForm "" $ List args

readCharProc :: (Handle -> IO Char) -> [LispVal] -> IOThrowsError LispVal
readCharProc func [] = readCharProc func [Port stdin]
readCharProc func [Port port] = do
    liftIO $ hSetBuffering port NoBuffering
    input <-  liftIO $ try (liftIO $ func port)
    liftIO $ hSetBuffering port LineBuffering
    case input of
        Left e -> if isEOFError e
                     then return $ EOF
                     else throwError $ Default "I/O error reading from port"
        Right inpChr -> do 
            return $ Char inpChr 
readCharProc _ args@(_ : _) = throwError $ BadSpecialForm "" $ List args

{-writeProc :: --forall a (m :: * -> *).
             (MonadIO m, MonadError LispError m) =>
             (Handle -> LispVal -> IO a) -> [LispVal] -> m LispVal -}
writeProc func [obj] = writeProc func [obj, Port stdout]
writeProc func [obj, Port port] = do
    output <- liftIO $ try (liftIO $ func port obj)
    case output of
        Left _ -> throwError $ Default "I/O error writing to port"
        Right _ -> return $ Nil ""
writeProc _ other = if length other == 2
                     then throwError $ TypeMismatch "(value port)" $ List other 
                     else throwError $ NumArgs 2 other

writeCharProc :: [LispVal] -> IOThrowsError LispVal
writeCharProc [obj] = writeCharProc [obj, Port stdout]
writeCharProc [obj@(Char _), Port port] = do
    output <- liftIO $ try (liftIO $ (hPutStr port $ show obj))
    case output of
        Left _ -> throwError $ Default "I/O error writing to port"
        Right _ -> return $ Nil ""
writeCharProc other = if length other == 2
                     then throwError $ TypeMismatch "(character port)" $ List other 
                     else throwError $ NumArgs 2 other

readContents :: [LispVal] -> IOThrowsError LispVal
readContents [String filename] = liftM String $ liftIO $ readFile filename
readContents [] = throwError $ NumArgs 1 []
readContents args@(_ : _) = throwError $ NumArgs 1 args

load :: String -> IOThrowsError [LispVal]
load filename = do
  result <- liftIO $ doesFileExist filename
  if result
     then (liftIO $ readFile filename) >>= liftThrows . readExprList
     else throwError $ Default $ "File does not exist: " ++ filename

readAll :: [LispVal] -> IOThrowsError LispVal
readAll [String filename] = liftM List $ load filename
readAll [] = throwError $ NumArgs 1 []
readAll args@(_ : _) = throwError $ NumArgs 1 args

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [("+", numAdd),
              ("-", numSub),
              ("*", numMul),
              ("/", numDiv),
              ("modulo", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem),

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

data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsError a)

unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals arg1 arg2 (AnyUnpacker unpacker) = 
  do unpacked1 <- unpacker arg1
     unpacked2 <- unpacker arg2
     return $ unpacked1 == unpacked2
  `catchError` (const $ return False)

boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op args = if length args /= 2
                             then throwError $ NumArgs 2 args
                             else do left <- unpacker $ args !! 0
                                     right <- unpacker $ args !! 1
                                     return $ Bool $ left `op` right

unaryOp :: (LispVal -> ThrowsError LispVal) -> [LispVal] -> ThrowsError LispVal
unaryOp f [v] = f v
unaryOp _ [] = throwError $ NumArgs 1 []
unaryOp _ args@(_ : _) = throwError $ NumArgs 1 args

--numBoolBinop :: (Integer -> Integer -> Bool) -> [LispVal] -> ThrowsError LispVal
--numBoolBinop = boolBinop unpackNum
strBoolBinop :: (String -> String -> Bool) -> [LispVal] -> ThrowsError LispVal
strBoolBinop = boolBinop unpackStr
boolBoolBinop :: (Bool -> Bool -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBoolBinop = boolBinop unpackBool

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Number s) = return $ show s
unpackStr (Bool s) = return $ show s
unpackStr notString = throwError $ TypeMismatch "string" notString

unpackBool :: LispVal -> ThrowsError Bool
unpackBool  (Bool b) = return b
unpackBool notBool = throwError $ TypeMismatch "boolean" notBool

{- List primitives -}
car :: [LispVal] -> ThrowsError LispVal
car [List (x : _)] = return x
car [DottedList (x : _) _] = return x
car [badArg] = throwError $ TypeMismatch "pair" badArg
car badArgList = throwError $ NumArgs 1 badArgList

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (_ : xs)] = return $ List xs
cdr [DottedList [_] x] = return x
cdr [DottedList (_ : xs) x] = return $ DottedList xs x
cdr [badArg] = throwError $ TypeMismatch "pair" badArg
cdr badArgList = throwError $ NumArgs 1 badArgList

cons :: [LispVal] -> ThrowsError LispVal
cons [x1, List []] = return $ List [x1]
cons [x, List xs] = return $ List $ x : xs
cons [x, DottedList xs xlast] = return $ DottedList (x : xs) xlast
cons [x1, x2] = return $ DottedList [x1] x2
cons badArgList = throwError $ NumArgs 2 badArgList

equal :: [LispVal] -> ThrowsError LispVal
equal [(Vector arg1), (Vector arg2)] = eqvList equal [List $ (elems arg1), List $ (elems arg2)] 
equal [l1@(List _), l2@(List _)] = eqvList equal [l1, l2]
equal [(DottedList xs x), (DottedList ys y)] = equal [List $ xs ++ [x], List $ ys ++ [y]]
equal [arg1, arg2] = do
  primitiveEquals <- liftM or $ mapM (unpackEquals arg1 arg2)
                     [AnyUnpacker unpackNum, AnyUnpacker unpackStr, AnyUnpacker unpackBool]
  eqvEquals <- eqv [arg1, arg2]
  return $ Bool $ (primitiveEquals || let (Bool x) = eqvEquals in x)
equal badArgList = throwError $ NumArgs 2 badArgList

-------------- Vector Primitives --------------

makeVector, buildVector, vectorLength, vectorRef, vectorToList, listToVector :: [LispVal] -> ThrowsError LispVal
makeVector [(Number n)] = makeVector [Number n, List []]
makeVector [(Number n), a] = do
  let l = replicate (fromInteger n) a 
  return $ Vector $ (listArray (0, length l - 1)) l
makeVector [badType] = throwError $ TypeMismatch "integer" badType 
makeVector badArgList = throwError $ NumArgs 1 badArgList

buildVector (o:os) = do
  let lst = o:os
  return $ Vector $ (listArray (0, length lst - 1)) lst
buildVector badArgList = throwError $ NumArgs 1 badArgList

vectorLength [(Vector v)] = return $ Number $ toInteger $ length (elems v)
vectorLength [badType] = throwError $ TypeMismatch "vector" badType 
vectorLength badArgList = throwError $ NumArgs 1 badArgList

vectorRef [(Vector v), (Number n)] = return $ v ! (fromInteger n)
vectorRef [badType] = throwError $ TypeMismatch "vector integer" badType 
vectorRef badArgList = throwError $ NumArgs 2 badArgList

vectorToList [(Vector v)] = return $ List $ elems v 
vectorToList [badType] = throwError $ TypeMismatch "vector" badType 
vectorToList badArgList = throwError $ NumArgs 1 badArgList

listToVector [(List l)] = return $ Vector $ (listArray (0, length l - 1)) l
listToVector [badType] = throwError $ TypeMismatch "list" badType 
listToVector badArgList = throwError $ NumArgs 1 badArgList

-------------- Hash Table Primitives --------------

-- Future: support (equal?), (hash) parameters
hashTblMake, isHashTbl, hashTblExists, hashTblRef, hashTblSize, hashTbl2List, hashTblKeys, hashTblValues, hashTblCopy:: [LispVal] -> ThrowsError LispVal
hashTblMake _ = return $ HashTable $ Data.Map.fromList []

isHashTbl [(HashTable _)] = return $ Bool True
isHashTbl _             = return $ Bool False

hashTblExists [(HashTable ht), key@(_)] = do
  case Data.Map.lookup key ht of
    Just _ -> return $ Bool True
    Nothing -> return $ Bool False
hashTblExists [] = throwError $ NumArgs 2 []
hashTblExists args@(_ : _) = throwError $ NumArgs 2 args

hashTblRef [(HashTable ht), key@(_)] = do
  case Data.Map.lookup key ht of
    Just val -> return $ val
    Nothing -> throwError $ BadSpecialForm "Hash table does not contain key" key
hashTblRef [(HashTable ht), key@(_), Func _ _ _ _] = do 
  case Data.Map.lookup key ht of
    Just val -> return $ val
    Nothing -> throwError $ NotImplemented "thunk"
-- FUTURE: a thunk can optionally be specified, this drives definition of /default
--         Nothing -> apply thunk []
hashTblRef [badType] = throwError $ TypeMismatch "hash-table" badType
hashTblRef badArgList = throwError $ NumArgs 2 badArgList

hashTblSize [(HashTable ht)] = return $ Number $ toInteger $ Data.Map.size ht
hashTblSize [badType] = throwError $ TypeMismatch "hash-table" badType
hashTblSize badArgList = throwError $ NumArgs 1 badArgList

hashTbl2List [(HashTable ht)] = do
  return $ List $ map (\(k, v) -> List [k, v]) $ Data.Map.toList ht
hashTbl2List [badType] = throwError $ TypeMismatch "hash-table" badType
hashTbl2List badArgList = throwError $ NumArgs 1 badArgList

hashTblKeys [(HashTable ht)] = do
  return $ List $ map (\(k, _) -> k) $ Data.Map.toList ht
hashTblKeys [badType] = throwError $ TypeMismatch "hash-table" badType
hashTblKeys badArgList = throwError $ NumArgs 1 badArgList

hashTblValues [(HashTable ht)] = do
  return $ List $ map (\(_, v) -> v) $ Data.Map.toList ht
hashTblValues [badType] = throwError $ TypeMismatch "hash-table" badType
hashTblValues badArgList = throwError $ NumArgs 1 badArgList

hashTblCopy [(HashTable ht)] = do
  return $ HashTable $ Data.Map.fromList $ Data.Map.toList ht
hashTblCopy [badType] = throwError $ TypeMismatch "hash-table" badType
hashTblCopy badArgList = throwError $ NumArgs 1 badArgList

-------------- String Primitives --------------

buildString :: [LispVal] -> ThrowsError LispVal
buildString [(Char c)] = return $ String [c]
buildString (Char c:rest) = do
  cs <- buildString rest
  case cs of
    String s -> return $ String $ [c] ++ s
    badType -> throwError $ TypeMismatch "character" badType
buildString [badType] = throwError $ TypeMismatch "character" badType
buildString badArgList = throwError $ NumArgs 1 badArgList

makeString :: [LispVal] -> ThrowsError LispVal
makeString [(Number n)] = return $ doMakeString n ' ' ""
makeString [(Number n), (Char c)] = return $ doMakeString n c ""
makeString badArgList = throwError $ NumArgs 1 badArgList

doMakeString :: forall a.(Num a) => a -> Char -> String -> LispVal
doMakeString n char s = 
    if n == 0 
       then String s
       else doMakeString (n - 1) char (s ++ [char])

stringLength :: [LispVal] -> ThrowsError LispVal
stringLength [String s] = return $ Number $ foldr (const (+1)) 0 s -- Could probably do 'length s' instead...
stringLength [badType] = throwError $ TypeMismatch "string" badType
stringLength badArgList = throwError $ NumArgs 1 badArgList

stringRef :: [LispVal] -> ThrowsError LispVal
stringRef [(String s), (Number k)] = return $ Char $ s !! fromInteger k
stringRef [badType] = throwError $ TypeMismatch "string number" badType
stringRef badArgList = throwError $ NumArgs 2 badArgList

substring :: [LispVal] -> ThrowsError LispVal
substring [(String s), (Number start), (Number end)] = 
  do let slength = fromInteger $ end - start
     let begin = fromInteger start 
     return $ String $ (take slength . drop begin) s
substring [badType] = throwError $ TypeMismatch "string number number" badType
substring badArgList = throwError $ NumArgs 3 badArgList

stringCIEquals :: [LispVal] -> ThrowsError LispVal
stringCIEquals [(String str1), (String str2)] = do
  if (length str1) /= (length str2)
     then return $ Bool False
     else return $ Bool $ ciCmp str1 str2 0
  where ciCmp s1 s2 idx = if idx == (length s1)
                             then True
                             else if (toLower $ s1 !! idx) == (toLower $ s2 !! idx)
                                     then ciCmp s1 s2 (idx + 1)
                                     else False
stringCIEquals [badType] = throwError $ TypeMismatch "string string" badType
stringCIEquals badArgList = throwError $ NumArgs 2 badArgList

stringCIBoolBinop :: ([Char] -> [Char] -> Bool) -> [LispVal] -> ThrowsError LispVal
stringCIBoolBinop op [(String s1), (String s2)] = boolBinop unpackStr op [(String $ strToLower s1), (String $ strToLower s2)]
  where strToLower str = map (toLower) str 
stringCIBoolBinop _ [badType] = throwError $ TypeMismatch "string string" badType
stringCIBoolBinop _ badArgList = throwError $ NumArgs 2 badArgList

stringAppend :: [LispVal] -> ThrowsError LispVal
stringAppend [(String s)] = return $ String s -- Needed for "last" string value
stringAppend (String st:sts) = do
  rest <- stringAppend sts
  case rest of
    String s -> return $ String $ st ++ s
    other -> throwError $ TypeMismatch "string" other
stringAppend [badType] = throwError $ TypeMismatch "string" badType
stringAppend badArgList = throwError $ NumArgs 1 badArgList

stringToNumber :: [LispVal] -> ThrowsError LispVal
stringToNumber [(String s)] = do
  result <- (readExpr s)
  case result of
    n@(Number _) -> return n
    n@(Rational _) -> return n
    n@(Float _) -> return n
    n@(Complex _) -> return n
    _ -> return $ Bool False
stringToNumber [(String s), Number radix] = do
  case radix of
    2  -> stringToNumber [String $ "#b" ++ s]
    8  -> stringToNumber [String $ "#o" ++ s]
    10 -> stringToNumber [String s]
    16 -> stringToNumber [String $ "#x" ++ s]
    _  -> throwError $ Default $ "Invalid radix: " ++ show radix 
stringToNumber [badType] = throwError $ TypeMismatch "string" badType
stringToNumber badArgList = throwError $ NumArgs 1 badArgList

stringToList :: [LispVal] -> ThrowsError LispVal
stringToList [(String s)] = return $ List $ map (Char) s
stringToList [badType] = throwError $ TypeMismatch "string" badType
stringToList badArgList = throwError $ NumArgs 1 badArgList

listToString :: [LispVal] -> ThrowsError LispVal
listToString [(List [])] = return $ String ""
listToString [(List l)] = buildString l
listToString [badType] = throwError $ TypeMismatch "list" badType
listToString [] = throwError $ NumArgs 1 []
listToString args@(_ : _) = throwError $ NumArgs 1 args

stringCopy :: [LispVal] -> ThrowsError LispVal
stringCopy [String s] = return $ String s
stringCopy [badType] = throwError $ TypeMismatch "string" badType
stringCopy badArgList = throwError $ NumArgs 2 badArgList

isDottedList :: [LispVal] -> ThrowsError LispVal
isDottedList ([DottedList _ _]) = return $ Bool True
-- Must include lists as well since they are made up of 'chains' of pairs
isDottedList ([List []]) = return $ Bool False
isDottedList ([List _]) = return $ Bool True
isDottedList _ = return $  Bool False

isProcedure :: [LispVal] -> ThrowsError LispVal
isProcedure ([Continuation _ _ _ _ _]) = return $ Bool True
isProcedure ([PrimitiveFunc _]) = return $ Bool True
isProcedure ([Func _ _ _ _]) = return $ Bool True
isProcedure ([IOFunc _]) = return $ Bool True
isProcedure ([EvalFunc _]) = return $ Bool True
isProcedure _ = return $ Bool False

isVector, isList :: LispVal -> ThrowsError LispVal
isVector (Vector _) = return $ Bool True
isVector _          = return $ Bool False
isList (List _) = return $ Bool True
isList _        = return $ Bool False

isNull :: [LispVal] -> ThrowsError LispVal
isNull ([List []]) = return $ Bool True
isNull _ = return $ Bool False

isEOFObject :: [LispVal] -> ThrowsError LispVal
isEOFObject ([EOF]) = return $ Bool True
isEOFObject _ = return $ Bool False

isSymbol :: [LispVal] -> ThrowsError LispVal
isSymbol ([Atom _]) = return $ Bool True
isSymbol _ = return $ Bool False

symbol2String :: [LispVal] -> ThrowsError LispVal
symbol2String ([Atom a]) = return $ String a
symbol2String [notAtom] = throwError $ TypeMismatch "symbol" notAtom
symbol2String [] = throwError $ NumArgs 1 []
symbol2String args@(_ : _) = throwError $ NumArgs 1 args

string2Symbol :: [LispVal] -> ThrowsError LispVal
string2Symbol ([String s]) = return $ Atom s
string2Symbol [] = throwError $ NumArgs 1 []
string2Symbol [notString] = throwError $ TypeMismatch "string" notString
string2Symbol args@(_ : _) = throwError $ NumArgs 1 args

isChar :: [LispVal] -> ThrowsError LispVal
isChar ([Char _]) = return $ Bool True
isChar _ = return $ Bool False

isString :: [LispVal] -> ThrowsError LispVal
isString ([String _]) = return $ Bool True
isString _ = return $ Bool False

isBoolean :: [LispVal] -> ThrowsError LispVal
isBoolean ([Bool _]) = return $ Bool True
isBoolean _ = return $ Bool False


