{-
 - husk scheme interpreter
 -
 - A lightweight dialect of R5RS scheme.
 - Core functionality
 -
 - @author Justin Ethier
 -
 - -}

{-
 - TODO: 
 -
 - => compare my functions against those listed on 
 -    http://en.wikipedia.org/wiki/Scheme_(programming_language)
 -
 - -}

module Scheme.Core 
    (
      eval
    , evalLisp
    , evalString
    , evalAndPrint
    , primitiveBindings -- FUTURE: this may be a bad idea...
                        -- but there should be an interface to inject custom functions written in Haskell
    ) where
import Scheme.Macro
import Scheme.Numerical
import Scheme.Parser
import Scheme.Types
import Scheme.Variables
import Control.Monad.Error
import Char
import Data.Array
import qualified Data.Map
import Maybe
import List
import IO hiding (try)

import Debug.Trace


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
evalAndPrint env expr = evalString env expr >>= putStrLn --TODO: cont parameter

-- |Evaluate lisp code that has already been loaded into haskell
--
--  TODO: code example for this, via ghci and/or a custom program.
evalLisp :: Env -> LispVal -> IOThrowsError LispVal
evalLisp env lisp = macroEval env lisp >>= (eval env (makeNullContinuation env))


{- Changes will be required to eval to support continuations. According to original wiki book:
 -   TBD
 -
 -
 - Need to rethink below and come up with a clear, top-level design approach. Some starting points
 - for this are:
 -  http://c2.com/cgi/wiki?ContinuationImplementation
 -  http://c2.com/cgi/wiki?CallWithCurrentContinuation (the link to this book may be helpful as well: http://c2.com/cgi/wiki?EssentialsOfProgrammingLanguages - apparently if the interpreter is written using CPS, then call/cc is free)
 -  http://tech.phillipwright.com/2010/05/23/continuations-in-scheme/
 -  http://community.schemewiki.org/?call-with-current-continuation
 -
 - ALSO, consider the following quote: 
 -  "CPS is a programming style where no function is ever allowed to return."
 - So, this would mean that when evaluating a simple integer, string, etc eval should call into
 - the continuation instead of just returning.
 - Need to think about how this will be handled, how functions will be called using CPS, and what
 - the continuation data type needs to contain.
 -
 -
 -
 -
 -
 - Some of my notes:
 - as simple as using CPS to evaluate lists of "lines" (body)? Then could pass the next part of the CPS as the cont arg to eval. Or is this too simple to work? need to think about this - http://en.wikipedia.org/wiki/Continuation-passing_style
 -
 - Possible design approach:
 -
 -  * thread cont through eval
 -  * instead of returning, call into next eval using CPS style, with the cont parameter.
 -    this replaces code in evalBody (possibly other places?) that uses local CPS to execute a function
 -  * parameter will consist of a lisp function
 -  * eval will call into another function to deal with details of manipulating the cont prior to next call
 -    need to work out details of exactly how that would work, but could for example just go to the next line
 -    of body. 
 -  * To continue above point, where is eval'd value returned to? May want to refer to R5RS section that describes call/cc:
 -  A common use of call-with-current-continuation is for structured, non-local exits from loops or procedure bodies, but in fact call-with-current-continuation is extremely useful for implementing a wide variety of advanced control structures.
 -
 -  Whenever a Scheme expression is evaluated there is a continuation wanting the result of the expression. The continuation represents an entire (default) future for the computation. If the expression is evaluated at top level, for example, then the continuation might take the result, print it on the screen, prompt for the next input, evaluate it, and so on forever. Most of the time the continuation includes actions specified by user code, as in a continuation that will take the result, multiply it by the value stored in a local variable, add seven, and give the answer to the top level continuation to be printed. Normally these ubiquitous continuations are hidden behind the scenes and programmers do not think much about them. On rare occasions, however, a programmer may need to deal with continuations explicitly. Call-with-current-continuation allows Scheme programmers to do that by creating a procedure that acts just like the current continuation.
 -
 -  Most programming languages incorporate one or more special-purpose escape constructs with names like exit, return, or even goto. In 1965, however, Peter Landin [16] invented a general purpose escape operator called the J-operator. John Reynolds [24] described a simpler but equally powerful construct in 1972. The catch special form described by Sussman and Steele in the 1975 report on Scheme is exactly the same as Reynolds's construct, though its name came from a less general construct in MacLisp. Several Scheme implementors noticed that the full power of the catch construct could be provided by a procedure instead of by a special syntactic construct, and the name call-with-current-continuation was coined in 1982. This name is descriptive, but opinions differ on the merits of such a long name, and some people use the name call/cc instead.
 -
 -  * need to consider what would be passed when evaluating via a REPL, at top-level, via haskell entry points, etc...
 -
 - -}

{-  
 - Transformed eval section into CPS by calling into this instead of returning from eval.
 - This function uses the cont argument to determine whether to keep going or to 
 - finally return a result.
 - -}
continueEval :: Env -> LispVal -> LispVal -> IOThrowsError LispVal
continueEval _ cont@(Continuation cEnv cBody cCont cFunc@Nothing cArgs@Nothing) val = do
    case cBody of
--      case (trace ("cBody => " ++ show cBody ++ " val => " ++ show val) cBody) of
        [] -> do
          case cCont of
            Continuation nEnv nBody nCont nFunc nArgs -> continueEval nEnv cCont val
            _ -> return val
        [lv] -> eval cEnv (Continuation cEnv [] cCont Nothing Nothing) lv --val
--        [lv] -> eval cEnv (Continuation cEnv [] cCont) (trace ("clv => " ++ show lv) lv) --val
        (lv : lvs) -> eval cEnv (Continuation cEnv lvs cCont Nothing Nothing) lv
--        (lv : lvs) -> eval cEnv (Continuation cEnv (trace ("clvs => " ++ show lvs) lvs) cCont) (trace ("lv:lvs, (lv) => " ++ show lv) lv)
{-
continueEval _ cont@(Continuation cEnv cBody cCont cFunc Nothing) _ = do
    -- This section is called when we are evaluating a function call
    -- First the function needs to be eval'd. Then once that is done,
    -- We will drop into the section below to eval each argument.
    -- Once all that is done, the function can be called.
    --
    -- Notes: function needs to eval'd, then args
    -- need to call back into the cont later on, probably after
    -- calling one of the makefunc variants? need to make sure
    -- changing those calls does not break anything else
    eval cEnv (Continuation cEnv cBody cCont (Just $ Nil "") (Just [])) (fromJust cFunc)
--    continueEval cEnv (Continuation cEnv cBody cCont cFunc (Just [])) =<< eval cEnv (Continuation cEnv cBody cCont (Just $ Nil "") (Just [])) (fromJust cFunc)
    -- TODO: above call to continueEval is just temporary; this needs to be called from the proper place(s) in eval

continueEval _ cont@(Continuation cEnv cBody cCont (Just cFunc) (Just cArgs)) val = do 
--    if length cArgs == 0 && cFunc == (Nil "")
    case cFunc of
       Nil _ -> do
            case (trace ("cBody1: " ++ show cBody) cBody) of
                [] -> apply cCont cFunc []
                [arg] -> do -- Eval the arg, but keep in mind val contains the function
                            eval cEnv (Continuation cEnv [] cCont (Just val) (Just cArgs)) arg
                (arg : args) -> do -- Peel off next arg and evaluate it, saving function
                                   eval cEnv (Continuation cEnv args cCont (Just (trace ("val: " ++ show val) val)) (Just cArgs)) arg
       o -> case (trace ("cBody2: " ++ show cBody) cBody) of
                [] -> apply cCont cFunc (trace ("args: " ++ show (cArgs ++ [val])) (cArgs ++ [val])) -- No more arguments, call the function
                [arg] -> do -- Evaluate the last arg
                            eval cEnv (Continuation cEnv [] cCont (Just cFunc) (Just $ cArgs ++ [val])) arg
                (arg : args) -> do -- Peel off next arg and evaluate it
                                   eval cEnv (Continuation cEnv args cCont (Just cFunc) (Just $ cArgs ++ [val])) arg
-}
-- |Core eval function
--
--  NOTE:  This function does not include macro support and should not be called directly. Instead, use 'evalLisp'
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
eval env cont (List [Atom "quote", val])         = continueEval env cont val
eval envi cont (List [Atom "quasiquote", value]) = continueEval envi cont =<< doUnQuote envi value
  where doUnQuote :: Env -> LispVal -> IOThrowsError LispVal
        doUnQuote env val = do
          case val of
            List [Atom "unquote", val] -> eval env cont val
            List (x : xs) -> unquoteListM env (x:xs) >>= return . List
            DottedList xs x -> do
              rxs <- unquoteListM env xs >>= return 
              rx <- doUnQuote env x
              case rx of
                List [] -> return $ List rxs
                List rxlst -> return $ List $ rxs ++ rxlst 
                DottedList rxlst rxlast -> return $ DottedList (rxs ++ rxlst) rxlast
                _ -> return $ DottedList rxs rx
            Vector vec -> do
              let len = length (elems vec)
              vList <- unquoteListM env $ elems vec >>= return
              return $ Vector $ listArray (0, len) vList
            _ -> eval env cont (List [Atom "quote", val]) -- Behave like quote if there is nothing to "unquote"... 
        unquoteListM env lst = foldlM (unquoteListFld env) ([]) lst
        unquoteListFld env (acc) val = do
            case val of
                List [Atom "unquote-splicing", val] -> do
                    value <- eval env cont val
                    case value of
                        List v -> return $ (acc ++ v)
                        -- Question: In which cases should I generate a type error if value is not a list?
                        --
                        -- csi reports an error for this: `(1 ,@(+ 1 2) 4)
                        -- but allows cases such as: `,@2
                        -- For now we just throw an error - perhaps more strict than we need to be, but at
                        -- least we will not allow anything invalid to be returned.
                        --
                        -- Old code that we might build on if this changes down the road: otherwise -> return $ (acc ++ [v])
                        _ -> throwError $ TypeMismatch "proper list" value

                _ -> do result <- doUnQuote env val
                        return $ (acc ++ [result])

eval env cont (List [Atom "if", pred, conseq, alt]) =
    do result <- eval env cont pred
       case result of
         Bool False -> eval env cont alt
         otherwise -> eval env cont conseq

eval env cont (List [Atom "if", pred, conseq]) = 
    do result <- eval env cont pred
       case result of
         Bool True -> eval env cont conseq
         otherwise -> eval env cont $ List []

eval env cont (List (Atom "cond" : clauses)) = 
  if length clauses == 0
   then throwError $ BadSpecialForm "No matching clause" $ String "cond"
   else do
       let c =  clauses !! 0 -- First clause
       let cs = tail clauses -- other clauses
       test <- case c of
         List (Atom "else" : expr) -> eval env cont $ Bool True
         List (cond : expr) -> eval env cont cond
         badType -> throwError $ TypeMismatch "clause" badType 
       case test of
         Bool True -> evalCond env cont c
         otherwise -> eval env cont $ List $ (Atom "cond" : cs)

eval env cont (List (Atom "case" : keyAndClauses)) = 
    do let key = keyAndClauses !! 0
       let cls = tail keyAndClauses
       ekey <- eval env cont key
       evalCase env cont $ List $ (ekey : cls)

eval env cont (List (Atom "begin" : funcs)) = 
  if length funcs == 0
     then eval env cont $ Nil ""
     else if length funcs == 1
             then eval env cont (head funcs)
             else do
                 let fs = tail funcs
                 eval env cont (head funcs)
                 eval env cont (List (Atom "begin" : fs))

eval env cont (List [Atom "load", String filename]) = do
--     load filename >>= liftM last . mapM (evaluate env cont)
     result <- load filename >>= liftM last . mapM (evaluate env (makeNullContinuation env))
     continueEval env cont result
	 where evaluate env cont2 val = macroEval env val >>= eval env cont2

eval env cont (List [Atom "set!", Atom var, form]) = do 
--  eval env cont form >>= setVar env var
  result <- eval env (makeNullContinuation env) form >>= setVar env var
  continueEval env cont result

eval env cont (List [Atom "define", Atom var, form]) = do 
--  eval env cont form >>= defineVar env var
  result <- eval env (makeNullContinuation env) form >>= defineVar env var
  continueEval env cont result

eval env cont (List (Atom "define" : List (Atom var : params) : body )) = 
  makeNormalFunc env params body >>= defineVar env var
eval env cont (List (Atom "define" : DottedList (Atom var : params) varargs : body)) = 
  makeVarargs varargs env params body >>= defineVar env var
eval env cont (List (Atom "lambda" : List params : body)) = 
  makeNormalFunc env params body
eval env cont (List (Atom "lambda" : DottedList params varargs : body)) = 
  makeVarargs varargs env params body
eval env cont (List (Atom "lambda" : varargs@(Atom _) : body)) = 
  makeVarargs varargs env [] body

eval env cont (List [Atom "string-fill!", Atom var, character]) = do 
  str <- eval env (makeNullContinuation env) =<< getVar env var 
  chr <- eval env (makeNullContinuation env) character
  result <- (eval env (makeNullContinuation env) $ fillStr(str, chr)) >>= setVar env var
  continueEval env cont result
  where fillStr (String str, Char chr) = doFillStr (String "", Char chr, length str)
  
        doFillStr (String str, Char chr, left) = do
        if left == 0
           then String str
           else doFillStr(String $ chr : str, Char chr, left - 1)

eval env cont (List [Atom "string-set!", Atom var, index, character]) = do 
  idx <- eval env (makeNullContinuation env) index
  str <- eval env (makeNullContinuation env) =<< getVar env var
  result <- (eval env (makeNullContinuation env) $ substr(str, character, idx)) >>= setVar env var
  continueEval env cont result
  where substr (String str, Char char, Number index) = do
                              String $ (take (fromInteger index) . drop 0) str ++ 
                                       [char] ++
                                       (take (length str) . drop (fromInteger index + 1)) str
    -- TODO: error handler

eval env cont (List [Atom "vector-set!", Atom var, index, object]) = do 
  idx <- eval env (makeNullContinuation env) index
  obj <- eval env (makeNullContinuation env) object
  vec <- eval env (makeNullContinuation env) =<< getVar env var
  result <- (eval env (makeNullContinuation env) $ (updateVector vec idx obj)) >>= setVar env var
  continueEval env cont result
  where updateVector (Vector vec) (Number idx) obj = Vector $ vec//[(fromInteger idx, obj)]
        -- TODO: error handler?
-- TODO: error handler? - eval env (List [Atom "vector-set!", args]) = throwError $ NumArgs 2 args

eval env cont (List [Atom "vector-fill!", Atom var, object]) = do 
  obj <- eval env (makeNullContinuation env) object
  vec <- eval env (makeNullContinuation env) =<< getVar env var
  result <- (eval env (makeNullContinuation env) $ (fillVector vec obj)) >>= setVar env var
  continueEval env cont result
  where fillVector (Vector vec) obj = do
          let l = replicate (lenVector vec) obj
          Vector $ (listArray (0, length l - 1)) l
        lenVector v = length (elems v)
        -- TODO: error handler?
-- TODO: error handler? - eval env cont (List [Atom "vector-fill!", args]) = throwError $ NumArgs 2 args

eval env cont (List [Atom "hash-table-set!", Atom var, rkey, rvalue]) = do 
  key <- eval env (makeNullContinuation env) rkey
  value <- eval env (makeNullContinuation env) rvalue
  h <- eval env (makeNullContinuation env) =<< getVar env var
  case h of
    HashTable ht -> do
      result <- (eval env (makeNullContinuation env) $ HashTable $ Data.Map.insert key value ht) >>= setVar env var
      continueEval env cont result
    other -> throwError $ TypeMismatch "hash-table" other

eval env cont (List [Atom "hash-table-delete!", Atom var, rkey]) = do 
  key <- eval env (makeNullContinuation env) rkey
  h <- eval env (makeNullContinuation env) =<< getVar env var
  case h of
    HashTable ht -> do
      result <- (eval env (makeNullContinuation env) $ HashTable $ Data.Map.delete key ht) >>= setVar env var
      continueEval env cont result
    other -> throwError $ TypeMismatch "hash-table" other

-- TODO:
--  hash-table-merge!


-- TODO: for CPS form, need to be able to pass a continuation to a function
-- need to work through this implementation.
--
-- See http://www.schemers.org/Documents/Standards/R5RS/HTML/r5rs-Z-H-9.html#%_sec_6.6
-- for test cases that are required to ensure apply is not broken by this change. Need
-- it intact prior to proceeding with CPS and functions
eval env cont (List [Atom "apply"]) = throwError $ BadSpecialForm "apply" $ String "Function not specified"
eval env cont (List [Atom "apply", proc]) = throwError $ BadSpecialForm "apply" $ String "Arguments not specified"
eval env cont (List (Atom "apply" : params)) = do
    -- FUTURE: verify length of list?
    -- TODO: for all Continuations below, will almost certainly need to pull each into this continuation
    proc <- eval env (makeNullContinuation env) $ head $ params
    lst <- eval env (makeNullContinuation env) $ head $ reverse params
    argVals <- mapM (eval env (makeNullContinuation env)) $ tail $ reverse $ tail (reverse params)
    case lst of
      List l -> apply cont proc (argVals ++ l)
      other -> throwError $ TypeMismatch "list" other

eval env cont (List (Atom "call-with-current-continuation" : args)) = 
  eval env cont (List (Atom "call/cc" : args))
eval env cont (List [Atom "call/cc"]) = throwError $ Default "Procedure not specified"
eval env cont (List [Atom "call/cc", proc]) = do
  func <- eval env (makeNullContinuation env) proc 
  case func of
    PrimitiveFunc func -> liftThrows $ func [cont]
    Func aparams _ _ _ _ ->
      if (toInteger $ length aparams) == 1 
        then apply cont func [cont] 
        else throwError $ NumArgs (toInteger $ length aparams) [cont] 
    other -> throwError $ TypeMismatch "procedure" other


eval env cont (List (function : args)) = do
--    continueEval env (Continuation env (args) cont (Just function) Nothing) $ Nil ""  
--{ - TODO: obsolete code, delete once above is working
  func <- eval env (makeNullContinuation env) function -- TODO: almost certainly need to pull this into the continuation
  argVals <- mapM (eval env (makeNullContinuation env)) args -- TODO: almost certainly need to pull this into the continuation
  apply cont func argVals
-- } 

--Obsolete (?) - eval env cont (List (Atom func : args)) = mapM (eval env) args >>= liftThrows . apply func
eval env cont badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

-- Helper function for evaluating 'case'
-- TODO: still need to handle case where nothing matches key
--       (same problem exists with cond, if)
evalCase :: Env -> LispVal -> LispVal -> IOThrowsError LispVal
evalCase env cont (List (key : cases)) = do
         let c = cases !! 0
         ekey <- eval env cont key
         case c of
           List (Atom "else" : exprs) -> last $ map (eval env cont) exprs
           List (List cond : exprs) -> do test <- checkEq env ekey (List cond)
                                          case test of
                                            Bool True -> last $ map (eval env cont) exprs
                                            _ -> evalCase env cont $ List $ ekey : tail cases
           badForm -> throwError $ BadSpecialForm "Unrecognized special form in case" badForm
  where
    checkEq env ekey (List (x : xs)) = do 
     test <- eval env cont $ List [Atom "eqv?", ekey, x]
     case test of
       Bool True -> eval env cont $ Bool True
       _ -> checkEq env ekey (List xs)

    checkEq env ekey val =
     case val of
       List [] -> eval env cont $ Bool False -- If nothing else is left, then nothing matched key
       _ -> do
          test <- eval env cont $ List [Atom "eqv?", ekey, val]
          case test of
            Bool True -> eval env cont $ Bool True
            _ -> eval env cont $ Bool False

evalCase _ _ badForm = throwError $ BadSpecialForm "case: Unrecognized special form" badForm

-- Helper function for evaluating 'cond'
evalCond :: Env -> LispVal -> LispVal -> IOThrowsError LispVal
evalCond env cont (List [_, expr]) = eval env cont expr
evalCond env cont (List (_ : expr)) = last $ map (eval env cont) expr -- TODO: all expr's need to be evaluated, not sure happening right now
evalCond _ _ badForm = throwError $ BadSpecialForm "evalCond: Unrecognized special form" badForm

--makeFunc :: forall (m :: * -> *).(Monad m) => Maybe String -> Env -> [LispVal] -> [LispVal] -> m LispVal
makeFunc varargs env params body = return $ Func (map showVal params) varargs body env False
{-makeNormalFunc :: Env
               -> [LispVal]
               -> [LispVal]
               -> m LispVal-}
makeNormalFunc = makeFunc Nothing
{-makeVarargs :: LispVal  -> Env
                        -> [LispVal]
                        -> [LispVal]
                        -> m LispVal-}
makeVarargs = makeFunc . Just . showVal

apply :: LispVal -> LispVal -> [LispVal] -> IOThrowsError LispVal
apply _ c@(Continuation env _ _ _ _) args = do
  if (toInteger $ length args) /= 1 
    then throwError $ NumArgs 1 args
    else continueEval env c $ head args -- may not be correct, what happens if call/cc is an inner part of a list?
--    else continueEval env (trace ("continueEval => " ++ show cont) c) $ head args -- may not be correct, what happens if call/cc is an inner part of a list?
      -- TODO:
      -- this is not good enough. is it correct if we take c and replace the "outer" continuation with it??
      --
      -- this would work for return and other simple examples
apply _ (IOFunc func) args = func args
apply _ (PrimitiveFunc func) args = liftThrows $ func args
apply cont f@(Func aparams avarargs abody aclosure _) args =
  if num aparams /= num args && avarargs == Nothing
     then throwError $ NumArgs (num aparams) args
     else (liftIO $ extendEnv aclosure $ zip (map ((,) varNamespace) aparams) args) >>= bindVarArgs avarargs >>= (evalBody abody)
  where remainingArgs = drop (length aparams) args
        num = toInteger . length
        --
        -- TODO: this works, but at the cost of breaking proper tail calls due to 
        -- high mem usage. To fix this, we need to implement TCO, for example, by
        -- not creating a new continuation object if the same function is being
        -- called with the same parameters.
        --
        evalBody body env = continueEval env (Continuation env body cont Nothing Nothing) $ Nil ""
        bindVarArgs arg env = case arg of
          Just argName -> liftIO $ extendEnv env [((varNamespace, argName), List $ remainingArgs)]
          Nothing -> return env
apply _ func args = throwError $ BadSpecialForm "Unable to evaluate form" $ List (func : args)

-- |Environment containing the primitive forms that are built into the Scheme language. Note that this only includes
--  forms that are implemented in Haskell; derived forms implemented in Scheme (such as let, list, etc) are available
--  in the standard library which must be pulled into the environment using (load).
primitiveBindings :: IO Env
primitiveBindings = nullEnv >>= (flip extendEnv $ map (domakeFunc IOFunc) ioPrimitives
                                              ++ map (domakeFunc PrimitiveFunc) primitives)
  where domakeFunc constructor (var, func) = ((varNamespace, var), constructor func)

ioPrimitives :: [(String, [LispVal] -> IOThrowsError LispVal)]
ioPrimitives = [("open-input-file", makePort ReadMode),
                ("open-output-file", makePort WriteMode),
                ("close-input-port", closePort),
                ("close-output-port", closePort),
                ("read", readProc),
                ("write", writeProc),
                ("read-contents", readContents),
                ("read-all", readAll)]

makePort :: IOMode -> [LispVal] -> IOThrowsError LispVal
makePort mode [String filename] = liftM Port $ liftIO $ openFile filename mode

closePort :: [LispVal] -> IOThrowsError LispVal
closePort [Port port] = liftIO $ hClose port >> (return $ Bool True)
closePort _ = return $ Bool False

readProc :: [LispVal] -> IOThrowsError LispVal
readProc [] = readProc [Port stdin]
readProc [Port port] = (liftIO $ hGetLine port) >>= liftThrows . readExpr

writeProc :: [LispVal] -> IOThrowsError LispVal
writeProc [obj] = writeProc [obj, Port stdout]
writeProc [obj, Port port] = liftIO $ hPrint port obj >> (return $ Nil "")

readContents :: [LispVal] -> IOThrowsError LispVal
readContents [String filename] = liftM String $ liftIO $ readFile filename

load :: String -> IOThrowsError [LispVal]
load filename = (liftIO $ readFile filename) >>= liftThrows . readExprList

readAll :: [LispVal] -> IOThrowsError LispVal
readAll [String filename] = liftM List $ load filename

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

-- TODO: sweep through the spec to make sure all numeric procedures are accounted for

-- TODO: sweep through spec and implement all numeric "library procedures" - but in stdlib.scm

-- TODO: string and number conversion functions; need to make
--       sure they are implemented and that they handle the full tower


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
{-
			  TODO: full numeric tower: number?, complex?, rational?
			  --}
              ("number?", isNumber),
              ("complex?", isComplex),
              ("real?", isReal),
              ("rational?", isRational),
              ("integer?", isInteger),
              ("list?", unaryOp isList),
              ("null?", isNull),
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
-- TODO: alist->hash-table
              ("hash-table-exists?", hashTblExists),
              ("hash-table-ref", hashTblRef),
              ("hash-table-size", hashTblSize),
              ("hash-table->alist", hashTbl2List),
              ("hash-table-keys", hashTblKeys),
              ("hash-table-values", hashTblValues),
-- TODO next: hash-table-walk, hash-table-fold 
-- TODO: many more, see SRFI
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
-- TODO: hash table?
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

hashTblRef [(HashTable ht), key@(_)] = do
  case Data.Map.lookup key ht of
    Just val -> return $ val
    Nothing -> throwError $ BadSpecialForm "Hash table does not contain key" key
hashTblRef [(HashTable ht), key@(_), Func _ _ _ _ _] = do --thunk@(Func _ _ _ _ _)] = do
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
-- TODO: I needed to use <- instead of "let = " here, for type problems. Why???
-- TBD: this probably will solve type problems when processing other lists of objects in the other string functions
  case rest of
    String s -> return $ String $ st ++ s
    other -> throwError $ TypeMismatch "string" other
stringAppend [badType] = throwError $ TypeMismatch "string" badType
stringAppend badArgList = throwError $ NumArgs 1 badArgList

-- This could be expanded, for now just converts integers
-- TODO: handle a radix param
stringToNumber :: [LispVal] -> ThrowsError LispVal
stringToNumber [(String s)] = do
  result <- (readExpr s) -- result <- parseExpr s
  case result of
    n@(Number _) -> return n
    n@(Rational _) -> return n
    n@(Float _) -> return n
    n@(Complex _) -> return n
    _ -> return $ Bool False
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

stringCopy :: [LispVal] -> ThrowsError LispVal
stringCopy [String s] = return $ String s
stringCopy [badType] = throwError $ TypeMismatch "string" badType
stringCopy badArgList = throwError $ NumArgs 2 badArgList

isDottedList :: [LispVal] -> ThrowsError LispVal
isDottedList ([DottedList _ _]) = return $ Bool True
isDottedList _ = return $  Bool False

isProcedure :: [LispVal] -> ThrowsError LispVal
isProcedure ([Continuation _ _ _ _ _]) = return $ Bool True
isProcedure ([PrimitiveFunc _]) = return $ Bool True
isProcedure ([Func _ _ _ _ _]) = return $ Bool True
isProcedure ([IOFunc _]) = return $ Bool True
isProcedure _ = return $ Bool False

isVector, isList :: LispVal -> ThrowsError LispVal
isVector (Vector _) = return $ Bool True
isVector _          = return $ Bool False
isList (List _) = return $ Bool True
isList _        = return $ Bool False

isNull :: [LispVal] -> ThrowsError LispVal
isNull ([List []]) = return $ Bool True
isNull _ = return $ Bool False

isSymbol :: [LispVal] -> ThrowsError LispVal
isSymbol ([Atom _]) = return $ Bool True
isSymbol _ = return $ Bool False

symbol2String :: [LispVal] -> ThrowsError LispVal
symbol2String ([Atom a]) = return $ String a
symbol2String [notAtom] = throwError $ TypeMismatch "symbol" notAtom

string2Symbol :: [LispVal] -> ThrowsError LispVal
string2Symbol ([String s]) = return $ Atom s
string2Symbol [notString] = throwError $ TypeMismatch "string" notString

isChar :: [LispVal] -> ThrowsError LispVal
isChar ([Char _]) = return $ Bool True
isChar _ = return $ Bool False

isString :: [LispVal] -> ThrowsError LispVal
isString ([String _]) = return $ Bool True
isString _ = return $ Bool False

isBoolean :: [LispVal] -> ThrowsError LispVal
isBoolean ([Bool _]) = return $ Bool True
isBoolean _ = return $ Bool False
-- end Eval section

{- Should not need this function, since we are using Haskell
trampoline :: Env -> LispVal -> IOThrowsError LispVal
trampoline env val = do
  result <- eval env val
  case result of
       -- If a form is not fully-evaluated to a value, bounce it back onto the trampoline...
       func@(Func params vararg body closure True) -> trampoline env func -- next iteration, via tail call (?)
       val -> return val
-}
