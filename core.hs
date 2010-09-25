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

module Main where
import Scheme.Macro
import Scheme.Numerical
import Scheme.Parser
import Scheme.Types
import Scheme.Variables
import Complex
import Control.Monad
import Control.Monad.Error
import Char
import Data.Array
import Data.IORef
import qualified Data.Map
import Maybe
import List
import IO hiding (try)
import Numeric
import Ratio
import System.Environment
import System.Console.Haskeline

main :: IO ()
main = do args <- getArgs
          if null args then do showBanner
                               runRepl
                       else runOne $ args

-- REPL Section
flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

evalString :: Env -> String -> IO String
evalString env expr = runIOThrows $ liftM show $ (liftThrows $ readExpr expr) >>= macroEval env >>= eval env

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = evalString env expr >>= putStrLn

runOne :: [String] -> IO ()
runOne args = do
-- TODO: hook into macroEval (probably at a lower level, though)
  env <-primitiveBindings >>= flip bindVars [((varNamespace, "args"), List $ map String $ drop 1 args)]
  (runIOThrows $ liftM show $ eval env (List [Atom "load", String (args !! 0)]))
     >>= hPutStrLn stderr  -- TODO: echo this or not??

  -- Call into (main) if it exists...
  alreadyDefined <- liftIO $ isBound env "main"
  let argv = List $ map String $ args
  if alreadyDefined
     then (runIOThrows $ liftM show $ eval env (List [Atom "main", List [Atom "quote", argv]])) >>= hPutStrLn stderr
     else (runIOThrows $ liftM show $ eval env $ Nil "") >>= hPutStrLn stderr

showBanner :: IO ()
showBanner = do
  putStrLn " __  __     __  __     ______     __  __                             "
  putStrLn "/\\ \\_\\ \\   /\\ \\/\\ \\   /\\  ___\\   /\\ \\/ /     Scheme Interpreter " 
  putStrLn "\\ \\  __ \\  \\ \\ \\_\\ \\  \\ \\___  \\  \\ \\  _\\\"-.  Version 1.0"
  putStrLn " \\ \\_\\ \\_\\  \\ \\_____\\  \\/\\_____\\  \\ \\_\\ \\_\\  (c) 2010 Justin Ethier "
  putStrLn "  \\/_/\\/_/   \\/_____/   \\/_____/   \\/_/\\/_/  github.com/justinethier/husk-scheme "
  putStrLn ""

runRepl :: IO ()
runRepl = do
    env <- primitiveBindings
    runInputT defaultSettings (loop env) 
    where 
        loop :: Env -> InputT IO ()
        loop env = do
            minput <- getInputLine "huski> "
            case minput of
                Nothing -> return ()
                Just "quit" -> return ()
                Just "" -> loop env -- FUTURE: integrate with strip to ignore inputs of just whitespace
                Just input -> do result <- liftIO (evalString env input)
                                 if (length result) > 0
                                    then do outputStrLn result
                                            loop env
                                    else loop env
-- End REPL Section


-- Eval section
eval :: Env -> LispVal -> IOThrowsError LispVal
eval env val@(Nil _) = return val
eval env val@(String _) = return val
eval env val@(Char _) = return val
eval env val@(Complex _) = return val
eval env val@(Float _) = return val
eval env val@(Rational _) = return val
eval env val@(Number _) = return val
eval env val@(Bool _) = return val
eval env val@(HashTable _) = return val
eval env (Atom id) = getVar env id
eval env (List [Atom "quote", val]) = return val
eval env (List [Atom "quasiquote", val]) = do
  case val of
    List [Atom "unquote", val] -> eval env val -- Handle cases like `,(+ 1 2) 
    List [Atom "unquote-splicing", val] -> eval env val -- TODO: not quite right behavior 
    List (x : xs) -> mapM (doUnQuote env) (x:xs) >>= return . List -- TODO: understand *why* this works 
    otherwise -> doUnQuote env val 
  where doUnQuote :: Env -> LispVal -> IOThrowsError LispVal
        doUnQuote env val = do
          case val of
            List [Atom "unquote", val] -> eval env val
            List [Atom "unquote-splicing", val] -> eval env val -- TODO: not quite right behavior
            otherwise -> eval env (List [Atom "quote", val]) -- TODO: could this be simplified?

eval env (List [Atom "if", pred, conseq, alt]) = {- TODO: alt should be optional (though not per spec)-} 
    do result <- eval env pred
       case result of
         Bool False -> eval env alt
         otherwise -> eval env conseq
         {- ex #1: only allow boolean conditions: otherwise -> throwError $ TypeMismatch "bool" otherwise-}

-- TODO: implement this 'if' form, such that it returns nothing if the pred evaluates to false
eval env (List [Atom "if", pred, conseq]) = 
    do result <- eval env pred
       case result of
         Bool True -> eval env conseq
         otherwise -> eval env $ List []

eval env (List (Atom "cond" : clauses)) = 
  if length clauses == 0
   then throwError $ BadSpecialForm "No matching clause" $ String "cond"
   else do
       let c =  clauses !! 0 -- First clause
       let cs = tail clauses -- other clauses
       test <- case c of
         List (Atom "else" : expr) -> eval env $ Bool True
         List (cond : expr) -> eval env cond
         badType -> throwError $ TypeMismatch "clause" badType 
       case test of
         Bool True -> evalCond env c
         otherwise -> eval env $ List $ (Atom "cond" : cs)

eval env (List (Atom "case" : keyAndClauses)) = 
    do let key = keyAndClauses !! 0
       let cls = tail keyAndClauses
       ekey <- eval env key
       evalCase env $ List $ (ekey : cls)

eval env (List (Atom "begin" : funcs)) = 
  if length funcs == 0
     then eval env $ Nil ""
     else if length funcs == 1
             then eval env (head funcs)
             else do
                 let fs = tail funcs
                 eval env (head funcs)
                 eval env (List (Atom "begin" : fs))

eval env (List [Atom "load", String filename]) =
     load filename >>= liftM last . mapM (evaluate env)
	 where evaluate env val = macroEval env val >>= eval env

eval env (List [Atom "set!", Atom var, form]) = 
  eval env form >>= setVar env var

eval env (List [Atom "define", Atom var, form]) = 
  eval env form >>= defineVar env var

eval env (List (Atom "define" : List (Atom var : params) : body )) = 
  makeNormalFunc env params body >>= defineVar env var
eval env (List (Atom "define" : DottedList (Atom var : params) varargs : body)) = 
  makeVarargs varargs env params body >>= defineVar env var
eval env (List (Atom "lambda" : List params : body)) = 
  makeNormalFunc env params body
eval env (List (Atom "lambda" : DottedList params varargs : body)) = 
  makeVarargs varargs env params body
eval env (List (Atom "lambda" : varargs@(Atom _) : body)) = 
  makeVarargs varargs env [] body

eval env (List [Atom "string-fill!", Atom var, character]) = do 
  str <- eval env =<< getVar env var
  chr <- eval env character
  (eval env $ fillStr(str, chr)) >>= setVar env var
  where fillStr (String str, Char chr) = doFillStr (String "", Char chr, length str)
  
        doFillStr (String str, Char chr, left) = do
        if left == 0
           then String str
           else doFillStr(String $ chr : str, Char chr, left - 1)

eval env (List [Atom "string-set!", Atom var, index, character]) = do 
  idx <- eval env index
  chr <- eval env character
  str <- eval env =<< getVar env var
  (eval env $ substr(str, character, idx)) >>= setVar env var
  where substr (String str, Char chr, Number index) = do
                              let slength = fromInteger index
                              String $ (take (fromInteger index) . drop 0) str ++ 
                                       [chr] ++
                                       (take (length str) . drop (fromInteger index + 1)) str
    -- TODO: error handler

eval env val@(Vector _) = return val

eval env (List [Atom "vector-set!", Atom var, index, object]) = do 
  idx <- eval env index
  obj <- eval env object
  vec <- eval env =<< getVar env var
  (eval env $ (updateVector vec idx obj)) >>= setVar env var
  where updateVector (Vector vec) (Number idx) obj = Vector $ vec//[(fromInteger idx, obj)]
        -- TODO: error handler?
-- TODO: error handler? - eval env (List [Atom "vector-set!", args]) = throwError $ NumArgs 2 args

eval env (List [Atom "vector-fill!", Atom var, object]) = do 
  obj <- eval env object
  vec <- eval env =<< getVar env var
  (eval env $ (fillVector vec obj)) >>= setVar env var
  where fillVector (Vector vec) obj = do
          let l = replicate (lenVector vec) obj
          Vector $ (listArray (0, length l - 1)) l
        lenVector v = length (elems v)
        -- TODO: error handler?
-- TODO: error handler? - eval env (List [Atom "vector-fill!", args]) = throwError $ NumArgs 2 args

eval env (List [Atom "hash-table-set!", Atom var, rkey, rvalue]) = do 
  key <- eval env rkey
  value <- eval env rvalue
  h <- eval env =<< getVar env var
  case h of
    HashTable ht -> (eval env $ HashTable $ Data.Map.insert key value ht) >>= setVar env var
    otherwise -> throwError $ TypeMismatch "hash-table" otherwise

eval env (List [Atom "hash-table-delete!", Atom var, rkey]) = do 
  key <- eval env rkey
  h <- eval env =<< getVar env var
  case h of
    HashTable ht -> (eval env $ HashTable $ Data.Map.delete key ht) >>= setVar env var
    otherwise -> throwError $ TypeMismatch "hash-table" otherwise

-- TODO:
--  hash-table-update!
--  hash-table-update!/default
--  hash-table-merge!

eval env (List (function : args)) = do
  func <- eval env function
  argVals <- mapM (eval env) args
  apply func argVals

--Obsolete (?) - eval env (List (Atom func : args)) = mapM (eval env) args >>= liftThrows . apply func
eval env badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

-- Helper function for evaluating 'case'
-- TODO: still need to handle case where nothing matches key
--       (same problem exists with cond, if)
evalCase :: Env -> LispVal -> IOThrowsError LispVal
evalCase env (List (key : cases)) = do
         let c = cases !! 0
         ekey <- eval env key
         case c of
           List (Atom "else" : exprs) -> last $ map (eval env) exprs
           List (List cond : exprs) -> do test <- checkEq env ekey (List cond)
                                          case test of
                                            Bool True -> last $ map (eval env) exprs
                                            otherwise -> evalCase env $ List $ ekey : tail cases
           badForm -> throwError $ BadSpecialForm "Unrecognized special form in case" badForm
  where
    checkEq env ekey (List (x : xs)) = do 
     test <- eval env $ List [Atom "eqv?", ekey, x]
     case test of
       Bool True -> eval env $ Bool True
       otherwise -> checkEq env ekey (List xs)

    checkEq env ekey val =
     case val of
       List [] -> eval env $ Bool False -- If nothing else is left, then nothing matched key
       otherwise -> do
          test <- eval env $ List [Atom "eqv?", ekey, val]
          case test of
            Bool True -> eval env $ Bool True
            otherwise -> eval env $ Bool False

evalCase key badForm = throwError $ BadSpecialForm "case: Unrecognized special form" badForm

-- Helper function for evaluating 'cond'
evalCond :: Env -> LispVal -> IOThrowsError LispVal
evalCond env (List [test, expr]) = eval env expr
evalCond env (List (test : expr)) = last $ map (eval env) expr -- TODO: all expr's need to be evaluated, not sure happening right now
evalCond env badForm = throwError $ BadSpecialForm "evalCond: Unrecognized special form" badForm

makeFunc varargs env params body = return $ Func (map showVal params) varargs body env
makeNormalFunc = makeFunc Nothing
makeVarargs = makeFunc . Just . showVal

apply :: LispVal -> [LispVal] -> IOThrowsError LispVal
apply (IOFunc func) args = func args
apply (PrimitiveFunc func) args = liftThrows $ func args
apply (Func params varargs body closure) args =
  if num params /= num args && varargs == Nothing
     then throwError $ NumArgs (num params) args
     else (liftIO $ bindVars closure $ zip (map ((,) varNamespace) params) args) >>= bindVarArgs varargs >>= evalBody
  where remainingArgs = drop (length params) args
        num = toInteger . length
        evalBody env = liftM last $ mapM (eval env) body
        bindVarArgs arg env = case arg of
          Just argName -> liftIO $ bindVars env [((varNamespace, argName), List $ remainingArgs)]
          Nothing -> return env
apply func args = throwError $ BadSpecialForm "Unable to evaluate form" $ List (func : args)

primitiveBindings :: IO Env
primitiveBindings = nullEnv >>= (flip bindVars $ map (makeFunc IOFunc) ioPrimitives
                                              ++ map (makeFunc PrimitiveFunc) primitives)
  where makeFunc constructor (var, func) = ((varNamespace, var), constructor func)

ioPrimitives :: [(String, [LispVal] -> IOThrowsError LispVal)]
ioPrimitives = [("apply", applyProc),
                ("open-input-file", makePort ReadMode),
                ("open-output-file", makePort WriteMode),
                ("close-input-port", closePort),
                ("close-output-port", closePort),
                ("read", readProc),
                ("write", writeProc),
                ("read-contents", readContents),
                ("read-all", readAll)]

applyProc :: [LispVal] -> IOThrowsError LispVal
applyProc [func, List args] = apply func args
applyProc (func : args) = apply func args

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

-- TODO: need to make sure these handle the full tower:
              ("=", numBoolBinop (==)),
              ("<", numBoolBinop (<)),
              (">", numBoolBinop (>)),
              ("/=", numBoolBinop (/=)),
              (">=", numBoolBinop (>=)),
              ("<=", numBoolBinop (<=)),

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

numBoolBinop = boolBinop unpackNum
strBoolBinop = boolBinop unpackStr
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
car [List (x : xs)] = return x
car [DottedList (x : xs) _] = return x
car [badArg] = throwError $ TypeMismatch "pair" badArg
car badArgList = throwError $ NumArgs 1 badArgList

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (x : xs)] = return $ List xs
cdr [DottedList [xs] x] = return x
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
equal [l1@(List arg1), l2@(List arg2)] = eqvList equal [l1, l2]
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
    Just val -> return $ Bool True
    Nothing -> return $ Bool False

hashTblRef [(HashTable ht), key@(_)] = do
  case Data.Map.lookup key ht of
    Just val -> return $ val
    Nothing -> throwError $ BadSpecialForm "Hash table does not contain key" key
-- TODO: a thunk can optionally be specified, this drives definition of /default
hashTblRef [(HashTable ht), key@(_), thunk@(Func params vararg body closure)] = do
  case Data.Map.lookup key ht of
    Just val -> return $ val
-- TODO:    Nothing -> apply thunk []
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
  return $ List $ map (\(k, v) -> k) $ Data.Map.toList ht
hashTblKeys [badType] = throwError $ TypeMismatch "hash-table" badType
hashTblKeys badArgList = throwError $ NumArgs 1 badArgList

hashTblValues [(HashTable ht)] = do
  return $ List $ map (\(k, v) -> v) $ Data.Map.toList ht
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

doMakeString n chr s = 
    if n == 0 
       then String s
       else doMakeString (n - 1) chr (s ++ [chr])

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
  do let length = fromInteger $ end - start
     let begin = fromInteger start 
     return $ String $ (take length . drop begin) s
substring [badType] = throwError $ TypeMismatch "string number number" badType
substring badArgList = throwError $ NumArgs 3 badArgList

stringCIEquals :: [LispVal] -> ThrowsError LispVal
stringCIEquals [(String s1), (String s2)] = do
  if (length s1) /= (length s2)
     then return $ Bool False
     else return $ Bool $ ciCmp s1 s2 0
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
stringCIBoolBinop op [badType] = throwError $ TypeMismatch "string string" badType
stringCIBoolBinop op badArgList = throwError $ NumArgs 2 badArgList

stringAppend :: [LispVal] -> ThrowsError LispVal
stringAppend [(String s)] = return $ String s -- Needed for "last" string value
stringAppend (String st:sts) = do
  rest <- stringAppend sts
-- TODO: I needed to use <- instead of "let = " here, for type problems. Why???
-- TBD: this probably will solve type problems when processing other lists of objects in the other string functions
  case rest of
    String s -> return $ String $ st ++ s
    otherwise -> throwError $ TypeMismatch "string" otherwise
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
    otherwise -> return $ Bool False
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
isDottedList ([DottedList l d]) = return $ Bool True
isDottedList _ = return $  Bool False

isProcedure :: [LispVal] -> ThrowsError LispVal
isProcedure ([PrimitiveFunc f]) = return $ Bool True
isProcedure ([Func params vararg body closure]) = return $ Bool True
isProcedure ([IOFunc f]) = return $ Bool True
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
isSymbol ([Atom a]) = return $ Bool True
isSymbol _ = return $ Bool False

symbol2String :: [LispVal] -> ThrowsError LispVal
symbol2String ([Atom a]) = return $ String a
symbol2String [notAtom] = throwError $ TypeMismatch "symbol" notAtom

string2Symbol :: [LispVal] -> ThrowsError LispVal
string2Symbol ([String s]) = return $ Atom s
string2Symbol [notString] = throwError $ TypeMismatch "string" notString

isChar :: [LispVal] -> ThrowsError LispVal
isChar ([Char a]) = return $ Bool True
isChar _ = return $ Bool False

isString :: [LispVal] -> ThrowsError LispVal
isString ([String s]) = return $ Bool True
isString _ = return $ Bool False

isBoolean :: [LispVal] -> ThrowsError LispVal
isBoolean ([Bool n]) = return $ Bool True
isBoolean _ = return $ Bool False
-- end Eval section

-- Begin Util section, of generic functions

-- Remove leading/trailing white space from a string; based on corresponding Python function
-- Code taken from: http://gimbo.org.uk/blog/2007/04/20/splitting-a-string-in-haskell/
strip :: String -> String
strip s = dropWhile ws $ reverse $ dropWhile ws $ reverse s
    where ws = (`elem` [' ', '\n', '\t', '\r'])

-- End Util
