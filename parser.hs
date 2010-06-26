{-
 - Scheme Parser
 -
 - @author Justin Ethier
 - -}
module Main where
import Control.Monad
import Control.Monad.Error
import Char
import Data.IORef
import Maybe
import List
import IO hiding (try)
import Numeric
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)


main :: IO ()
main = do args <- getArgs
          case length args of
               0 -> runRepl
               1 -> runOne $ args !! 0
               otherwise -> putStrLn "Program takes only 0 or 1 arguments"

--  args <- getArgs
--  evaled <- return $ liftM show $ readExpr (args !! 0) >>= eval
--  putStrLn $ extractValue $ trapError evaled

-- REPL Section
flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalString :: Env -> String -> IO String
evalString env expr = runIOThrows $ liftM show $ (liftThrows $ readExpr expr) >>= eval env
--return $ extractValue $ trapError (liftM show $ readExpr expr >>= eval)

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = evalString env expr >>= putStrLn

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do
  result <- prompt
  if pred result
     then return ()
     else action result >> until_ pred prompt action

runOne :: String -> IO ()
runOne expr = nullEnv >>= flip evalAndPrint expr

runRepl :: IO ()
runRepl = nullEnv >>= until_ (== "quit") (readPrompt "skim> ") . evalAndPrint

-- End REPL Section
readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
	Left err -> throwError $ Parser err
	Right val -> return val

{- Variables Section -}
type Env = IORef [(String, IORef LispVal)]
type IOThrowsError = ErrorT LispError IO

nullEnv :: IO Env
nullEnv = newIORef []

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = runErrorT (trapError action) >>= return . extractValue

isBound :: Env -> String -> IO Bool
isBound envRef var = readIORef envRef >>= return . maybe False (const True) . lookup var

getVar :: Env -> String -> IOThrowsError LispVal
getVar envRef var = do env <- liftIO $ readIORef envRef
                       maybe (throwError $ UnboundVar "Getting an unbound variable: " var)
                             (liftIO . readIORef)
                             (lookup var env)

setVar :: Env -> String -> LispVal -> IOThrowsError LispVal
setVar envRef var value = do env <- liftIO $ readIORef envRef
                             maybe (throwError $ UnboundVar "Setting an unbound variable: " var)
                                   (liftIO . (flip writeIORef value))
                                   (lookup var env)
                             return value

defineVar :: Env -> String -> LispVal -> IOThrowsError LispVal
defineVar envRef var value = do
  alreadyDefined <- liftIO $ isBound envRef var
  if alreadyDefined
    then setVar envRef var value >> return value
    else liftIO $ do
       valueRef <- newIORef value
       env <- readIORef envRef
       writeIORef envRef ((var, valueRef) : env)
       return value

bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
  where extendEnv bindings env = liftM  (++ env) (mapM addBinding bindings)
        addBinding (var, value) = do ref <- newIORef value
                                     return (var, ref)


-- TODO: pick up "variables" chapter from here

{- Error handling code -}
data LispError = NumArgs Integer [LispVal]
  | TypeMismatch String LispVal
  | Parser ParseError
  | BadSpecialForm String LispVal
  | NotFunction String String
  | UnboundVar String String
  | Default String

showError :: LispError -> String
showError (NumArgs expected found) = "Expected " ++ show expected
                                  ++ " args; found values " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected
                                  ++ ", found " ++ show found
showError (Parser parseErr) = "Parse error at " ++ ": " ++ show parseErr
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func) = message ++ ": " ++ show func
showError (UnboundVar message varname) = message ++ ": " ++ varname

instance Show LispError where show = showError
instance Error LispError where
  noMsg = Default "An error has occurred"
  strMsg = Default

type ThrowsError = Either LispError

trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val


{- TODO: could error handling code above be moved to its own file? -}

data LispVal = Atom String
	| List [LispVal]
	| DottedList [LispVal] LispVal
	| Number Integer
	| Float Float
 	| String String
	| Char Char
	| Bool Bool

showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Char chr) = [chr] {- TODO: this is only temporary, for testing-}
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Float contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

{- Allow conversion of lispval instances to strings -}
instance Show LispVal where show = showVal

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

parseAtom :: Parser LispVal
parseAtom = do
	first <- letter <|> symbol
	rest <- many (letter <|> digit <|> symbol)
	let atom = first:rest
	return $ case atom of
		"#t" -> Bool True
		"#f" -> Bool False
		_    -> Atom atom

parseChar :: Parser LispVal
parseChar = do
  try (string "#\\")
  c <- anyChar 
  r <- many(letter)
  let chr = c:r
  return $ case chr of
    "space"   -> Char ' '
    "newline" -> Char '\n'
    _         -> Char c {- TODO: err if invalid char -}

parseOctalNumber :: Parser LispVal
parseOctalNumber = do
  try (string "#o")
  num <- many1(oneOf "01234567")
  return $ Number $ fst $ Numeric.readOct num !! 0

parseBinaryNumber :: Parser LispVal
parseBinaryNumber = do
  try (string "#b")
  num <- many1(oneOf "01")
  return $ Number $ fst $ Numeric.readInt 2 (`elem` "01") Char.digitToInt num !! 0

parseHexNumber :: Parser LispVal
parseHexNumber = do
  try (string "#x")
  num <- many1(digit <|> oneOf "abcdefABCDEF")
  return $ Number $ fst $ Numeric.readHex num !! 0

{- Parser for Integer, base 10-}
parseDecimalNumber :: Parser LispVal
parseDecimalNumber = do
  try (many(string "#d"))
  num <- many1 (digit)
  return $ (Number . read) $ num

parseNumber :: Parser LispVal
parseNumber = parseDecimalNumber <|> 
              parseHexNumber     <|> 
              parseBinaryNumber  <|> 
              parseOctalNumber   <?> 
              "Unable to parse number"

{- Parser for floating points -}
parseDecimal :: Parser LispVal
parseDecimal = do 
  num <- many1(digit)
  char '.'
  frac <- many1(digit)
  let dec = num ++ "." ++ frac
  return $ Float $ fst $ Numeric.readFloat dec !! 0

{- TODO: implement full numeric stack, see Parsing, exercise #7 -}


parseEscapedChar = do 
  char '\\'
  c <- anyChar
  return $ case c of
    'n' -> '\n'
    't' -> '\t'
    'r' -> '\r'
    _   -> c

parseString :: Parser LispVal
parseString = do
	char '"'
	x <- many (parseEscapedChar <|> noneOf("\""))
	char '"'
	return $ String x

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
  head <- endBy parseExpr spaces
  tail <- char '.' >> spaces >> parseExpr
  return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do
  char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]

{- TODO: do exercises from parsing, bottom of page to add
 -       backquote, vector support
 - -}

parseExpr :: Parser LispVal
parseExpr = try(parseDecimal) 
  <|> parseNumber
  <|> parseChar
  <|> parseAtom
  <|> parseString 
  <|> parseQuoted
  <|> do char '('
         x <- try parseList <|> parseDottedList
         char ')'
         return x
  <?> "Expression"

{- Eval section -}
eval :: Env -> LispVal -> IOThrowsError LispVal
eval env val@(String _) = return val
eval env val@(Char _) = return val
eval env val@(Number _) = return val
eval env val@(Float _) = return val
eval env val@(Bool _) = return val
eval env (Atom id) = getVar env id
eval env (List [Atom "quote", val]) = return val
eval env (List [Atom "if", pred, conseq, alt]) = {- TODO: alt should be optional-} 
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

-- TODO: return nothing if all cond cases are false
eval env (List (Atom "cond" : clauses)) = 
    do let c =  clauses !! 0 -- First clause
       let cs = tail clauses -- other clauses
       test <- case c of
         List (Atom "else" : expr) -> eval env $ Bool True
         List (cond : expr) -> eval env cond
         -- TODO: some kind of error?  otherwise -> eval $ Bool False
       case test of
         Bool True -> evalCond env c
         otherwise -> eval env $ List $ (Atom "cond" : cs)

eval env (List (Atom "case" : keyAndClauses)) = 
    do let key = keyAndClauses !! 0
       let cls = tail keyAndClauses
       ekey <- eval env key
       evalCase env $ List $ (ekey : cls)

eval env (List [Atom "set!", Atom var, form]) = 
  eval env form >>= setVar env var

eval env (List [Atom "define", Atom var, form]) = 
  eval env form >>= defineVar env var

eval env (List [Atom "string-fill!", Atom var, character]) = do 
  str <- eval env =<< getVar env var
  chr <- eval env character
  (eval env $ fillStr(str, chr)) >>= setVar env var
  where fillStr (String str, Char chr) = doFillStr (String "", Char chr, length str)
  
        doFillStr (String str, Char chr, left) = do
        if left == 0
           then String str
           else doFillStr(String $ chr : str, Char chr, (left - 1))

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

eval env (List (Atom func : args)) = mapM (eval env) args >>= liftThrows . apply func
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

{- Was trying to use this for equality check in evalCase...
-- TODO: expand this definition
-- see: http://www.haskell.org/tutorial/classes.html
instance Eq LispVal where
  Number a == Number b = a == b
  _ == _ = False
{-    do test <- liftIO $ liftThrows $ eqv [a, b]
       case test of
         Bool True -> True
         otherwise -> False -}
-}

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe (throwError $ NotFunction "Unrecognized primitive function args" func)
                        ($ args)
                        (lookup func primitives)

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop(-)),
              ("*", numericBinop(*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem),

              ("=", numBoolBinop (==)),
              ("<", numBoolBinop (<)),
              (">", numBoolBinop (>)),
              ("/=", numBoolBinop (/=)),
              (">=", numBoolBinop (>=)),
              ("<=", numBoolBinop (<=)),
              ("&&", boolBoolBinop (&&)),
              ("||", boolBoolBinop (||)),
              ("string=?", strBoolBinop (==)),
              ("string<?", strBoolBinop (<)),
              ("string>?", strBoolBinop (>)),
              ("string<=?", strBoolBinop (<=)),
              ("string>=?", strBoolBinop (>=)),
              ("string-ci=?", stringCIEquals),
{- TODO:
              ("string-ci<?", strBoolBinop (<)),
              ("string-ci>?", strBoolBinop (>)),
              ("string-ci<=?", strBoolBinop (<=)),
              ("string-ci>=?", strBoolBinop (>=)),
-}
              ("car", car),
              ("cdr", cdr),
              ("cons", cons),
              ("eq?", eqv),
              ("eqv?", eqv),
              ("equal?", equal),

              ("pair?", isDottedList),
{- TODO:              ("procedure?", isProcedure),
              ("vector?", isVector),
			  TODO: full numeric tower: number?, complex?, real?, rational?, and integer?.
			  --}
              ("print", printLispVal),
              ("number?", isNumber),
              ("integer?", isInteger),
              ("real?", isReal),
              ("list?", isList),
              ("symbol?", isSymbol),
			  ("symbol->string", symbol2String),
			  ("string->symbol", string2Symbol),
              ("char?", isChar),

              ("string?", isString),
              ("string", buildString),
              ("make-string", makeString),
              ("string-length", stringLength),
              ("string-ref", stringRef),
              ("substring", substring),
              ("string-append", stringAppend),
              ("string->list", stringToList),
              ("list->string", listToString),
              ("string-copy", stringCopy),
-- TODO:              ("string-fill!", TBD),

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

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params = mapM unpackNum params >>= return . Number . foldl1 op

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
{- Obsolete code: unpackNum (String n) = let parsed = reads n in
                           if null parsed
                             then 0
                             else fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n-}
unpackNum notNum = throwError $ TypeMismatch "number" notNum

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

eqv :: [LispVal] -> ThrowsError LispVal
eqv [(Bool arg1), (Bool arg2)] = return $ Bool $ arg1 == arg2
eqv [(Number arg1), (Number arg2)] = return $ Bool $ arg1 == arg2
eqv [(Float arg1), (Float arg2)] = return $ Bool $ arg1 == arg2
eqv [(String arg1), (String arg2)] = return $ Bool $ arg1 == arg2
eqv [(Char arg1), (Char arg2)] = return $ Bool $ arg1 == arg2
eqv [(Atom arg1), (Atom arg2)] = return $ Bool $ arg1 == arg2
eqv [(DottedList xs x), (DottedList ys y)] = eqv [List $ xs ++ [x], List $ ys ++ [y]]
eqv [(List arg1), (List arg2)] = return $ Bool $ (length arg1 == length arg2) && 
                                                    (all eqvPair $ zip arg1 arg2)
    where eqvPair (x1, x2) = case eqv [x1, x2] of
                               Left err -> False
                               Right (Bool val) -> val
eqv [_, _] = return $ Bool False
eqv badArgList = throwError $ NumArgs 2 badArgList

{- TODO: fix bug from exercise #2:
 -       equal? has a bug in that a list of values is compared using eqv? instead of equal?.
 -       For example, (equal? '(1 "2") '(1 2)) = #f, while you'd expect it to be true. 
 - -}
equal :: [LispVal] -> ThrowsError LispVal
equal [arg1, arg2] = do
  primitiveEquals <- liftM or $ mapM (unpackEquals arg1 arg2)
                     [AnyUnpacker unpackNum, AnyUnpacker unpackStr, AnyUnpacker unpackBool]
  eqvEquals <- eqv [arg1, arg2]
  return $ Bool $ (primitiveEquals || let (Bool x) = eqvEquals in x)
equal badArgList = throwError $ NumArgs 2 badArgList

{- TODO: must be a better way to implement some of these... -}
printLispVal :: [LispVal] -> ThrowsError LispVal
printLispVal [lv] = 
  do let tmp = show lv
     return lv

-------------- String Functions --------------

buildString :: [LispVal] -> ThrowsError LispVal
buildString [(Char c)] = return $ String [c]
buildString (Char c:rest) = do
  cs <- buildString rest
  case cs of
    String s -> return $ String $ [c] ++ s
    badType -> throwError $ TypeMismatch "character" badType
buildString [badType] = throwError $ TypeMismatch "character" badType
buildString badArgList = throwError $ NumArgs 1 badArgList -- TODO: wrong error for this

makeString :: [LispVal] -> ThrowsError LispVal
makeString [(Number n)] = return $ doMakeString n ' ' ""
makeString [(Number n), (Char c)] = return $ doMakeString n c ""
makeString badArgList = throwError $ NumArgs 1 badArgList

doMakeString n chr s = 
    if n == 0 
       then String s
       else doMakeString (n - 1) chr (s ++ [chr])

stringLength :: [LispVal] -> ThrowsError LispVal
stringLength [String s] = return $ Number $ foldr (const (+1)) 0 s -- TODO: length s
stringLength [badType] = throwError $ TypeMismatch "string" badType
stringLength badArgList = throwError $ NumArgs 1 badArgList
-- TODO: type error? arg error?

stringRef :: [LispVal] -> ThrowsError LispVal
stringRef [(String s), (Number k)] = return $ Char $ s !! fromInteger k
stringRef [badType] = throwError $ TypeMismatch "string number" badType
stringRef badArgList = throwError $ NumArgs 2 badArgList
-- TODO: error?

substring :: [LispVal] -> ThrowsError LispVal
substring [(String s), (Number start), (Number end)] = 
  do let length = fromInteger $ end - start
     let begin = fromInteger start 
     return $ String $ (take length . drop begin) s
-- TODO: error handling

stringCIEquals :: [LispVal] -> ThrowsError LispVal
stringCIEquals [(String s1), (String s2)] = return $ Bool $ s1 == s2 -- TODO: needs to ignore case

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

stringToList :: [LispVal] -> ThrowsError LispVal
stringToList [(String s)] = return $ List $ map (Char) s
stringToList [badType] = throwError $ TypeMismatch "string" badType
stringToList badArgList = throwError $ NumArgs 1 badArgList

-- TODO: this is not working yet
listToString :: [LispVal] -> ThrowsError LispVal
listToString [(List l)] = return $ String "TODO" 
listToString [badType] = throwError $ TypeMismatch "string" badType
listTostring badArgList = throwError $ NumArgs 1 badArgList

stringCopy :: [LispVal] -> ThrowsError LispVal
stringCopy [String s] = return $ String s
stringCopy [badType] = throwError $ TypeMismatch "string" badType
stringCopy badArgList = throwError $ NumArgs 2 badArgList

isNumber :: [LispVal] -> ThrowsError LispVal
isNumber ([Number n]) = return $ Bool True
isNumber ([Float f]) = return $ Bool True
isNumber _ = return $ Bool False

isReal :: [LispVal] -> ThrowsError LispVal
isReal ([Number n]) = return $ Bool True
isReal ([Float f]) = return $ Bool True
isReal _ = return $ Bool False

isInteger :: [LispVal] -> ThrowsError LispVal
isInteger ([Number n]) = return $ Bool True
isInteger _ = return $ Bool False

isDottedList :: [LispVal] -> ThrowsError LispVal
isDottedList ([DottedList l d]) = return $ Bool True {- TODO: review code to convince myself why this works -}
isDottedList _ = return $  Bool False

isList :: [LispVal] -> ThrowsError LispVal
isList ([List a]) = return $ Bool True
isList _ = return $ Bool False

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
{- end Eval section-}


