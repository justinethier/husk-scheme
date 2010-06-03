{-
 - Scheme Parser
 -
 - @author Justin Ethier
 - -}
module Main where
import Control.Monad
import Control.Monad.Error
import Char
import Numeric
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)


main :: IO ()
main = getArgs >>= print . eval . readExpr . head
{-
do
	args <- getArgs
	putStrLn (readExpr (args !! 0))-}

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
eval :: LispVal -> LispVal
eval val@(String _) = val
eval val@(Char _) = val
eval val@(Number _) = val
eval val@(Float _) = val
eval val@(Bool _) = val
eval (List [Atom "quote", val]) = val

eval (List (Atom func : args)) = apply func $ map eval args

apply :: String -> [LispVal] -> LispVal
apply func args = maybe (Bool False) ($ args) $ lookup func primitives

primitives :: [(String, [LispVal] -> LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop(-)),
              ("*", numericBinop(*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem),
              ("pair?", isDottedList),
{- TODO:              ("procedure?", isProcedure),
              ("vector?", isVector),
			  TODO: full numeric tower: number?, complex?, real?, rational?, and integer?.
			  --}
              ("number?", isNumber),
              ("integer?", isInteger),
              ("real?", isReal),
              ("list?", isList),
              ("symbol?", isSymbol),
			  ("symbol->string", symbol2String),
			  ("string->symbol", string2Symbol),
              ("char?", isChar),
              ("string?", isString),
              ("boolean?", isBoolean)]

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinop op params = Number $ foldl1 op $ map unpackNum params

unpackNum :: LispVal -> Integer
unpackNum (Number n) = n
{- Obsolete code: unpackNum (String n) = let parsed = reads n in
                           if null parsed
                             then 0
                             else fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n-}
unpackNum _ = 0

{- TODO: must be a better way to implement some of these... -}
isNumber :: [LispVal] -> LispVal
isNumber ([Number n]) = Bool True
isNumber ([Float f]) = Bool True
isNumber _ = Bool False

isReal :: [LispVal] -> LispVal
isReal ([Number n]) = Bool True
isReal ([Float f]) = Bool True
isReal _ = Bool False

isInteger :: [LispVal] -> LispVal
isInteger ([Number n]) = Bool True
isInteger _ = Bool False

isDottedList :: [LispVal] -> LispVal
isDottedList ([DottedList l d]) = Bool True {- TODO: review code to convince myself why this works -}
isDottedList _ = Bool False

isList :: [LispVal] -> LispVal
isList ([List a]) = Bool True
isList _ = Bool False

isSymbol :: [LispVal] -> LispVal
isSymbol ([Atom a]) = Bool True
isSymbol _ = Bool False

symbol2String :: [LispVal] -> LispVal
symbol2String ([Atom a]) = String a

string2Symbol :: [LispVal] -> LispVal
string2Symbol ([String s]) = Atom s

isChar :: [LispVal] -> LispVal
isChar ([Char a]) = Bool True
isChar _ = Bool False

isString :: [LispVal] -> LispVal
isString ([String s]) = Bool True
isString _ = Bool False

isBoolean :: [LispVal] -> LispVal
isBoolean ([Bool n]) = Bool True
isBoolean _ = Bool False
{- end Eval section-}

readExpr :: String -> LispVal
readExpr input = case parse parseExpr "lisp" input of
	Left err -> String $ "No match: " ++ show err
	Right val -> val {-"Found " ++ show val-}

