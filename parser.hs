{- TODO: finish "lesson" 2, start again at Return Values -}
module Main where
import Control.Monad
import Numeric
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)


main :: IO ()
main = do
	args <- getArgs
	putStrLn (readExpr (args !! 0))

data LispVal = Atom String
	| List [LispVal]
	| DottedList [LispVal] LispVal
	| Number Integer
 	| String String
	| Bool Bool

showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
{-showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"-}

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

parseNumber :: Parser LispVal
parseNumber = do
{- TODO: perhaps too much going on here, break out into sep bool, hex, dec, oct parsers -}
  pre <- try ( string "#b" <|> string "#o" <|> string "#x" <|> string "" )
  num <- many (digit)
  return $ case pre of
    "#x" -> Number $ fst $ readHex num !! 0
    _    -> (Number . read) $ num
{-	num <- many (digit)
	return $ (Number . read) $ num -}
{- Orig version from the wiki - parseNumber = liftM (Number . read) $ many1 digit -}


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

parseExpr :: Parser LispVal
parseExpr = parseNumber <|> parseAtom <|> parseString {-<|> parseNumber-}

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
	Left err -> "No match: " ++ show err
	Right val -> "Found " ++ show val

