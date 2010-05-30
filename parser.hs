{-
 - Scheme Parser
 -
 - @author Justin Ethier
 - -}
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
	| Char Char
	| Bool Bool

showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Char chr) = "chr: " ++ [chr] {- TODO: this is only temporary, for testing-}
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

parseChar :: Parser LispVal
parseChar = do
  string "#\\"
  c <- anyChar 
  r <- many(letter)
  let chr = c:r
  return $ case chr of
    "space"   -> Char ' '
    "newline" -> Char '\n'
    _         -> Char c {- TODO: err if invalid char -}

{- TODO: #d, #x broken right now, since atom matches them
 - TODO: add #o, #b support
 - -}

parseHexNumber :: Parser LispVal
parseHexNumber = do
  string "#x"
  num <- many(digit <|> oneOf "abcdefABCDEF")
  return $ Number $ fst $ readHex num !! 0

parseDecimalNumber :: Parser LispVal
parseDecimalNumber = do
  string "#d" <|> string ""
  num <- many (digit)
  return $ (Number . read) $ num

parseNumber :: Parser LispVal
parseNumber = {-parseHexNumber <|>-} parseDecimalNumber <?> "Unable to parse number"

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
parseExpr = try  
  parseChar <|> 
  parseAtom <|> 
  parseString <|> 
  parseNumber  <?> 
  "Expression"

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
	Left err -> "No match: " ++ show err
	Right val -> "Found " ++ show val

