{-
 - Scheme Parser
 -
 - @author Justin Ethier
 - -}
module Main where
import Control.Monad
import Char
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
	| Float Float
 	| String String
	| Char Char
	| Bool Bool

showVal :: LispVal -> String
showVal (String contents) = "str - \"" ++ contents ++ "\""
showVal (Char chr) = "chr: " ++ [chr] {- TODO: this is only temporary, for testing-}
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Float contents) = show contents
showVal (Bool True) = "TODO: #t"
showVal (Bool False) = "TODO: #f"
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
  try (string "#\\")
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
parseExpr =   
  try(parseDecimal) <|>
  parseNumber  <|>
  parseChar <|> 
  parseAtom <|>
  parseString <?> 
  "Expression"

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
	Left err -> "No match: " ++ show err
	Right val -> "Found " ++ show val

