{- TODO: finish "lesson" 2, start again at Return Values -}
module Main where
import Control.Monad
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

{- Allow to convert lispval instances into strings  -}
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
	num <- many (digit)
	return $ (Number . read) $ num
{- Orig version from the wiki - parseNumber = liftM (Number . read) $ many1 digit -}


parseStringSingleChar = do
	fc <- noneOf "\"" 
	return fc

{- TODO: replacement for noneOf, accept char or \"
 -
 - could read anychar, and if it is a backspace, then
 - read next char and if it is ", t, etc then convert both.
 - then return char.
 -
 - TODO: how to then parse the next char in seq? just call anychar
 - again??
 -
 - -}

parseString :: Parser LispVal
parseString = do
	char '"'
	x <- many (parseStringSingleChar)
	char '"'
	return $ String x

parseExpr :: Parser LispVal
parseExpr = parseAtom <|> parseString <|> parseNumber

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
	Left err -> "No match: " ++ show err
	Right val -> "Found " ++ show val

