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

parseString :: Parser LispVal
parseString = do
	char '"'
	x <- many ("\"" <|> noneOf "\"")
	char '"'
	return $ String x

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

parseExpr :: Parser LispVal
parseExpr = parseAtom <|> parseString <|> parseNumber

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
	Left err -> "No match: " ++ show err
	Right val -> "Found value"

