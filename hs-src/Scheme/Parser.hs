{-
 - husk scheme
 - Parser
 -
 - This file contains the code for parsing scheme
 -
 - @author Justin Ethier
 -
 - -}
module Scheme.Parser where
import Scheme.Types
import Control.Monad.Error
import Char
import Complex
import Data.Array
import Numeric
import Ratio
import Text.ParserCombinators.Parsec hiding (spaces)

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~" -- TODO: I removed #, make sure this is OK w/spec, and test cases

spaces :: Parser ()
spaces = skipMany1 space

parseAtom :: Parser LispVal
parseAtom = do
	first <- letter <|> symbol <|> (oneOf ".")
	rest <- many (letter <|> digit <|> symbol <|> (oneOf "."))
	let atom = first:rest
	if atom == "."
           then pzero -- Do not match this form
           else return $ Atom atom

parseBool :: Parser LispVal
parseBool = do string "#"
               x <- oneOf "tf"
               return $ case x of
                          't' -> Bool True
                          'f' -> Bool False
                          _ -> Bool False

parseChar :: Parser LispVal
parseChar = do
  try (string "#\\")
  c <- anyChar 
  r <- many(letter)
  let pchr = c:r
  return $ case pchr of
    "space"   -> Char ' '
    "newline" -> Char '\n'
    _         -> Char c {- TODO: err if invalid char -}

parseOctalNumber :: Parser LispVal
parseOctalNumber = do
  try (string "#o")
  sign <- many (oneOf "-")
  num <- many1(oneOf "01234567")
  case (length sign) of
     0 -> return $ Number $ fst $ Numeric.readOct num !! 0
     1 -> return $ Number $ fromInteger $ (*) (-1) $ fst $ Numeric.readOct num !! 0
     _ -> pzero

parseBinaryNumber :: Parser LispVal
parseBinaryNumber = do
  try (string "#b")
  sign <- many (oneOf "-")
  num <- many1(oneOf "01")
  case (length sign) of
     0 -> return $ Number $ fst $ Numeric.readInt 2 (`elem` "01") Char.digitToInt num !! 0
     1 -> return $ Number $ fromInteger $ (*) (-1) $ fst $ Numeric.readInt 2 (`elem` "01") Char.digitToInt num !! 0
     _ -> pzero

parseHexNumber :: Parser LispVal
parseHexNumber = do
  try (string "#x")
  sign <- many (oneOf "-")
  num <- many1(digit <|> oneOf "abcdefABCDEF")
  case (length sign) of
     0 -> return $ Number $ fst $ Numeric.readHex num !! 0 
     1 -> return $ Number $ fromInteger $ (*) (-1) $ fst $ Numeric.readHex num !! 0
     _ -> pzero

-- |Parser for Integer, base 10
parseDecimalNumber :: Parser LispVal
parseDecimalNumber = do
  try (many(string "#d"))
  sign <- many (oneOf "-")
  num <- many1 (digit)
  if (length sign) > 1
     then pzero
     else return $ (Number . read) $ sign ++ num

parseNumber :: Parser LispVal
parseNumber = parseDecimalNumber <|> 
              parseHexNumber     <|> 
              parseBinaryNumber  <|> 
              parseOctalNumber   <?> 
              "Unable to parse number"

{- Parser for floating points 
 -
 - TODO: parse numbers in format #e1e10
 - TODO: bug - 
 -           huski> (string->number "3.42323+2i")
 -           3.42323
 - -}
parseRealNumber :: Parser LispVal
parseRealNumber = do 
  sign <- many (oneOf "-")
  num <- many1(digit)
  char '.'
  frac <- many1(digit)
  let dec = num ++ "." ++ frac
  case (length sign) of
     0 -> return $ Float $ fst $ Numeric.readFloat dec !! 0
     1 -> return $ Float $ (*) (-1.0) $ fst $ Numeric.readFloat dec !! 0
     _ -> pzero

parseRationalNumber :: Parser LispVal
parseRationalNumber = do
  pnumerator <- parseDecimalNumber
  case pnumerator of 
    Number n -> do
      char '/'
      sign <- many (oneOf "-")
      num <- many1 (digit)
      if (length sign) > 1
         then pzero
         else return $ Rational $ n % (read $ sign ++ num)
    _ -> pzero

parseComplexNumber :: Parser LispVal
parseComplexNumber = do
  lispreal <- (try(parseRealNumber) <|> parseDecimalNumber)
  let real = case lispreal of
                  Number n -> fromInteger n
                  Float f -> f
                  _ -> 0
  char '+'
  lispimag <- (try(parseRealNumber) <|> parseDecimalNumber)
  let imag = case lispimag of
                  Number n -> fromInteger n
                  Float f -> f
                  _ -> 0 -- Case should never be reached
  char 'i'
  return $ Complex $ real :+ imag

parseEscapedChar :: forall st.
                    GenParser Char st Char
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

parseVector :: Parser LispVal
parseVector = do
  vals <- sepBy parseExpr spaces
  return $ Vector (listArray (0, (length vals - 1)) vals)
-- TODO: old code from Data.Vector implementation:  return $ Vector $ Data.Vector.fromList vals

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
  phead <- endBy parseExpr spaces
  ptail <- char '.' >> spaces >> parseExpr
  return $ DottedList phead ptail

parseQuoted :: Parser LispVal
parseQuoted = do
  char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]

parseQuasiQuoted :: Parser LispVal
parseQuasiQuoted = do
  char '`'
  x <- parseExpr
  return $ List [Atom "quasiquote", x]

parseUnquoted :: Parser LispVal
parseUnquoted = do
  try (char ',')
  x <- parseExpr
  return $ List [Atom "unquote", x]

parseUnquoteSpliced :: Parser LispVal
parseUnquoteSpliced = do
  try (string ",@")
  x <- parseExpr
  return $ List [Atom "unquote-splicing", x]


-- Comment parser
-- TODO: this is a hack, it should really not return anything...
parseComment :: Parser LispVal
parseComment = do
  char ';'
  many (noneOf ("\n"))
  return $ Nil ""


parseExpr :: Parser LispVal
parseExpr = 
      try(parseRationalNumber)
  <|> try(parseComplexNumber)
  <|> parseComment
  <|> try(parseRealNumber)
  <|> try(parseNumber)
  <|> parseChar
  <|> parseUnquoteSpliced
  <|> do try (string "#(")
         x <- parseVector
         char ')'
         return x
  <|> try (parseAtom)
  <|> parseString 
  <|> parseBool
  <|> parseQuoted
  <|> parseQuasiQuoted
  <|> parseUnquoted
  <|> do char '('
         x <- try parseList <|> parseDottedList
         char ')'
         return x
  <?> "Expression"

readOrThrow :: Parser a -> String -> ThrowsError a
readOrThrow parser input = case parse parser "lisp" input of
	Left err -> throwError $ Parser err
	Right val -> return val

readExpr :: String -> ThrowsError LispVal
readExpr = readOrThrow parseExpr

readExprList :: String -> ThrowsError [LispVal]
readExprList = readOrThrow (endBy parseExpr spaces)

