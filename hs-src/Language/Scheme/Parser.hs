{- |
Module      : Language.Scheme.Parser
Copyright   : Justin Ethier
Licence     : MIT (see LICENSE in the distribution)

Maintainer  : github.com/justinethier
Stability   : experimental
Portability : portable

husk scheme interpreter

A lightweight dialect of R5RS scheme.

This module implements parsing of Scheme code.
-}

module Language.Scheme.Parser where
import Language.Scheme.Types
import Control.Monad.Error
import Char
import Complex
import Data.Array
import Numeric
import Ratio
import Text.ParserCombinators.Parsec hiding (spaces)
import Text.Parsec.Language
import qualified Text.Parsec.Token as P

lispDef :: LanguageDef ()
lispDef 
  = emptyDef    
  { P.commentStart   = "#|"
  , P.commentEnd     = "|#"
  , P.commentLine    = ";"
  , P.nestedComments = True
  , P.identStart     = letter <|> symbol
  , P.identLetter    = letter <|> digit <|> symbol
  , P.reservedNames  = []
  , P.caseSensitive  = True
  } 

lexer = P.makeTokenParser lispDef
dot = P.dot lexer
parens = P.parens lexer
identifier = P.identifier lexer
-- TODO: typedef. starting point was: whiteSpace :: CharParser ()
whiteSpace = P.whiteSpace lexer
lexeme = P.lexeme lexer

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~."

parseAtom :: Parser LispVal
parseAtom = do
  atom <- identifier
  if atom == "."
     then pzero -- Do not match this form
     else return $ Atom atom

parseBool :: Parser LispVal
parseBool = do _ <- string "#"
               x <- oneOf "tf"
               return $ case x of
                          't' -> Bool True
                          'f' -> Bool False
                          _ -> Bool False

parseChar :: Parser LispVal
parseChar = do
  _ <- try (string "#\\")
  c <- anyChar
  r <- many (letter)
  let pchr = c : r
  return $ case pchr of
    "space" -> Char ' '
    "newline" -> Char '\n'
    _ -> Char c

parseOctalNumber :: Parser LispVal
parseOctalNumber = do
  _ <- try (string "#o")
  sign <- many (oneOf "-")
  num <- many1 (oneOf "01234567")
  case (length sign) of
     0 -> return $ Number $ fst $ Numeric.readOct num !! 0
     1 -> return $ Number $ fromInteger $ (*) (-1) $ fst $ Numeric.readOct num !! 0
     _ -> pzero

parseBinaryNumber :: Parser LispVal
parseBinaryNumber = do
  _ <- try (string "#b")
  sign <- many (oneOf "-")
  num <- many1 (oneOf "01")
  case (length sign) of
     0 -> return $ Number $ fst $ Numeric.readInt 2 (`elem` "01") Char.digitToInt num !! 0
     1 -> return $ Number $ fromInteger $ (*) (-1) $ fst $ Numeric.readInt 2 (`elem` "01") Char.digitToInt num !! 0
     _ -> pzero

parseHexNumber :: Parser LispVal
parseHexNumber = do
  _ <- try (string "#x")
  sign <- many (oneOf "-")
  num <- many1 (digit <|> oneOf "abcdefABCDEF")
  case (length sign) of
     0 -> return $ Number $ fst $ Numeric.readHex num !! 0
     1 -> return $ Number $ fromInteger $ (*) (-1) $ fst $ Numeric.readHex num !! 0
     _ -> pzero

-- |Parser for Integer, base 10
parseDecimalNumber :: Parser LispVal
parseDecimalNumber = do
  _ <- try (many (string "#d"))
  sign <- many (oneOf "-")
  num <- many1 (digit)
  if (length sign) > 1
     then pzero
     else return $ (Number . read) $ sign ++ num

parseNumber :: Parser LispVal
parseNumber = parseDecimalNumber <|>
              parseHexNumber <|>
              parseBinaryNumber <|>
              parseOctalNumber <?>
              "Unable to parse number"

{- Parser for floating points
 - -}
parseRealNumber :: Parser LispVal
parseRealNumber = do
  sign <- many (oneOf "-+")
  num <- many1 (digit)
  _ <- char '.'
  frac <- many1 (digit)
  let dec = num ++ "." ++ frac
  case (length sign) of
     0 -> do
              let numbr = fst $ Numeric.readFloat dec !! 0
-- expnt <- try (char 'e')
              return $ Float $ numbr
{- FUTURE: Issue #14: parse numbers in format #e1e10
 -
              expnt <- try (char 'e')
              case expnt of
--                'e' -> return $ Float $ numbr
                _ -> return $ Float $ numbr
return $ Float $ fst $ Numeric.readFloat dec !! 0 -}

-- TODO: this is a hack, but need to support the + sign as well as the minus.

     1 -> if sign == "-" 
             then return $ Float $ (*) (-1.0) $ fst $ Numeric.readFloat dec !! 0
             else return $ Float $ fst $ Numeric.readFloat dec !! 0
     _ -> pzero

parseRationalNumber :: Parser LispVal
parseRationalNumber = do
  pnumerator <- parseDecimalNumber
  case pnumerator of
    Number n -> do
      _ <- char '/'
      sign <- many (oneOf "-")
      num <- many1 (digit)
      if (length sign) > 1
         then pzero
         else return $ Rational $ n % (read $ sign ++ num)
    _ -> pzero

parseComplexNumber :: Parser LispVal
parseComplexNumber = do
  lispreal <- (try (parseRealNumber) <|> try (parseRationalNumber) <|> parseDecimalNumber)
  let real = case lispreal of
                  Number n -> fromInteger n
                  Rational r -> fromRational r
                  Float f -> f
                  _ -> 0
  _ <- char '+'
  lispimag <- (try (parseRealNumber) <|> try (parseRationalNumber) <|> parseDecimalNumber)
  let imag = case lispimag of
                  Number n -> fromInteger n
                  Rational r -> fromRational r
                  Float f -> f
                  _ -> 0 -- Case should never be reached
  _ <- char 'i'
  return $ Complex $ real :+ imag

parseEscapedChar :: forall st .
                    GenParser Char st Char
parseEscapedChar = do
  _ <- char '\\'
  c <- anyChar
  return $ case c of
    'n' -> '\n'
    't' -> '\t'
    'r' -> '\r'
    _ -> c

parseString :: Parser LispVal
parseString = do
  _ <- char '"'
  x <- many (parseEscapedChar <|> noneOf ("\""))
  _ <- char '"'
  return $ String x

parseVector :: Parser LispVal
parseVector = do
  vals <- sepBy parseExpr whiteSpace
  return $ Vector (listArray (0, (length vals - 1)) vals)

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr whiteSpace
-- TODO: wanted to use endBy (or a variant) above, but it causes an error such that dotted lists are not parsed

parseDottedList :: Parser LispVal
parseDottedList = do
  phead <- endBy parseExpr whiteSpace
  ptail <- dot >> parseExpr --char '.' >> whiteSpace >> parseExpr
--  return $ DottedList phead ptail
  case ptail of
    DottedList ls l -> return $ DottedList (phead ++ ls) l 
    -- TODO: Issue #41 - List l -> return $ List $ phead ++ l
    {- Regarding above, see http://community.schemewiki.org/?scheme-faq-language#dottedapp
     
       Note, however, that most Schemes expand literal lists occurring in function applications, 
       e.g. (foo bar . (1 2 3)) is expanded into (foo bar 1 2 3) by the reader. It is not entirely 
       clear whether this is a consequence of the standard - the notation is not part of the R5RS 
       grammar but there is strong evidence to suggest a Scheme implementation cannot comply with 
       all of R5RS without performing this transformation. -}
    _ -> return $ DottedList phead ptail

parseQuoted :: Parser LispVal
parseQuoted = do
  _ <- lexeme $ char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]

parseQuasiQuoted :: Parser LispVal
parseQuasiQuoted = do
  _ <- lexeme $ char '`'
  x <- parseExpr
  return $ List [Atom "quasiquote", x]

parseUnquoted :: Parser LispVal
parseUnquoted = do
  _ <- try (lexeme $ char ',')
  x <- parseExpr
  return $ List [Atom "unquote", x]

parseUnquoteSpliced :: Parser LispVal
parseUnquoteSpliced = do
  _ <- try (lexeme $ string ",@")
  x <- parseExpr
  return $ List [Atom "unquote-splicing", x]

parseExpr :: Parser LispVal
parseExpr =
      try (lexeme parseComplexNumber)
  <|> try (lexeme parseRationalNumber)
  <|> try (lexeme parseRealNumber)
  <|> try (lexeme parseNumber)
  <|> lexeme parseChar
  <|> parseUnquoteSpliced
  <|> do _ <- try (lexeme $ string "#(")
         x <- parseVector
         _ <- lexeme $ char ')'
         return x
  <|> try (parseAtom)
  <|> lexeme parseString
  <|> lexeme parseBool
  <|> parseQuoted
  <|> parseQuasiQuoted
  <|> parseUnquoted
  <|> try (parens parseList)
  <|> parens parseDottedList
  <?> "Expression"

mainParser :: Parser LispVal
mainParser = do
    _ <- whiteSpace
    x <- parseExpr
-- FUTURE? (seemed to break test cases, but is supposed to be best practice?)    eof
    return x

readOrThrow :: Parser a -> String -> ThrowsError a
readOrThrow parser input = case parse parser "lisp" input of
  Left err -> throwError $ Parser err
  Right val -> return val

readExpr :: String -> ThrowsError LispVal
readExpr = readOrThrow mainParser

readExprList :: String -> ThrowsError [LispVal]
readExprList = readOrThrow (endBy mainParser whiteSpace)

