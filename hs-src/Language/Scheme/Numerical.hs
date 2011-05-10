{- | 
Module      : Language.Scheme.Numerical
Copyright   : Justin Ethier
Licence     : MIT (see LICENSE in the distribution)

Maintainer  : github.com/justinethier
Stability   : experimental
Portability : portable

husk scheme interpreter

A lightweight dialect of R5RS scheme.

This module implements the numerical tower.
-}

module Language.Scheme.Numerical where
import Language.Scheme.Types
import Complex
import Control.Monad.Error
import Data.Char
import Numeric 
import Ratio
import Text.Printf

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop _ singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op aparams = mapM unpackNum aparams >>= return . Number . foldl1 op

--- Begin GenUtil - http://repetae.net/computer/haskell/GenUtil.hs
foldlM :: Monad m => (a -> b -> m a) -> a -> [b] -> m a
foldlM f v (x:xs) = (f v x) >>= \a -> foldlM f a xs
foldlM _ v [] = return v

foldl1M :: Monad m => (a -> a -> m a) ->  [a] -> m a
foldl1M f (x:xs) = foldlM f x xs
foldl1M _ _ = error "Unexpected error in foldl1M"
-- end GenUtil


-- FUTURE: as a general comment here, operations need to be more permissive of the
--         numerical types they accept. Within reason, a user should not have to know
--         what numerical type they are passing when using these functions


numAdd, numSub, numMul, numDiv :: [LispVal] -> ThrowsError LispVal
numAdd [] = return $ Number 0
numAdd aparams = do
  foldl1M (\a b -> doAdd =<< (numCast [a, b])) aparams
  where doAdd (List [(Number a), (Number b)]) = return $ Number $ a + b
        doAdd (List [(Float a), (Float b)]) = return $ Float $ a + b
        doAdd (List [(Rational a), (Rational b)]) = return $ Rational $ a + b
        doAdd (List [(Complex a), (Complex b)]) = return $ Complex $ a + b
        doAdd _ = throwError $ Default "Unexpected error in +"
numSub [] = throwError $ NumArgs 1 [] 
numSub [Number n] = return $ Number $ -1 * n
numSub [Float n] = return $ Float $ -1 * n
numSub [Rational n] = return $ Rational $ -1 * n
numSub [Complex n] = return $ Complex $ -1 * n
numSub aparams = do
  foldl1M (\a b -> doSub =<< (numCast [a, b])) aparams
  where doSub (List [(Number a), (Number b)]) = return $ Number $ a - b
        doSub (List [(Float a), (Float b)]) = return $ Float $ a - b
        doSub (List [(Rational a), (Rational b)]) = return $ Rational $ a - b
        doSub (List [(Complex a), (Complex b)]) = return $ Complex $ a - b
        doSub _ = throwError $ Default "Unexpected error in -"
numMul [] = return $ Number 1 
numMul aparams = do 
  foldl1M (\a b -> doMul =<< (numCast [a, b])) aparams
  where doMul (List [(Number a), (Number b)]) = return $ Number $ a * b
        doMul (List [(Float a), (Float b)]) = return $ Float $ a * b
        doMul (List [(Rational a), (Rational b)]) = return $ Rational $ a * b
        doMul (List [(Complex a), (Complex b)]) = return $ Complex $ a * b
        doMul _ = throwError $ Default "Unexpected error in *"
numDiv [] = throwError $ NumArgs 1 [] 
numDiv [Number n] = return $ Rational $ 1 / (fromInteger n)
numDiv [Float n] = return $ Float $ 1.0 / n
numDiv [Rational n] = return $ Rational $ 1 / n
numDiv [Complex n] = return $ Complex $ 1 / n
numDiv aparams = do 
  foldl1M (\a b -> doDiv =<< (numCast [a, b])) aparams
  where doDiv (List [(Number a), (Number b)]) = if b == 0 
                                                   then throwError $ DivideByZero 
                                                   else if (mod a b) == 0 
                                                           then return $ Number $ div a b
                                                           -- Convert to a rational if the result is not an integer
                                                           else return $ Rational $ (fromInteger a) / (fromInteger b)
        doDiv (List [(Float a), (Float b)]) = if b == 0.0 
                                                   then throwError $ DivideByZero 
                                                   else return $ Float $ a / b
        doDiv (List [(Rational a), (Rational b)]) = if b == 0
                                                       then throwError $ DivideByZero 
                                                       else return $ Rational $ a / b
        doDiv (List [(Complex a), (Complex b)]) = if b == 0
                                                       then throwError $ DivideByZero 
                                                       else return $ Complex $ a / b
        doDiv _ = throwError $ Default "Unexpected error in /"

numBoolBinopEq :: [LispVal] -> ThrowsError LispVal
numBoolBinopEq [] = throwError $ NumArgs 0 []
numBoolBinopEq aparams = do 
  foldl1M (\a b -> doOp =<< (numCast [a, b])) aparams
  where doOp (List [(Number a), (Number b)]) = return $ Bool $ a == b
        doOp (List [(Float a), (Float b)]) = return $ Bool $ a == b
        doOp (List [(Rational a), (Rational b)]) = return $ Bool $ a == b
        doOp (List [(Complex a), (Complex b)]) = return $ Bool $ a == b
        doOp _ = throwError $ Default "Unexpected error in =" 

numBoolBinopGt :: [LispVal] -> ThrowsError LispVal
numBoolBinopGt [] = throwError $ NumArgs 0 []
numBoolBinopGt aparams = do 
  foldl1M (\a b -> doOp =<< (numCast [a, b])) aparams
  where doOp (List [(Number a), (Number b)]) = return $ Bool $ a > b
        doOp (List [(Float a), (Float b)]) = return $ Bool $ a > b
        doOp (List [(Rational a), (Rational b)]) = return $ Bool $ a > b
        doOp _ = throwError $ Default "Unexpected error in >" 

numBoolBinopGte :: [LispVal] -> ThrowsError LispVal
numBoolBinopGte [] = throwError $ NumArgs 0 []
numBoolBinopGte aparams = do 
  foldl1M (\a b -> doOp =<< (numCast [a, b])) aparams
  where doOp (List [(Number a), (Number b)]) = return $ Bool $ a >= b
        doOp (List [(Float a), (Float b)]) = return $ Bool $ a >= b
        doOp (List [(Rational a), (Rational b)]) = return $ Bool $ a >= b
        doOp _ = throwError $ Default "Unexpected error in >=" 

numBoolBinopLt :: [LispVal] -> ThrowsError LispVal
numBoolBinopLt [] = throwError $ NumArgs 0 []
numBoolBinopLt aparams = do 
  foldl1M (\a b -> doOp =<< (numCast [a, b])) aparams
  where doOp (List [(Number a), (Number b)]) = return $ Bool $ a < b
        doOp (List [(Float a), (Float b)]) = return $ Bool $ a < b
        doOp (List [(Rational a), (Rational b)]) = return $ Bool $ a < b
        doOp _ = throwError $ Default "Unexpected error in <" 

numBoolBinopLte :: [LispVal] -> ThrowsError LispVal
numBoolBinopLte [] = throwError $ NumArgs 0 []
numBoolBinopLte aparams = do 
  foldl1M (\a b -> doOp =<< (numCast [a, b])) aparams
  where doOp (List [(Number a), (Number b)]) = return $ Bool $ a <= b
        doOp (List [(Float a), (Float b)]) = return $ Bool $ a <= b
        doOp (List [(Rational a), (Rational b)]) = return $ Bool $ a <= b
        doOp _ = throwError $ Default "Unexpected error in <=" 

numCast :: [LispVal] -> ThrowsError LispVal
numCast [a@(Number _), b@(Number _)] = return $ List [a, b]
numCast [a@(Float _), b@(Float _)] = return $ List [a, b]
numCast [a@(Rational _), b@(Rational _)] = return $ List [a, b]
numCast [a@(Complex _), b@(Complex _)] = return $ List [a, b]
numCast [(Number a), b@(Float _)] = return $ List [Float $ fromInteger a, b]
numCast [(Number a), b@(Rational _)] = return $ List [Rational $ fromInteger a, b]
numCast [(Number a), b@(Complex _)] = return $ List [Complex $ fromInteger a, b]
numCast [a@(Float _), (Number b)] = return $ List [a, Float $ fromInteger b]
numCast [a@(Float _), (Rational b)] = return $ List [a, Float $ fromRational b]
numCast [(Float a), b@(Complex _)] = return $ List [Complex $ a :+ 0, b]
numCast [a@(Rational _), (Number b)] = return $ List [a, Rational $ fromInteger b]
numCast [(Rational a), b@(Float _)] = return $ List [Float $ fromRational a, b]
numCast [(Rational a), b@(Complex _)] = return $ List [Complex $ (fromInteger $ numerator a) / (fromInteger $ denominator a), b]
numCast [a@(Complex _), (Number b)] = return $ List [a, Complex $ fromInteger b]
numCast [a@(Complex _), (Float b)] = return $ List [a, Complex $ b :+ 0]
numCast [a@(Complex _), (Rational b)] = return $ List [a, Complex $ (fromInteger $ numerator b) / (fromInteger $ denominator b)]
numCast [a, b] = case a of 
               Number _   -> doThrowError b
               Float _    -> doThrowError b
               Rational _ -> doThrowError b
               Complex _  -> doThrowError b
               _          -> doThrowError a
  where doThrowError num = throwError $ TypeMismatch "number" num
numCast _ = throwError $ Default "Unexpected error in numCast"

numRound, numFloor, numCeiling, numTruncate :: [LispVal] -> ThrowsError LispVal
numRound [n@(Number _)] = return n
numRound [(Rational n)] = return $ Number $ round n
numRound [(Float n)] = return $ Float $ fromInteger $ round n
numRound [(Complex n)] = do
  return $ Complex $ (fromInteger $ round $ realPart n) :+ (fromInteger $ round $ imagPart n)
numRound [x] = throwError $ TypeMismatch "number" x
numRound badArgList = throwError $ NumArgs 1 badArgList

numFloor [n@(Number _)] = return n
numFloor [(Rational n)] = return $ Number $ floor n
numFloor [(Float n)] = return $ Float $ fromInteger $ floor n
numFloor [(Complex n)] = do
  return $ Complex $ (fromInteger $ floor $ realPart n) :+ (fromInteger $ floor $ imagPart n)
numFloor [x] = throwError $ TypeMismatch "number" x
numFloor badArgList = throwError $ NumArgs 1 badArgList

numCeiling [n@(Number _)] = return n
numCeiling [(Rational n)] = return $ Number $ ceiling n
numCeiling [(Float n)] = return $ Float $ fromInteger $ ceiling n
numCeiling [(Complex n)] = do
  return $ Complex $ (fromInteger $ ceiling $ realPart n) :+ (fromInteger $ ceiling $ imagPart n)
numCeiling [x] = throwError $ TypeMismatch "number" x
numCeiling badArgList = throwError $ NumArgs 1 badArgList

numTruncate [n@(Number _)] = return n
numTruncate [(Rational n)] = return $ Number $ truncate n
numTruncate [(Float n)] = return $ Float $ fromInteger $ truncate n
numTruncate [(Complex n)] = do
  return $ Complex $ (fromInteger $ truncate $ realPart n) :+ (fromInteger $ truncate $ imagPart n)
numTruncate [x] = throwError $ TypeMismatch "number" x
numTruncate badArgList = throwError $ NumArgs 1 badArgList


numSin :: [LispVal] -> ThrowsError LispVal
numSin [(Number n)] = return $ Float $ sin $ fromInteger n
numSin [(Float n)] = return $ Float $ sin n
numSin [(Rational n)] = return $ Float $ sin $ fromRational n
numSin [(Complex n)] = return $ Complex $ sin n
numSin [x] = throwError $ TypeMismatch "number" x
numSin badArgList = throwError $ NumArgs 1 badArgList

numCos :: [LispVal] -> ThrowsError LispVal
numCos [(Number n)] = return $ Float $ cos $ fromInteger n
numCos [(Float n)] = return $ Float $ cos n
numCos [(Rational n)] = return $ Float $ cos $ fromRational n
numCos [(Complex n)] = return $ Complex $ cos n
numCos [x] = throwError $ TypeMismatch "number" x
numCos badArgList = throwError $ NumArgs 1 badArgList

numTan :: [LispVal] -> ThrowsError LispVal
numTan [(Number n)] = return $ Float $ tan $ fromInteger n
numTan [(Float n)] = return $ Float $ tan n
numTan [(Rational n)] = return $ Float $ tan $ fromRational n
numTan [(Complex n)] = return $ Complex $ tan n
numTan [x] = throwError $ TypeMismatch "number" x
numTan badArgList = throwError $ NumArgs 1 badArgList
       
numAsin :: [LispVal] -> ThrowsError LispVal
numAsin [(Number n)] = return $ Float $ asin $ fromInteger n
numAsin [(Float n)] = return $ Float $ asin n
numAsin [(Rational n)] = return $ Float $ asin $ fromRational n
numAsin [(Complex n)] = return $ Complex $ asin n
numAsin [x] = throwError $ TypeMismatch "number" x
numAsin badArgList = throwError $ NumArgs 1 badArgList
      
numAcos :: [LispVal] -> ThrowsError LispVal
numAcos [(Number n)] = return $ Float $ acos $ fromInteger n
numAcos [(Float n)] = return $ Float $ acos n
numAcos [(Rational n)] = return $ Float $ acos $ fromRational n
numAcos [(Complex n)] = return $ Complex $ acos n
numAcos [x] = throwError $ TypeMismatch "number" x
numAcos badArgList = throwError $ NumArgs 1 badArgList

numAtan :: [LispVal] -> ThrowsError LispVal
numAtan [(Number n)] = return $ Float $ atan $ fromInteger n
numAtan [Number y, Number x] = return $ Float $ phase $ (fromInteger x) :+ (fromInteger y)
numAtan [(Float n)] = return $ Float $ atan n
numAtan [Float y, Float x] = return $ Float $ phase $ x :+ y
numAtan [(Rational n)] = return $ Float $ atan $ fromRational n
numAtan [Rational y, Rational x] = return $ Float $ phase $ (fromRational x) :+ (fromRational y)
numAtan [(Complex n)] = return $ Complex $ atan n
numAtan [x] = throwError $ TypeMismatch "number" x
numAtan badArgList = throwError $ NumArgs 1 badArgList

numSqrt, numExpt :: [LispVal] -> ThrowsError LispVal
numSqrt [(Number n)] = if n >= 0 then return $ Float $ sqrt $ fromInteger n
                                 else return $ Complex $ sqrt ((fromInteger n) :+ 0)
numSqrt [(Float n)] = if n >= 0 then return $ Float $ sqrt n
                                else return $ Complex $ sqrt (n :+ 0)
numSqrt [(Rational n)] = numSqrt  [Float $ fromRational n]
numSqrt [(Complex n)] = return $ Complex $ sqrt n
numSqrt [x] = throwError $ TypeMismatch "number" x
numSqrt badArgList = throwError $ NumArgs 1 badArgList

numExpt [(Number n),   (Number p)] = return $ Float $ (fromInteger n) ^ p
numExpt [(Rational n), (Number p)] = return $ Float $ (fromRational n) ^ p
numExpt [(Float n),    (Number p)] = return $ Float $ n ^ p
numExpt [(Complex n),  (Number p)] = return $ Complex $ n ^ p
numExpt [_, y] = throwError $ TypeMismatch "integer" y
numExpt badArgList = throwError $ NumArgs 2 badArgList

{-numExpt params = do
  foldl1M (\a b -> doExpt =<< (numCast [a, b])) params
  where doExpt (List [(Number a), (Number b)]) = return $ Float $ (fromInteger a) ^ (fromInteger b)
--        doExpt (List [(Rational a), (Rational b)]) = return $ Float $ fromRational $ a ^ b
        doExpt (List [(Float a), (Float b)]) = return $ Float $ a ^ b
--        doExpt (List [(Complex a), (Complex b)]) = return $ Complex $ a ^ b-}

numExp :: [LispVal] -> ThrowsError LispVal
numExp [(Number n)] = return $ Float $ exp $ fromInteger n
numExp [(Float n)] = return $ Float $ exp n
numExp [(Rational n)] = return $ Float $ exp $ fromRational n
numExp [(Complex n)] = return $ Complex $ exp n
numExp [x] = throwError $ TypeMismatch "number" x
numExp badArgList = throwError $ NumArgs 1 badArgList

numLog :: [LispVal] -> ThrowsError LispVal
numLog [(Number n)] = return $ Float $ log $ fromInteger n
numLog [(Float n)] = return $ Float $ log n
numLog [(Rational n)] = return $ Float $ log $ fromRational n
numLog [(Complex n)] = return $ Complex $ log n
numLog [x] = throwError $ TypeMismatch "number" x
numLog badArgList = throwError $ NumArgs 1 badArgList


buildComplex :: LispVal -> LispVal -> ThrowsError LispVal
buildComplex (Number x) (Number y)   = return $ Complex $ (fromInteger x) :+ (fromInteger y)
buildComplex (Number x) (Rational y) = return $ Complex $(fromInteger x) :+ (fromRational y)
buildComplex (Number x) (Float y)    = return $ Complex $ (fromInteger x) :+ y
buildComplex (Rational x) (Number y)   = return $ Complex $ (fromRational x) :+ (fromInteger y)
buildComplex (Rational x) (Rational y) = return $ Complex $ (fromRational x) :+ (fromRational y)
buildComplex (Rational x) (Float y)    = return $ Complex $ (fromRational x) :+ y 
buildComplex (Float x) (Number y)   = return $ Complex $ x :+ (fromInteger y)
buildComplex (Float x) (Rational y) = return $ Complex $ x :+ (fromRational y)
buildComplex (Float x) (Float y)    = return $ Complex $ x :+ y
buildComplex x y = throwError $ TypeMismatch "number" $ List [x, y]

-- Complex number functions
numMakeRectangular, numMakePolar, numRealPart, numImagPart, numMagnitude, numAngle :: [LispVal] -> ThrowsError LispVal
numMakeRectangular [x, y] = buildComplex x y
numMakeRectangular badArgList = throwError $ NumArgs 2 badArgList

numMakePolar [(Float x), (Float y)] = return $ Complex $ mkPolar x y
numMakePolar [(Float _), y] = throwError $ TypeMismatch "real" y
numMakePolar [x, (Float _)] = throwError $ TypeMismatch "real real" $ x
numMakePolar badArgList = throwError $ NumArgs 2 badArgList

numAngle [(Complex c)] = return $ Float $ phase c
numAngle [x] = throwError $ TypeMismatch "complex number" x
numAngle badArgList = throwError $ NumArgs 1 badArgList

numMagnitude [(Complex c)] = return $ Float $ magnitude c
numMagnitude [x] = throwError $ TypeMismatch "complex number" x
numMagnitude badArgList = throwError $ NumArgs 1 badArgList

numRealPart [(Complex c)] = return $ Float $ realPart c
numRealPart [x] = throwError $ TypeMismatch "complex number" x
numRealPart badArgList = throwError $ NumArgs 1 badArgList

numImagPart [(Complex c)] = return $ Float $ imagPart c
numImagPart [x] = throwError $ TypeMismatch "complex number" x
numImagPart badArgList = throwError $ NumArgs 1 badArgList


numNumerator, numDenominator:: [LispVal] -> ThrowsError LispVal
numNumerator [(Rational r)] = return $ Number $ numerator r
numNumerator [x] = throwError $ TypeMismatch "rational number" x
numNumerator badArgList = throwError $ NumArgs 1 badArgList

numDenominator [(Rational r)] = return $ Number $ denominator r
numDenominator [x] = throwError $ TypeMismatch "rational number" x
numDenominator badArgList = throwError $ NumArgs 1 badArgList

numExact2Inexact, numInexact2Exact :: [LispVal] -> ThrowsError LispVal
numExact2Inexact [(Number n)] = return $ Float $ fromInteger n
numExact2Inexact [(Rational n)] = return $ Float $ fromRational n
numExact2Inexact [n@(Float _)] = return n
numExact2Inexact [n@(Complex _)] = return n
numExact2Inexact [badType] = throwError $ TypeMismatch "number" badType
numExact2Inexact badArgList = throwError $ NumArgs 1 badArgList

numInexact2Exact [n@(Number _)] = return n
numInexact2Exact [n@(Rational _)] = return n
numInexact2Exact [(Float n)] = return $ Number $ round n
numInexact2Exact [c@(Complex _)] = numRound [c] 
numInexact2Exact [badType] = throwError $ TypeMismatch "number" badType
numInexact2Exact badArgList = throwError $ NumArgs 1 badArgList

-- Convert a number to a string; radix is optional, defaults to base 10
num2String :: [LispVal] -> ThrowsError LispVal
num2String [(Number n)] = return $ String $ show n
num2String [(Number n), (Number radix)] = do
  case radix of
    2  -> do -- Nice tip from StackOverflow question #1959715
             return $ String $ showIntAtBase 2 intToDigit n ""
    8  -> return $ String $ printf "%o" n
    10 -> return $ String $ printf "%d" n
    16 -> return $ String $ printf "%x" n
    _  -> throwError $ BadSpecialForm "Invalid radix value" $ Number radix
num2String [n@(Rational _)] = return $ String $ show n
num2String [(Float n)] = return $ String $ show n
num2String [n@(Complex _)] = return $ String $ show n
num2String [x] = throwError $ TypeMismatch "number" x
num2String badArgList = throwError $ NumArgs 1 badArgList

isNumber, isComplex, isReal, isRational, isInteger :: [LispVal] -> ThrowsError LispVal
isNumber ([Number _]) = return $ Bool True
isNumber ([Float _]) = return $ Bool True
isNumber ([Complex _]) = return $ Bool True
isNumber ([Rational _]) = return $ Bool True
isNumber _ = return $ Bool False

isComplex ([Complex _]) = return $ Bool True
isComplex ([Number _]) = return $ Bool True
isComplex ([Rational _]) = return $ Bool True
isComplex ([Float _]) = return $ Bool True
isComplex _ = return $ Bool False

isReal ([Number _]) = return $ Bool True
isReal ([Rational _]) = return $ Bool True
isReal ([Float _]) = return $ Bool True
isReal ([Complex c]) = return $ Bool $ (imagPart c) == 0
isReal _ = return $ Bool False

isRational ([Number _]) = return $ Bool True
isRational ([Rational _]) = return $ Bool True
isRational ([n@(Float _)]) = return $ Bool $ isFloatAnInteger n 
                             -- FUTURE: not quite good enough, could be represented exactly and not an integer
isRational _ = return $ Bool False

isInteger ([Number _]) = return $ Bool True
isInteger ([Complex n]) = do
  return $ Bool $ (isFloatAnInteger $ Float $ realPart n) && (isFloatAnInteger $ Float $ imagPart n)
isInteger ([Rational n]) = do
    let numer = numerator n
    let denom = denominator n
    return $ Bool $ (numer >= denom) && ((mod numer denom) == 0)
isInteger ([n@(Float _)]) = return $ Bool $ isFloatAnInteger n 
isInteger _ = return $ Bool False

isFloatAnInteger :: LispVal -> Bool
isFloatAnInteger (Float n) = (floor n) == (ceiling n)
isFloatAnInteger _ = False 

--- end Numeric operations section ---


unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum notNum = throwError $ TypeMismatch "number" notNum
