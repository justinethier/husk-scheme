{-
 - husk scheme interpreter
 -
 - A lightweight dialect of R5RS scheme.
 - Numerical tower functionality
 -
 - @author Justin Ethier
 -
 - -}

module Scheme.Numerical where
import Scheme.Types
import Scheme.Variables
import Complex
import Control.Monad.Error
import Numeric
import Ratio
import Text.Printf

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params = mapM unpackNum params >>= return . Number . foldl1 op

--- Begin GenUtil - http://repetae.net/computer/haskell/GenUtil.hs
foldlM :: Monad m => (a -> b -> m a) -> a -> [b] -> m a
foldlM f v (x:xs) = (f v x) >>= \a -> foldlM f a xs
foldlM _ v [] = return v

foldl1M :: Monad m => (a -> a -> m a) ->  [a] -> m a
foldl1M f (x:xs) = foldlM f x xs
foldl1M _ _ = error "foldl1M"
-- end GenUtil


--- Numeric operations section ---
-- TODO: move all of this out into its own file

numAdd, numSub, numMul, numDiv :: [LispVal] -> ThrowsError LispVal
numAdd params = do
  foldl1M (\a b -> doAdd =<< (numCast [a, b])) params
  where doAdd (List [(Number a), (Number b)]) = return $ Number $ a + b
        doAdd (List [(Float a), (Float b)]) = return $ Float $ a + b
        doAdd (List [(Rational a), (Rational b)]) = return $ Rational $ a + b
        doAdd (List [(Complex a), (Complex b)]) = return $ Complex $ a + b
numSub [Number n] = return $ Number $ -1 * n
numSub [Float n] = return $ Float $ -1 * n
numSub [Rational n] = return $ Rational $ -1 * n
numSub [Complex n] = return $ Complex $ -1 * n
numSub params = do
  foldl1M (\a b -> doSub =<< (numCast [a, b])) params
  where doSub (List [(Number a), (Number b)]) = return $ Number $ a - b
        doSub (List [(Float a), (Float b)]) = return $ Float $ a - b
        doSub (List [(Rational a), (Rational b)]) = return $ Rational $ a - b
        doSub (List [(Complex a), (Complex b)]) = return $ Complex $ a - b
numMul params = do 
  foldl1M (\a b -> doMul =<< (numCast [a, b])) params
  where doMul (List [(Number a), (Number b)]) = return $ Number $ a * b
        doMul (List [(Float a), (Float b)]) = return $ Float $ a * b
        doMul (List [(Rational a), (Rational b)]) = return $ Rational $ a * b
        doMul (List [(Complex a), (Complex b)]) = return $ Complex $ a * b
numDiv [Number n] = return $ Rational $ 1 / (fromInteger n)
numDiv [Float n] = return $ Float $ 1.0 / n
numDiv [Rational n] = return $ Rational $ 1 / n
numDiv [Complex n] = return $ Complex $ 1 / n
numDiv params = do -- TODO: for Number type, need to cast results to Rational, per R5RS spec 
  foldl1M (\a b -> doDiv =<< (numCast [a, b])) params
  where doDiv (List [(Number a), (Number b)]) = if b == 0 
                                                   then throwError $ DivideByZero 
                                                   else return $ Number $ div a b
        doDiv (List [(Float a), (Float b)]) = if b == 0.0 
                                                   then throwError $ DivideByZero 
                                                   else return $ Float $ a / b
        doDiv (List [(Rational a), (Rational b)]) = if b == 0
                                                       then throwError $ DivideByZero 
                                                       else return $ Rational $ a / b
        doDiv (List [(Complex a), (Complex b)]) = if b == 0
                                                       then throwError $ DivideByZero 
                                                       else return $ Complex $ a / b

numBoolBinopEq :: [LispVal] -> ThrowsError LispVal
numBoolBinopEq params = do 
  foldl1M (\a b -> doOp =<< (numCast [a, b])) params
  where doOp (List [(Number a), (Number b)]) = return $ Bool $ a == b
        doOp (List [(Float a), (Float b)]) = return $ Bool $ a == b
        doOp (List [(Rational a), (Rational b)]) = return $ Bool $ a == b
        doOp (List [(Complex a), (Complex b)]) = return $ Bool $ a == b

numBoolBinopGt :: [LispVal] -> ThrowsError LispVal
numBoolBinopGt params = do 
  foldl1M (\a b -> doOp =<< (numCast [a, b])) params
  where doOp (List [(Number a), (Number b)]) = return $ Bool $ a > b
        doOp (List [(Float a), (Float b)]) = return $ Bool $ a > b
        doOp (List [(Rational a), (Rational b)]) = return $ Bool $ a > b

numBoolBinopGte :: [LispVal] -> ThrowsError LispVal
numBoolBinopGte params = do 
  foldl1M (\a b -> doOp =<< (numCast [a, b])) params
  where doOp (List [(Number a), (Number b)]) = return $ Bool $ a >= b
        doOp (List [(Float a), (Float b)]) = return $ Bool $ a >= b
        doOp (List [(Rational a), (Rational b)]) = return $ Bool $ a >= b

numBoolBinopLt :: [LispVal] -> ThrowsError LispVal
numBoolBinopLt params = do 
  foldl1M (\a b -> doOp =<< (numCast [a, b])) params
  where doOp (List [(Number a), (Number b)]) = return $ Bool $ a < b
        doOp (List [(Float a), (Float b)]) = return $ Bool $ a < b
        doOp (List [(Rational a), (Rational b)]) = return $ Bool $ a < b

numBoolBinopLte :: [LispVal] -> ThrowsError LispVal
numBoolBinopLte params = do 
  foldl1M (\a b -> doOp =<< (numCast [a, b])) params
  where doOp (List [(Number a), (Number b)]) = return $ Bool $ a <= b
        doOp (List [(Float a), (Float b)]) = return $ Bool $ a <= b
        doOp (List [(Rational a), (Rational b)]) = return $ Bool $ a <= b

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
               otherwise  -> doThrowError a
  where doThrowError a = throwError $ TypeMismatch "number" a


numRound, numFloor, numCeiling, numTruncate :: [LispVal] -> ThrowsError LispVal
numRound [n@(Number _)] = return n
numRound [(Rational n)] = return $ Number $ round n
numRound [(Float n)] = return $ Float $ fromInteger $ round n
-- TODO: complex (?)
numRound [x] = throwError $ TypeMismatch "number" x
numRound badArgList = throwError $ NumArgs 1 badArgList

-- TODO:
numFloor [n@(Number _)] = return n
numFloor [(Rational n)] = return $ Number $ floor n
numFloor [(Float n)] = return $ Float $ fromInteger $ floor n
-- TODO: complex (?)
numFloor [x] = throwError $ TypeMismatch "number" x
numFloor badArgList = throwError $ NumArgs 1 badArgList

numCeiling [n@(Number _)] = return n
numCeiling [(Rational n)] = return $ Number $ ceiling n
numCeiling [(Float n)] = return $ Float $ fromInteger $ ceiling n
-- TODO: complex (?)
numCeiling [x] = throwError $ TypeMismatch "number" x
numCeiling badArgList = throwError $ NumArgs 1 badArgList

numTruncate [n@(Number _)] = return n
numTruncate [(Rational n)] = return $ Number $ truncate n
numTruncate [(Float n)] = return $ Float $ fromInteger $ truncate n
-- TODO: complex (?)
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

-- TODO: support for (atan y x) - see spec
numAtan :: [LispVal] -> ThrowsError LispVal
numAtan [(Number n)] = return $ Float $ atan $ fromInteger n
numAtan [(Float n)] = return $ Float $ atan n
numAtan [(Rational n)] = return $ Float $ atan $ fromRational n
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
numExpt [x, y] = throwError $ TypeMismatch "integer" y
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


-- Complex number functions
numMakeRectangular, numMakePolar, numRealPart, numImagPart, numMagnitude, numAngle :: [LispVal] -> ThrowsError LispVal
numMakeRectangular [(Float x), (Float y)] = return $ Complex $ x :+ y 
-- TODO: other members of the numeric tower (?)
numMakeRectangular [x, y] = throwError $ TypeMismatch "real real" $ List [x, y]
numMakeRectangular badArgList = throwError $ NumArgs 2 badArgList

numMakePolar [(Float x), (Float y)] = return $ Complex $ mkPolar x y
-- TODO: other members of the numeric tower (?)
numMakePolar [x, y] = throwError $ TypeMismatch "real real" $ List [x, y]
numMakePolar badArgList = throwError $ NumArgs 2 badArgList

numAngle [(Complex c)] = return $ Float $ phase c -- TODO: correct?? need to check this
numAngle [x] = throwError $ TypeMismatch "number" x
numAngle badArgList = throwError $ NumArgs 1 badArgList

numMagnitude [(Complex c)] = return $ Float $ magnitude c
numMagnitude [x] = throwError $ TypeMismatch "number" x
numMagnitude badArgList = throwError $ NumArgs 1 badArgList

numRealPart [(Complex c)] = return $ Float $ realPart c
numRealPart [x] = throwError $ TypeMismatch "number" x
numRealPart badArgList = throwError $ NumArgs 1 badArgList

numImagPart [(Complex c)] = return $ Float $ imagPart c
numImagPart [x] = throwError $ TypeMismatch "number" x
numImagPart badArgList = throwError $ NumArgs 1 badArgList


numNumerator, numDenominator:: [LispVal] -> ThrowsError LispVal
numNumerator [(Rational r)] = return $ Number $ numerator r
-- TODO: real?
numNumerator [x] = throwError $ TypeMismatch "rational number" x
numNumerator badArgList = throwError $ NumArgs 1 badArgList

numDenominator [(Rational r)] = return $ Number $ denominator r
-- TODO: real?
numDenominator [x] = throwError $ TypeMismatch "rational number" x
numDenominator badArgList = throwError $ NumArgs 1 badArgList

numExact2Inexact, numInexact2Exact :: [LispVal] -> ThrowsError LispVal
numExact2Inexact [(Number n)] = return $ Float $ fromInteger n
numExact2Inexact [(Rational n)] = return $ Float $ fromRational n
numExact2Inexact [n@(Float _)] = return n
-- TODO: numExact2Inexact [(Complex n)] = return ??

numInexact2Exact [n@(Number _)] = return n
numInexact2Exact [n@(Rational _)] = return n
numInexact2Exact [(Float n)] = return $ Number $ round n
-- TODO: numInexact2Exact [(Complex n)] = return ??

-- TODO: remember to support both forms:
-- procedure:  (number->string z) 
-- procedure:  (number->string z radix) 
num2String :: [LispVal] -> ThrowsError LispVal
num2String [(Number n)] = return $ String $ show n
num2String [(Number n), (Number radix)] = do
  case radix of
-- TODO: boolean    2 -> return $ String $ printf "%x" n
    8 -> return $ String $ printf "%o" n
    10 -> return $ String $ printf "%d" n
    16 -> return $ String $ printf "%x" n
    otherwise -> throwError $ BadSpecialForm "Invalid radix value" $ Number radix
num2String [n@(Rational _)] = return $ String $ show n
num2String [(Float n)] = return $ String $ show n
num2String [n@(Complex _)] = return $ String $ show n
num2String [x] = throwError $ TypeMismatch "number" x
num2String badArgList = throwError $ NumArgs 1 badArgList

-- TODO: relocated string->number logic here (???),
--       and extend to support all of the tower...

isNumber, isComplex, isReal, isRational, isInteger :: [LispVal] -> ThrowsError LispVal
isNumber ([Number n]) = return $ Bool True
isNumber ([Float f]) = return $ Bool True
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
-- TODO: true of float if it can be represented exactly???
isRational _ = return $ Bool False

isInteger ([Number _]) = return $ Bool True
-- TODO: true of real/rational types if they round to an integer
isInteger _ = return $ Bool False

--- end Numeric operations section ---


unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum notNum = throwError $ TypeMismatch "number" notNum
