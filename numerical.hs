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
  foldl1M (\a b -> doAdd =<< (numCast [a, b])) params
  where doAdd (List [(Number a), (Number b)]) = return $ Number $ a - b
        doAdd (List [(Float a), (Float b)]) = return $ Float $ a - b
        doAdd (List [(Rational a), (Rational b)]) = return $ Rational $ a - b
        doAdd (List [(Complex a), (Complex b)]) = return $ Complex $ a - b
numMul params = do 
  foldl1M (\a b -> doAdd =<< (numCast [a, b])) params
  where doAdd (List [(Number a), (Number b)]) = return $ Number $ a * b
        doAdd (List [(Float a), (Float b)]) = return $ Float $ a * b
        doAdd (List [(Rational a), (Rational b)]) = return $ Rational $ a * b
        doAdd (List [(Complex a), (Complex b)]) = return $ Complex $ a * b
numDiv [Number n] = return $ Rational $ 1 / (fromInteger n)
numDiv [Float n] = return $ Float $ 1.0 / n
numDiv [Rational n] = return $ Rational $ 1 / n
numDiv [Complex n] = return $ Complex $ 1 / n
numDiv params = do -- TODO: for Number type, need to cast results to Rational, per R5RS spec 
  foldl1M (\a b -> doAdd =<< (numCast [a, b])) params
  where doAdd (List [(Number a), (Number b)]) = return $ Number $ div a b
        doAdd (List [(Float a), (Float b)]) = return $ Float $ a / b
        doAdd (List [(Rational a), (Rational b)]) = return $ Rational $ a / b
        doAdd (List [(Complex a), (Complex b)]) = return $ Complex $ a / b

numCast :: [LispVal] -> ThrowsError LispVal
numCast [a@(Number _), b@(Number _)] = return $ List [a, b]
numCast [a@(Float _), b@(Float _)] = return $ List [a, b]
numCast [a@(Rational _), b@(Rational _)] = return $ List [a, b]
numCast [a@(Complex _), b@(Complex _)] = return $ List [a, b]
numCast [(Number a), b@(Float _)] = return $ List [Float $ fromInteger a, b]
numCast [(Number a), b@(Rational _)] = return $ List [Rational $ fromInteger a, b]
numCast [(Number a), b@(Complex _)] = return $ List [Complex $ fromInteger a, b]
numCast [a@(Float _), (Number b)] = return $ List [a, Float $ fromInteger b]
numCast [(Float a), b@(Rational _)] = return $ List [Rational $ toRational a, b]
numCast [(Float a), b@(Complex _)] = return $ List [Complex $ a :+ 0, b]
numCast [a@(Rational _), (Number b)] = return $ List [a, Rational $ fromInteger b]
numCast [a@(Rational _), (Float b)] = return $ List [a, Rational $ toRational b]
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


numMakeRectangular, numMakePolar, numRealPart, numImagPart, numMagnitude, numAngle :: [LispVal] -> ThrowsError LispVal
-- TODO: numMakeRectangular, numMakePolar, numRealPart, numImagPart, numMagnitude, numAngle 

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
