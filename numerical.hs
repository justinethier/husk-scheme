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
import Control.Monad
import Control.Monad.Error
--import Char
--import Data.Array
--import Data.IORef
--import qualified Data.Map
import Maybe
import List
--import IO hiding (try)
import Numeric
import Ratio
--import System.Environment

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
numDiv params = do 
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

-- TODO: for sin, etc - can have a func that converts args to proper input type (such as real)

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
