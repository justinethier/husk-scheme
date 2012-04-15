{- |
Module      : Main
Licence     : MIT (see LICENSE in the distribution)

Maintainer  : github.com/justinethier
Stability   : experimental
Portability : portable

This file provides an example of how to use Opaque types to
store arbitrary haskell data within a LispVal.

Written by Josh Triplett (https://github.com/joshtriplett).

To build, run: ghc Opaque.hs

-}

module Main where

import Control.Monad
import Control.Monad.Error
import qualified Data.Set as S
import Language.Scheme.Core
import Language.Scheme.Primitives
import Language.Scheme.Types
import Language.Scheme.Variables

unpackSet :: LispVal -> ThrowsError (S.Set String)
unpackSet = fromOpaque

primSingleton :: [LispVal] -> ThrowsError LispVal
primSingleton [e] = liftM (toOpaque . S.singleton) (unpackStr e)
primSingleton badArgs = throwError $ NumArgs 1 badArgs

primElem :: [LispVal] -> ThrowsError LispVal
primElem [e, s] = liftM Bool $ liftM2 (S.member) (unpackStr e) (unpackSet s)
primElem badArgs = throwError $ NumArgs 2 badArgs

primUnion :: [LispVal] -> ThrowsError LispVal
primUnion = liftM (toOpaque . S.unions) . mapM unpackSet

setBindings :: [(String, LispVal)]
setBindings = [
        ("nullSet", toOpaque (S.empty :: S.Set String)),
        ("singleton", PrimitiveFunc primSingleton),
        ("elem", PrimitiveFunc primElem),
        ("union", PrimitiveFunc primUnion)
    ]

main :: IO ()
main = do
    env <- nullEnv >>= flip extendEnv [((varNamespace, name), value) | (name, value) <- setBindings]
    -- Correct usage
    evalAndPrint env "(elem \"hello\" (union (singleton \"hello\") nullSet (singleton \"world\")))"
    -- Scheme type error
    evalAndPrint env "(elem \"hello\" 42)"
