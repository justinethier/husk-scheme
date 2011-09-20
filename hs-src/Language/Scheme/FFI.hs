{- |
Module      : Language.Scheme.FFI
Copyright   : Justin Ethier
Licence     : MIT (see LICENSE in the distribution)

Maintainer  : github.com/justinethier
Stability   : experimental
Portability : non-portable (GHC API)

husk scheme interpreter

A lightweight dialect of R5RS scheme.

This module contains the foreign function interface.
-}
module Language.Scheme.FFI (evalfuncLoadFFI) where

import Language.Scheme.Types
import Language.Scheme.Variables
import Control.Monad.Error

import qualified GHC
import qualified GHC.Paths (libdir)
import qualified DynFlags
import qualified Unsafe.Coerce (unsafeCoerce)

evalfuncLoadFFI :: [LispVal] -> IOThrowsError LispVal
{-
 - |Load a Haskell function into husk using the foreign function inteface (FFI)
 -
 - Based on example code from:
 -
 - http://stackoverflow.com/questions/5521129/importing-a-known-function-from-an-already-compiled-binary-using-ghcs-api-or-hi
 - and
 - http://www.bluishcoder.co.nz/2008/11/dynamic-compilation-and-loading-of.html
 -
 -
 - TODO: pass a list of functions to import. Need to make sure this is done in an efficient way
 - (IE, result as a list that can be processed) 
 -}
evalfuncLoadFFI [(Continuation env _ _ _ _), String targetSrcFile,
                                                  String moduleName,
                                                  String externalFuncName,
                                                  String internalFuncName] = do
  result <- liftIO $ defaultRunGhc $ do
    dynflags <- GHC.getSessionDynFlags
    _ <- GHC.setSessionDynFlags dynflags
    -- let m = GHC.mkModule (GHC.thisPackage dynflags) (GHC.mkModuleName "Test")

--
{- TODO: migrate duplicate code into helper functions to drive everything
FUTURE: should be able to load multiple functions in one shot (?). -}
--
    target <- GHC.guessTarget targetSrcFile Nothing
    GHC.addTarget target
    r <- GHC.load GHC.LoadAllTargets
    case r of
       GHC.Failed -> error "Compilation failed"
       GHC.Succeeded -> do
           m <- GHC.findModule (GHC.mkModuleName moduleName) Nothing
#if __GLASGOW_HASKELL__ < 700
           GHC.setContext [] [m]
#else
           GHC.setContext [] [(m, Nothing)]
#endif
           fetched <- GHC.compileExpr (moduleName ++ "." ++ externalFuncName)
           return (Unsafe.Coerce.unsafeCoerce fetched :: [LispVal] -> IOThrowsError LispVal)
  defineVar env internalFuncName (IOFunc result) -- >>= continueEval env cont

-- Overload that loads code from a compiled module
evalfuncLoadFFI [(Continuation env _ _ _ _), String moduleName, String externalFuncName, String internalFuncName] = do
  result <- liftIO $ defaultRunGhc $ do
    dynflags <- GHC.getSessionDynFlags
    _ <- GHC.setSessionDynFlags dynflags
    m <- GHC.findModule (GHC.mkModuleName moduleName) Nothing
#if __GLASGOW_HASKELL__ < 700
    GHC.setContext [] [m]
#else
    GHC.setContext [] [(m, Nothing)]
#endif
    fetched <- GHC.compileExpr (moduleName ++ "." ++ externalFuncName)
    return (Unsafe.Coerce.unsafeCoerce fetched :: [LispVal] -> IOThrowsError LispVal)
  defineVar env internalFuncName (IOFunc result) -- >>= continueEval env cont

evalfuncLoadFFI _ = throwError $ NumArgs 3 []

defaultRunGhc :: GHC.Ghc a -> IO a
defaultRunGhc = GHC.defaultErrorHandler DynFlags.defaultDynFlags . GHC.runGhc (Just GHC.Paths.libdir)
