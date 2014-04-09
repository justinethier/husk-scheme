{- |
Module      : Language.Scheme.Libraries
Copyright   : Justin Ethier
Licence     : MIT (see LICENSE in the distribution)

Maintainer  : github.com/justinethier
Stability   : experimental
Portability : portable

This module contains code to handle R7RS libraries.
NOTE: Libraries are usually referred to as "modules" in the husk source code.

-}

module Language.Scheme.Libraries
    (
      findModuleFile
    , moduleImport
    ) where
import Language.Scheme.Types
import Language.Scheme.Variables
import Control.Monad.Error

-- |Get the full path to a module file
findModuleFile 
    :: [LispVal]
    -> IOThrowsError LispVal
findModuleFile [p@(Pointer _ _)] = recDerefPtrs p >>= box >>= findModuleFile
findModuleFile [String file] = do
    --- Good enough now that load searches 'lib' if file not found
    return $ String file
findModuleFile _ = return $ Bool False

-- |Import definitions from one environment into another
moduleImport 
    :: Env  -- ^ Environment to import into
    -> Env  -- ^ Environment to import from
    -> [LispVal] -- ^ Identifiers to import
    -> IOThrowsError LispVal
moduleImport to from (p@(Pointer _ _) : is) = do
  i <- derefPtr p
  moduleImport to from (i : is)
moduleImport to from (Atom i : is) = do
  _ <- divertBinding to from i i
  moduleImport to from is
moduleImport to from (DottedList [Atom iRenamed] (Atom iOrig) : is) = do
  _ <- divertBinding to from iOrig iRenamed
  moduleImport to from is
moduleImport to _ [] = do
  return $ LispEnv to
moduleImport _ _ err = do
  throwError $ Default $ "Unexpected argument to moduleImport: " ++ show err

-- |Copy a binding from one env to another
divertBinding
    :: Env  -- ^ Environment to import into
    -> Env  -- ^ Environment to import from
    -> String -- ^ Name of the binding in 'from'
    -> String -- ^ Name to use for the binding in 'to'
    -> IOThrowsError LispVal
divertBinding to from nameOrig nameNew = do
  isMacroBound <- liftIO $ isNamespacedRecBound from macroNamespace nameOrig
  namespace <- liftIO $ if isMacroBound then return macroNamespace
                                        else return varNamespace
  m <- getNamespacedVar from namespace nameOrig
  defineNamespacedVar to namespace nameNew m

