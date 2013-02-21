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
import qualified Paths_husk_scheme as PHS (getDataFileName)
import Language.Scheme.Types
import Language.Scheme.Util
import Language.Scheme.Variables
import Control.Monad.Error

-- |Get the full path to a module file
findModuleFile 
    :: [LispVal]
    -> IOThrowsError LispVal
findModuleFile [p@(Pointer _ _)] = recDerefPtrs p >>= box >>= findModuleFile
findModuleFile [String file] 
    -- Built-in modules
-- TODO: does this work in Windows, since it uses the "wrong" type of slashes for that OS?
    | file == "r5rs/base.sld" ||
      file == "scheme/base.sld" ||
      file == "scheme/write.sld" = do
        path <- liftIO $ PHS.getDataFileName $ "lib/" ++ file
        return $ String path
    | otherwise = return $ String file
findModuleFile _ = return $ Bool False

-- |Import definitions from one environment into another
moduleImport 
    :: Env  -- ^ Environment to import into
    -> Env  -- ^ Environment to import from
    -> [LispVal] -- ^ Identifiers to import
    -> IOThrowsError LispVal
moduleImport to from (Atom i : is) = do
  _ <- divertBinding to from i i
  moduleImport to from is
moduleImport to from (DottedList [Atom iRenamed] (Atom iOrig) : is) = do
  _ <- divertBinding to from iOrig iRenamed
  moduleImport to from is
moduleImport to from [] = do
  return $ LispEnv to
-- DEBUG:
-- moduleImport to from unknown = do
--   (trace ("MODULE IMPORT DEBUG: " ++ show unknown) return) $ Nil ""

-- |Copy a binding from one env to another
divertBinding
    :: Env  -- ^ Environment to import into
    -> Env  -- ^ Environment to import from
    -> String -- ^ Name of the binding in 'from'
    -> String -- ^ Name to use for the binding in 'to'
    -> IOThrowsError LispVal
divertBinding to from nameOrig nameNew = do
  isMacroBound <- liftIO $ isNamespacedRecBound from macroNamespace nameOrig
  namespace <- liftIO $ case isMacroBound of
                          True -> return macroNamespace
                          _ -> return varNamespace
  m <- getNamespacedVar from namespace nameOrig
  defineNamespacedVar to namespace nameNew m

