{- |
Module      : Language.Scheme.Modules
Copyright   : Justin Ethier
Licence     : MIT (see LICENSE in the distribution)

Maintainer  : github.com/justinethier
Stability   : experimental
Portability : portable

This module contains code to handle modules (AKA R7RS libraries).
-}

module Language.Scheme.Modules
    (
      moduleImport
    ) where
import qualified Paths_husk_scheme as PHS (getDataFileName)
import Language.Scheme.Types
import Language.Scheme.Util
import Language.Scheme.Variables

findModuleFile 
    :: LispVal 
    -> IOThrowsError LispVal
findModuleFile (String s@("scheme/base.sld")) = return $ String s
findModuleFile (String file) = return $ String file
findModuleFile _ = return $ Bool False

-- |Import definitions from one environment into another
moduleImport 
    :: Env  -- ^ Environment to import into
    -> Env  -- ^ Environment to import from
    -> [LispVal] -- ^ Identifiers to import
    -> IOThrowsError LispVal
moduleImport to from (Atom i : is) = do
  v <- getVar from i 
  _ <- defineVar to i v
  moduleImport to from is
moduleImport to from (DottedList [Atom iRenamed] (Atom iOrig) : is) = do
  v <- getVar from iOrig 
  _ <- defineVar to iRenamed v
  moduleImport to from is
moduleImport to from [] = do
  return $ LispEnv to
-- DEBUG:
-- moduleImport to from unknown = do
--   (trace ("MODULE IMPORT DEBUG: " ++ show unknown) return) $ Nil ""
