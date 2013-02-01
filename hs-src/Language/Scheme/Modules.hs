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
findModuleFile [String file] 
    -- Built-in modules
    | file == "scheme/base.sld" ||
      file == "stdlib.scm" || -- TESTING!
      file == "scheme/write.sld" = do
        path <- liftIO $ PHS.getDataFileName file
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
