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
findModuleFile [String file] 
    -- Built-in modules
-- TODO: does this work in Windows, since it uses the "wrong" type of slashes for that OS?
    | file == "scheme/base.sld" ||
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
-- TODO: this does not work for macros!
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
