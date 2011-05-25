module Language.Scheme.Plugins.CPUTime (get, precision) where

import Language.Scheme.Types
import System.CPUTime
import Control.Monad.Error

-- TODO: this is not really elapsed time, rather the amount of CPU time consumed...
get, precision :: [LispVal] -> IOThrowsError LispVal

{- |Wrapper for CPUTime.getCPUTime -}
get [] = do
  t <- liftIO $ System.CPUTime.getCPUTime
  return $ Number t
get _ = throwError $ Default "unknown args passed to test method"

{- |Wrapper for CPUTime.cpuTimePrecision -}
precision [] = return $ Number $ System.CPUTime.cpuTimePrecision 
precision _ = throwError $ Default "unknown args passed to test method"
