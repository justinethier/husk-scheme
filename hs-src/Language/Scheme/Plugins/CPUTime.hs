module Language.Scheme.Plugins.CPUTime (elapsed, precision) where

import Language.Scheme.Types
import System.CPUTime
import Control.Monad.Error

-- TODO: this is not really elapsed time, rather the amount of CPU time consumed...
elapsed, precision :: [LispVal] -> IOThrowsError LispVal

elapsed [] = do
  t <- liftIO $ System.CPUTime.getCPUTime
  return $ Number t
elapsed _ = throwError $ Default "unknown args passed to test method"

precision [] = return $ Number $ System.CPUTime.cpuTimePrecision 
precision _ = throwError $ Default "unknown args passed to test method"
