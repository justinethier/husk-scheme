module Language.Scheme.Plugins.CPUTime (precision) where

import Language.Scheme.Types
import System.CPUTime
import Control.Monad.Error

precision :: [LispVal] -> ThrowsError LispVal
precision [] = return $ Number $ System.CPUTime.cpuTimePrecision 
precision _ = throwError $ Default "unknown args passed to test method"
