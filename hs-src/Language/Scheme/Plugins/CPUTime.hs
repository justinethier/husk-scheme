module Language.Scheme.Plugins.CPUTime (cpuTimePrecision) where

import Language.Scheme.Types
import System.CPUTime
import Control.Monad.Error

test :: [LispVal] -> ThrowsError LispVal
test [] = return $ Number $ System.CPUTime.cpuTimePrecision 
test _ = throwError $ Default "unknown args passed to test method"
