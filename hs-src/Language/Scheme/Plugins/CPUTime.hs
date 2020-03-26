{- |
Module      : Language.Scheme.Plugins.CPUTime
Copyright   : Justin Ethier
Licence     : MIT (see LICENSE in the distribution)

Maintainer  : github.com/justinethier
Stability   : experimental
Portability : portable

This module wraps System.CPUTime so that it can be used directly by Scheme code.

More importantly, it serves as an example of how to wrap existing Haskell code so
that it can be loaded and called by husk.

See 'examples/ffi/ffi-cputime.scm' in the husk source tree for an example of how to
call into this module from Scheme code.
-}

module Language.Scheme.Plugins.CPUTime (get, precision) where

import Language.Scheme.Types
import System.CPUTime
import Control.Monad.Except

-- |Wrapper for CPUTime.getCPUTime
get :: [LispVal] -> IOThrowsError LispVal
get [] = do
  t <- liftIO $ System.CPUTime.getCPUTime
  return $ Number t
get badArgList = throwError $ NumArgs (Just 0) badArgList

-- |Wrapper for CPUTime.cpuTimePrecision
precision :: [LispVal] -> IOThrowsError LispVal
precision [] = return $ Number $ System.CPUTime.cpuTimePrecision
precision badArgList = throwError $ NumArgs (Just 0) badArgList
