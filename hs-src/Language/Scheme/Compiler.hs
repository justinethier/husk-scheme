{- |
Module      : Language.Scheme.Core
Copyright   : Justin Ethier
Licence     : MIT (see LICENSE in the distribution)

Maintainer  : github.com/justinethier
Stability   : experimental
Portability : portable

This module contains an experimental compiler of Scheme to Haskell 
-}

module Language.Scheme.Compiler where 
{-    (
      evalLisp
    , evalString
    , evalAndPrint
    , primitiveBindings
    ) where -}
{-
import qualified Language.Scheme.FFI
import qualified Language.Scheme.Macro
-}
import Language.Scheme.Numerical
import Language.Scheme.Parser
import Language.Scheme.Primitives
import Language.Scheme.Types
import Language.Scheme.Variables
import Control.Monad.Error
{-
import Data.Array
import qualified Data.Map
-}
import System.IO

compileLisp :: String -> IO ()
compileLisp file = do
  putStrLn $ "TODO: load file " ++ file
