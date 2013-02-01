{- |
Module      : Language.Scheme.Util
Copyright   : Justin Ethier
Licence     : MIT (see LICENSE in the distribution)

Maintainer  : github.com/justinethier
Stability   : experimental
Portability : portable

This module contains general-purpose utility functions
-}

module Language.Scheme.Util
    (
      escapeBackslashes
    ) where
-- import qualified Paths_husk_scheme as PHS (getDataFileName)
-- import Language.Scheme.Types
-- import Language.Scheme.Variables

-- |A utility function to escape backslashes in the given string
escapeBackslashes :: String -> String
escapeBackslashes s = foldr step [] s
  where step x xs  | x == '\\'  = '\\' : '\\' : xs
                   | otherwise =  x : xs 

