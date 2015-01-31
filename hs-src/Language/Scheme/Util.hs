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
    ( countAllLetters
    , countLetters
    , escapeBackslashes
    , lastN'
    , strip
    ) where

import qualified Data.List as DL

-- |A utility function to escape backslashes in the given string
escapeBackslashes :: String -> String
escapeBackslashes = foldr step []
  where step x xs  | x == '\\'  = '\\' : '\\' : xs
                   | otherwise =  x : xs 

-- | Remove leading/trailing white space from a string; based on corresponding 
--   Python function. Code taken from: 
--
--   <http://gimbo.org.uk/blog/2007/04/20/splitting-a-string-in-haskell/>
strip :: String -> String
strip s = dropWhile ws $ reverse $ dropWhile ws $ reverse s
    where ws = (`elem` [' ', '\n', '\t', '\r'])

-- |Count occurences of a letter in a list of strings
countAllLetters :: Char -> [String] -> Int
countAllLetters c strs = sum $ map (countLetters c) strs

-- |Count occurences of a letter in a string
countLetters :: Char -> String -> Int
countLetters c str = length $ filter (== c) str

-- | Take last n elements of a list, from:
--   <http://stackoverflow.com/q/17252851/101258>
lastN' :: Int -> [a] -> [a]
lastN' n xs = DL.foldl' (const .drop 1) xs (drop n xs)

