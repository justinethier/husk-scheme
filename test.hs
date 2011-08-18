{- |
Module      : Language.Scheme.Utilities
Copyright   : Justin Ethier
Licence     : MIT (see LICENSE in the distribution)

Maintainer  : github.com/justinethier
Stability   : experimental
Portability : portable

husk scheme interpreter

A lightweight dialect of R5RS scheme.

This module contains utility functions, primarily used to support macro processing.
-}
module Language.Scheme.Utilities (getData, setData) where
import Language.Scheme.Types
import Control.Exception
--import Debug.Trace

-- |Create a nested list
create :: Int     -- ^ Number of nesting levels
       -> LispVal -- ^ Empty nested list
create level 
    | level < 1     = Nil "" -- Error
    | level == 1    = List []
    | otherwise = List [create $ level - 1]

-- |Fill any empty "holes" in a list from the beginning to the given length
--
-- The problem here is how to handle case when a requested insertion leaves "holes".
--
-- For example, in a 2-level nested list: ((1)) we have data as pos 0 but have none at pos 1.
-- If the code then tries to add an element 2 at pos 2 we should end up with:
--
--    ((1) () (2))
--
fill :: [LispVal] -> Int -> [LispVal]
fill l len 
  | length l < len  = fill (l ++ [List []]) len
  | otherwise       = l

-- |Get an element at given location in the nested list
getData :: LispVal -- ^ The nested list to read from
        -> [Int]   -- ^ Location to read an element from, all numbers are 0-based
        -> LispVal -- ^ Value read, or "Nil" if none
getData _ _ = Nil "" -- TODO: implement

-- |Add an element to the given nested list
setData :: LispVal -- ^ The nested list to modify
        -> [Int]   -- ^ Location to insert the new element, from top-most -> leaf 
                   --   (EG: [1, 2] means add to the second top-most list, at its 3rd position)
        -> LispVal -- ^ Value to insert 
        -> LispVal -- ^ Resulant list
setData (List lData) ellipsisIndex@(i:is) val = do
  if length is > 0 && length lData < i + 1 -- Fill "holes" as long as they are not at the leaves
     then set $ fill lData $ i + 1
     else set lData

 where 

--  set listData = case (snd (trace ("content = " ++ show content) content)) of
  set listData = do
    let content = splitAt i listData
    case (snd content) of
      [] -> List $ listData ++ [val]
      [c] ->    if length is < 1
                   then List $ (fst content) ++ [val] ++ [c] -- Base case - Requested pos must be one less than c
                   else List $ (fst content) ++ [setData c is val]
      (c:cs) -> if length is < 1
                   then List $ (fst content) ++ [val] ++ [c] ++ (cs) -- Base case - Requested pos must be one less than c
                   else List $ (fst content) ++ [setData c is val] ++ (cs) 

-- |Compare actual input with expected
cmp input expected = do
  putStrLn $ show input
  putStrLn $ show $ assert (eqVal expected input) input

-- |Run this function to test the above code
test :: IO ()
test = do
  cmp (setData (List [Number 1, Number 2, Number 3, Number 4]) [4] (Number 5)) 
               (List [Number 1, Number 2, Number 3, Number 4, Number 5])

  cmp (setData (List [Number 1, Number 2, Number 3, Number 4]) [1] (Number 5)) 
               (List [Number 1, Number 5, Number 2, Number 3, Number 4])

  cmp (setData (List [List [Number 1, Number 2], List [Number 3, Number 4, Number 5]]) [1, 3] (Number 6)) 
               (List [List [Number 1, Number 2], List [Number 3, Number 4, Number 5, Number 6]])

  cmp (setData (List [List [Number 1, Number 2], List [Number 3, Number 4, Number 5]]) [1, 2] (Number 6)) 
               (List [List [Number 1, Number 2], List [Number 3, Number 4, Number 6, Number 5]])

  cmp (setData (List [List [Number 1, Number 2], List [Number 3, Number 4, Number 5]]) [0, 2] (Number 6)) 
               (List [List [Number 1, Number 2, Number 6], List [Number 3, Number 4, Number 5]])

  let a = create 2
  cmp a 
      (List [List []])

  let b = setData a [0, 0] $ Atom "test"
  cmp b (List [List [Atom "test"]])

  let c = setData b [0, 1] $ Atom "test2"
  cmp c (List [List [Atom "test", Atom "test2"]])

  -- Illustrates an important point, that if we are adding into 
  -- a 'hole', we need to create a list there first
  let cc = setData b [1, 0] $ Atom "test2"
  cmp cc (List [List [Atom "test"], List [Atom "test2"]])

  let cc2 = setData b [1, 4] $ Atom "test2"
  cmp cc2 (List [List [Atom "test"], List [Atom "test2"]])

  let cc3 = setData b [4, 0] $ Atom "test2"
  cmp cc3 (List [List [Atom "test"], List [], List [], List [], List [Atom "test2"]])

  cmp (setData (List []) [4, 0] (Number 5)) 
               (List [List [], List [], List [], List [], List [Number 5]])


