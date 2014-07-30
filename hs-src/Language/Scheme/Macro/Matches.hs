{- | 
Module      : Language.Scheme.Macro.Matches
Copyright   : Justin Ethier
Licence     : MIT (see LICENSE in the distribution)

Maintainer  : github.com/justinethier
Stability   : experimental
Portability : portable

This module contains utility functions used to support macro processing,
by storing and/or manipulating data involving 0-or-many matches.
-}
module Language.Scheme.Macro.Matches (getData, setData) where
import Language.Scheme.Types
import Control.Exception
--import Debug.Trace

-- |Create a nested list
_create :: Int     -- ^ Number of nesting levels
       -> LispVal -- ^ Empty nested list
_create level 
    | level < 1     = Nil "" -- Error
    | level == 1    = List []
    | otherwise = List [_create $ level - 1]

-- |Fill any empty /holes/ in a list from the beginning to the given length
--
-- The problem here is how to handle case when a requested insertion leaves /holes/.
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
        -> LispVal -- ^ Value read, or @Nil@ if none
getData (List lData) (i:is) = do
  if length lData < i
     then Nil "" -- Error: there are not enough elements in the list
     else do
       let lst = drop i lData
       if not (null lst)
          then getData (head lst) is
          else Nil "" -- Error: not enough elements in list
getData val [] = val -- Base case: we have found the requested element
getData val _ = val -- Should never be reached, just give up and return val 

-- |Add an element to the given nested list
setData :: LispVal -- ^ The nested list to modify
        -> [Int]   -- ^ Location to insert the new element, from top-most to the leaf.
                   --   For example [1, 2] means add to the second top-most list, at
                   --   its 3rd position.
        -> LispVal -- ^ Value to insert 
        -> LispVal -- ^ Resulant list
setData (List lData) (i:is) val = do
  -- Fill /holes/ as long as they are not at the leaves.
  --
  -- This is because,  when a match occurs it happens 0 or more  times.
  -- Therefore it is not  possible (at the leaves) for a match to occur 
  -- where that match is not placed at the end of the list. For example
  -- if the pattern is:
  --
  -- a ...
  --
  -- And the input is:
  --
  -- 1 2 3
  --
  -- Then we always store the first match in position 0, second in 1, etc. 
  -- There  are no  holes  in this  case  because there is never a  reason 
  -- to skip any of these positions.
  if not (null is) && length lData < i + 1 
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
                   then List $ (fst content) ++ [val] ++ [c] ++ cs -- Base case - Requested pos must be one less than c
                   else List $ (fst content) ++ [setData c is val] ++ cs 

setData _ _ val = val -- Should never be reached; just return val

-- |Compare actual input with expected
_cmp :: LispVal -> LispVal -> IO ()
_cmp input expected = do
  print input
  print (assert (eqVal expected input) input)

-- |Run this function to test the above code
_test :: IO ()
_test = do
  _cmp (setData (List [Number 1, Number 2, Number 3, Number 4]) [4] (Number 5)) 
               (List [Number 1, Number 2, Number 3, Number 4, Number 5])

  _cmp (setData (List [Number 1, Number 2, Number 3, Number 4]) [1] (Number 5)) 
               (List [Number 1, Number 5, Number 2, Number 3, Number 4])

  _cmp (setData (List [List [Number 1, Number 2], List [Number 3, Number 4, Number 5]]) [1, 3] (Number 6)) 
               (List [List [Number 1, Number 2], List [Number 3, Number 4, Number 5, Number 6]])

  _cmp (setData (List [List [Number 1, Number 2], List [Number 3, Number 4, Number 5]]) [1, 2] (Number 6)) 
               (List [List [Number 1, Number 2], List [Number 3, Number 4, Number 6, Number 5]])

  _cmp (setData (List [List [Number 1, Number 2], List [Number 3, Number 4, Number 5]]) [0, 2] (Number 6)) 
               (List [List [Number 1, Number 2, Number 6], List [Number 3, Number 4, Number 5]])

  let a = _create 2
  _cmp a 
      (List [List []])

  let b = setData a [0, 0] $ Atom "test"
  _cmp b (List [List [Atom "test"]])

  let c = setData b [0, 1] $ Atom "test2"
  _cmp c (List [List [Atom "test", Atom "test2"]])


-- An invalid test case because it attempts to initialize a leaf by adding at a non-zero leaf position.
--_cmp (setData (List []) [0, 1, 2] $ Atom "test") 
--               (List [List[List [], List[List [], List[], Atom "test"]]])
--   A correct test is below:
  _cmp (setData (List []) [0, 1, 0] $ Atom "test") 
               (List [List[List [], List[Atom "test"]]])

  -- Illustrates an important point, that if we are adding into 
  -- a /hole/, we need to create a list there first
  let cc = setData b [1, 0] $ Atom "test2"
  _cmp cc (List [List [Atom "test"], List [Atom "test2"]])

  let cc2 = setData b [1, 4] $ Atom "test2"
  _cmp cc2 (List [List [Atom "test"], List [Atom "test2"]])

  let cc3 = setData b [4, 0] $ Atom "test2"
  _cmp cc3 (List [List [Atom "test"], List [], List [], List [], List [Atom "test2"]])

  _cmp (setData (List []) [4, 0] (Number 5)) 
               (List [List [], List [], List [], List [], List [Number 5]])

  _cmp (getData (List [List [List [], List [Number 1, Number 2, Number 3, Number 4]]]) [0, 1, 2]) 
               (Number 3)

--  _cmp (getData (List [List [List [], List [Number 1, Number 2, Number 3, Number 4]]]) [0, 1, 20]) 
--               (Nil "")

  _cmp (getData (List [List [List [], List [Atom "1", Number 2, Number 3, Number 4]]]) [0, 1, 0]) 
               (Atom "1")

  -- Real world case, we would like to take the list (all leaves) at [0, 1]
  _cmp (getData (List [List [List [], List [Atom "1", Number 2, Number 3, Number 4]]]) [0, 1]) 
               (List [Atom "1", Number 2, Number 3, Number 4])
