{- A temporary file to test out ideas for building the data structure required by Macro.hs 

 test via: ghci test.hs
-}
module Test (setData) where
import Language.Scheme.Types
import Control.Exception
import Debug.Trace

-- |Create a nested list
create :: Int     -- ^ Number of nesting levels
       -> LispVal -- ^ Empty nested list
create level 
    | level < 1     = Nil "" -- Error
    | level == 1    = List []
    | otherwise = List [create $ level - 1]

-- TODO: how to handle case when requested insertion leaves "holes".
--  for example, in a 2-level nested list: ((1)) we have data as pos 0 but have none at pos 1.
--  if the code then tries to add an element 2 at pos 2 we should end up with:
--
--    ((1) () (2))
--
--  The current code is not this smart, but it needs to be becuase that is exactly how macros
--  work.

-- |Add an element to the given nested list
setData :: LispVal -- ^ The nested list to modify
        -> [Int]   -- ^ Location to insert the new element, from top-most -> leaf 
                   --   (EG: [1, 2] means add to the second top-most list, at its 3rd position)
        -> LispVal -- ^ Value to insert 
        -> LispVal -- ^ Resulant list
setData (List dStruct) ellipsisIndex@(i:is) val = do
  let content = splitAt i dStruct
--  case (snd (trace ("content = " ++ show content) content)) of
  case (snd content) of
    [] -> List $ dStruct ++ [val]
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

-- |Run this method to test the above code
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

-- TODO: Illustrates an important point, that if we are adding into a 'hole', we need to create a list there first
  let cc = setData b [1, 0] $ Atom "test2"
  cmp cc (List [List [Atom "test"], List [Atom "test2"]])


