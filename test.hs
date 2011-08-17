{- A temporary file to test out ideas for building the data structure required by Macro.hs 

 test via: ghci test.hs
-}
module Test (setData) where
import Language.Scheme.Types
import Control.Exception
import Debug.Trace

--setData :: [a] -> [Int] -> a -> [a]
setData dStruct ellipsisIndex@(i:is) val = do
-- TODO: we want to break list into 3 parts - l:l[eIdx][level]:ls
  let content = splitAt (i) dStruct
  case (snd (trace ("content = " ++ show content) content)) of
--  case (snd content) of
    [] -> dStruct ++ [val]
    -- TODO: problem here is that lists are not homogenous; they consist of both [1] and 1.
    --  this should be solved by using lispval's as list nodes, since those can contain lists and scalars.
    --  try this and see if it will work
    --  TBD: will it break the assertions, though? should be able to make those work with the eqv function
    [[c]] -> (fst content) ++ (setData [c] is val)
    (c:cs) -> (fst content) ++ (setData [c] is val) ++ (cs) 
setData dStruct [] _ = dStruct


-- TODO: focus on this section and get a set of test cases that can be run
test input expected = do
  putStrLn $ show input
  putStrLn $ show $ assert (input == expected) input

main :: IO ()
main = do
  test (setData [[1], [2], [3], [4]] [4] [5]) 
                [[1], [2], [3], [4], [5]]

  test (setData [[1, 2], [3, 4, 5]] [1, 3] [6]) 
                [[1, 2], [3, 4, 5, 6]]

{-
  test (setData [[1, 2], [3, 4, 5]] [1, 2] [6]) 
                [[1, 2], [3, 4, 6, 5]]

  test (setData [[1, 2], [3, 4, 5]] [0, 2] [6]) 
                [[1, 2, 6], [3, 4, 5]]
                -}
