{- A temporary file to test out ideas for building the data structure required by Macro.hs 

 test via: ghci test.hs
-}
module Test (setData) where
import Language.Scheme.Types
import Control.Exception
import Debug.Trace

--setData :: [a] -> [Int] -> a -> [a]
setData (List dStruct) ellipsisIndex@(i:is) val = do
-- TODO: we want to break list into 3 parts - l:l[eIdx][level]:ls
  let content = splitAt (i) dStruct
  case (snd (trace ("content = " ++ show content) content)) of
--  case (snd content) of
    [] -> List $ dStruct ++ [val]
    -- TODO: problem here is that lists are not homogenous; they consist of both [1] and 1.
    --  this should be solved by using lispval's as list nodes, since those can contain lists and scalars.
    --  try this and see if it will work
    --  TBD: will it break the assertions, though? should be able to make those work with the eqv function
    [c] ->    List $ (fst content) ++ [setData c is val]
    (c:cs) -> List $ (fst content) ++ [setData c is val] ++ (cs) 
--  where extractList (List a) = a
setData dStruct [] _ = dStruct


-- TODO: focus on this section and get a set of test cases that can be run
test input expected = do
  putStrLn $ show input
  putStrLn $ show $ assert (eqVal expected input) input

main :: IO ()
main = do
  test (setData (List [Number 1, Number 2, Number 3, Number 4]) [4] (Number 5)) 
                (List [Number 1, Number 2, Number 3, Number 4, Number 5])

  test (setData (List [List [Number 1, Number 2], List [Number 3, Number 4, Number 5]]) [1, 3] (Number 6)) 
                (List [List [Number 1, Number 2], List [Number 3, Number 4, Number 5, Number 6]])

--  test (setData [[1, 2], [3, 4, 5]] [1, 3] [6]) 
--                [[1, 2], [3, 4, 5, 6]]

{-
  test (setData [[1, 2], [3, 4, 5]] [1, 2] [6]) 
                [[1, 2], [3, 4, 6, 5]]

  test (setData [[1, 2], [3, 4, 5]] [0, 2] [6]) 
                [[1, 2, 6], [3, 4, 5]]
                -}
