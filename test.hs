{- A temporary file to test out ideas for building the data structure required by Macro.hs 

 test via: ghci test.hs
-}
module Test (setData) where
import Control.Exception
import Debug.Trace

--setData :: [a] -> [Int] -> a -> [a]
setData dStruct ellipsisIndex@(i:is) val = do
-- TODO: we want to break list into 3 parts - l:l[eIdx][level]:ls
  let content = splitAt (i) dStruct
--  case (snd (trace ("content = " ++ show content) content)) of
  case (snd content) of
    [] -> [val]
    [c] -> (fst content) ++ (setData [c] is val)
    (c:cs) -> (fst content) ++ (setData [c] is val) ++ (cs) 
  dStruct ++ [val]  
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
  putStrLn $ show $ setData [[1, 2], [3, 4, 5]] [2, 3] [6]
  putStrLn $ show $ setData [[1, 2], [3, 4, 5]] [2, 2] [6]
  putStrLn $ show $ setData [[1, 2], [3, 4, 5]] [2, 1] [6]
-}
