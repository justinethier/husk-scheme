{- A temporary file to test out ideas for building the data structure required by Macro.hs 

 test via: ghci test.hs
-}
module Test (setData) where
--import Language.Scheme.Typesi

setData :: [a] -> [Int] -> a -> [a]
setData dStruct ellipsisIndex@(i:is) val = do
-- TODO: we want to break list into 3 parts - l:l[eIdx][level]:ls
  let content = splitAt (i - 1) dStruct
  case (snd content) of
--    [] ->
    [c] -> (fst content) ++ (setData c is val)
    (c:cs) -> (fst content) ++ (setData c is val) ++ (cs) 
  dStruct ++ [val]  
setData dStruct [] _ = dStruct

main :: IO ()
main = do
  putStr $ show $ setData [[1, 2], [3, 4, 5]] [2, 3] [6]
--  putStr $ show $ setData [[1], [2], [3], [4]] [4] [5]
