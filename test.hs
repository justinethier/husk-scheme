{- A temporary file to test out ideas for building the data structure required by Macro.hs -}
module Test (setData) where
--import Language.Scheme.Typesi

setData :: [a] -> [Integer] -> a -> [a]
setData dStruct ellipsisIndex@(i:is) val level = do
-- TODO: we want to break list into 3 parts - l:l[eIdx][level]:ls
  content <- splitAt 
  dStruct ++ [val]  

main :: IO ()
main = do
  putStr $ show $ setData [[1, 2], [3, 4, 5]] [2, 3] [6] 0
--  putStr $ show $ setData [[1], [2], [3], [4]] [4] [5]
