{- Hello, world from Haskell 
 -
 - -}
module Main where
import System.Environment

main :: IO ()
main = do
	args <- getArgs
	putStrLn ("Result is: " ++ show(read(args !! 0) / read(args !! 1)))
	putStrLn ("What is your name?")
	name <- getLine
	putStrLn ("Hello, " ++ name)
