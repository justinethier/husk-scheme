{- For now, a simple demonstration of a Hello, world from Haskell 
 - This file will be remove or repurposed in the future...
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
