{-
 - husk scheme interpreter
 -
 - A lightweight dialect of R5RS scheme.
 -
 - This file implements a REPL "shell" to host the interpreter, and also
 - allows execution of stand-alone files containing Scheme code.
 -
 - @author Justin Ethier
 -
 - -}

module Main where
import Scheme.Core
import Scheme.Macro
import Scheme.Numerical
import Scheme.Parser
import Scheme.Types
import Scheme.Variables
import Complex
import Control.Monad
import Control.Monad.Error
import Char
import Data.Array
import Data.IORef
import qualified Data.Map
import Maybe
import List
import IO hiding (try)
import Numeric
import Ratio
import System.Environment
import System.Console.Haskeline

main :: IO ()
main = do args <- getArgs
          if null args then do showBanner
                               runRepl
                       else runOne $ args

-- REPL Section
flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

evalString :: Env -> String -> IO String
evalString env expr = runIOThrows $ liftM show $ (liftThrows $ readExpr expr) >>= macroEval env >>= eval env

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = evalString env expr >>= putStrLn

runOne :: [String] -> IO ()
runOne args = do
  env <-primitiveBindings >>= flip bindVars [((varNamespace, "args"), List $ map String $ drop 1 args)]
  (runIOThrows $ liftM show $ eval env (List [Atom "load", String (args !! 0)]))
     >>= hPutStrLn stderr  -- echo this or not??

  -- Call into (main) if it exists...
  alreadyDefined <- liftIO $ isBound env "main"
  let argv = List $ map String $ args
  if alreadyDefined
     then (runIOThrows $ liftM show $ eval env (List [Atom "main", List [Atom "quote", argv]])) >>= hPutStrLn stderr
     else (runIOThrows $ liftM show $ eval env $ Nil "") >>= hPutStrLn stderr

showBanner :: IO ()
showBanner = do
  putStrLn " __  __     __  __     ______     __  __                             "
  putStrLn "/\\ \\_\\ \\   /\\ \\/\\ \\   /\\  ___\\   /\\ \\/ /     Scheme Interpreter " 
  putStrLn "\\ \\  __ \\  \\ \\ \\_\\ \\  \\ \\___  \\  \\ \\  _\\\"-.  Version 1.0"
  putStrLn " \\ \\_\\ \\_\\  \\ \\_____\\  \\/\\_____\\  \\ \\_\\ \\_\\  (c) 2010 Justin Ethier "
  putStrLn "  \\/_/\\/_/   \\/_____/   \\/_____/   \\/_/\\/_/  github.com/justinethier/husk-scheme "
  putStrLn ""

runRepl :: IO ()
runRepl = do
    env <- primitiveBindings
    evalString env "(load \"stdlib.scm\")" -- Load standard library into the REPL
    runInputT defaultSettings (loop env) 
    where 
        loop :: Env -> InputT IO ()
        loop env = do
            minput <- getInputLine "huski> "
            case minput of
                Nothing -> return ()
                Just "quit" -> return ()
                Just "" -> loop env -- FUTURE: integrate with strip to ignore inputs of just whitespace
                Just input -> do result <- liftIO (evalString env input)
                                 if (length result) > 0
                                    then do outputStrLn result
                                            loop env
                                    else loop env
-- End REPL Section

-- Begin Util section, of generic functions

-- Remove leading/trailing white space from a string; based on corresponding Python function
-- Code taken from: http://gimbo.org.uk/blog/2007/04/20/splitting-a-string-in-haskell/
strip :: String -> String
strip s = dropWhile ws $ reverse $ dropWhile ws $ reverse s
    where ws = (`elem` [' ', '\n', '\t', '\r'])

-- End Util
