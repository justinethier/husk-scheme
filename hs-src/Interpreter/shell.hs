{- |
Module      : Main
Copyright   : Justin Ethier
Licence     : MIT (see LICENSE in the distribution)

Maintainer  : github.com/justinethier
Stability   : experimental
Portability : portable

This file implements a REPL "shell" to host the interpreter, and also
allows execution of stand-alone files containing Scheme code.
-}

module Main where
import Paths_husk_scheme
import Language.Scheme.Core      -- Scheme Interpreter
import Language.Scheme.Types     -- Scheme data types
import Language.Scheme.Variables -- Scheme variable operations
--import Control.Monad (when)
import Control.Monad.Error
import System.IO
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

-- |Execute a single scheme file from the command line
runOne :: [String] -> IO ()
runOne args = do
  p <- primitiveBindings
  env <- extendEnv p Nothing $ [((varNamespace, "args"),
                                newList $ map String $ drop 1 args)]
  -- Load standard library
  _ <- loadLibraries env

  result <- (runIOThrows $ liftM show $ evalLisp env (newList [Atom "load", String (args !! 0)]))
  case result of
    Just errMsg -> putStrLn errMsg
    _  -> do 
      -- Call into (main) if it exists...
      alreadyDefined <- liftIO $ isBound env "main"
      let argv = newList $ map String $ args
      when alreadyDefined (do 
        mainResult <- (runIOThrows $ liftM show $ evalLisp env (newList [Atom "main", newList [Atom "quote", argv]]))
        case mainResult of
          Just errMsg -> putStrLn errMsg
          _  -> return ())

-- |Load standard libraries into the given environment
loadLibraries :: Env -> IO ()
loadLibraries env = do
  stdlib <- getDataFileName "stdlib.scm"
  srfi55 <- getDataFileName "srfi/srfi-55.scm" -- (require-extension)
  -- Load standard library
  _ <- evalString env $ "(load \"" ++ (escapeBackslashes stdlib) ++ "\")" 
  -- Load (require-extension), which can be used to load other SRFI's
  _ <- evalString env $ "(load \"" ++ (escapeBackslashes srfi55) ++ "\")"
  registerSRFI env 1

-- |Register the given SRFI
registerSRFI :: Env -> Integer -> IO ()
registerSRFI env num = do
 filename <- getDataFileName $ "srfi/srfi-" ++ show num ++ ".scm"
 _ <- evalString env $ "(register-extension '(srfi " ++ show num ++ ") \"" ++ 
  (escapeBackslashes filename) ++ "\")"
 return ()

-- |Start the REPL (interactive interpreter)
runRepl :: IO ()
runRepl = do
    env <- primitiveBindings
    _ <- loadLibraries env

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

{- Remove leading/trailing white space from a string; based on corresponding Python function
   Code taken from: http://gimbo.org.uk/blog/2007/04/20/splitting-a-string-in-haskell/ -}
strip :: String -> String
strip s = dropWhile ws $ reverse $ dropWhile ws $ reverse s
    where ws = (`elem` [' ', '\n', '\t', '\r'])

-- End Util
