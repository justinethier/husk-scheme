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

runOne :: [String] -> IO ()
runOne args = do
  {- Use this to suppress unwanted output.
     Makes this unix-specific, but as of now
     everything else is anyway, so... -}
  nullIO <- openFile "/dev/null" WriteMode

  stdlib <- getDataFileName "stdlib.scm"
  env <- primitiveBindings >>= flip extendEnv
                                   [((varNamespace, "args"),
                                    List $ map String $ drop 1 args)]
  _ <- evalString env $ "(load \"" ++ stdlib ++ "\")" -- Load standard library
  (runIOThrows $ liftM show $ evalLisp env (List [Atom "load", String (args !! 0)]))
     >>= hPutStr nullIO

  -- Call into (main) if it exists...
  alreadyDefined <- liftIO $ isBound env "main"
  let argv = List $ map String $ args
  if alreadyDefined
     then (runIOThrows $ liftM show $ evalLisp env
            (List [Atom "main", List [Atom "quote", argv]])) >>= hPutStr stderr
     else (runIOThrows $ liftM show $ evalLisp env (Nil "")) >>= hPutStr stderr

runRepl :: IO ()
runRepl = do
    stdlib <- getDataFileName "stdlib.scm"
    env <- primitiveBindings
    _ <- evalString env $ "(load \"" ++ stdlib ++ "\")" -- Load standard library into the REPL
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
