{- |
Module      : Main
Copyright   : Justin Ethier
Licence     : MIT (see LICENSE in the distribution)

Maintainer  : github.com/justinethier
Stability   : experimental
Portability : portable

A front-end for an experimental compiler
-}

module Main where
import Paths_husk_scheme
import Language.Scheme.Compiler
import Language.Scheme.Types     -- Scheme data types
import Language.Scheme.Variables -- Scheme variable operations
import Control.Monad.Error
import System.IO
import System.Environment

main :: IO ()
main = do args <- getArgs
          if null args then showUsage
                       else comp args

comp :: [String] -> IO ()
comp args = do
  env <- liftIO $ nullEnv -- TODO: load an Env specifically for compilation (perhaps in the Compiler module)
  (runIOThrows $ liftM show $ compileLisp env $ args !! 0) >>= putStrLn

{-
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
-}
showUsage :: IO ()
showUsage = do
  putStrLn "huskc: no input files"
--  putStrLn ""
