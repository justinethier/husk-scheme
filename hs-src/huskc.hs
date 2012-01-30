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
import Language.Scheme.Core
import Language.Scheme.Types     -- Scheme data types
import Language.Scheme.Variables -- Scheme variable operations
import Control.Monad.Error
import System.Cmd (system)
import System.Exit (ExitCode (..), exitWith, exitFailure)
import System.IO
import System.Environment

main :: IO ()
main = do args <- getArgs
          if null args then showUsage
                       else process args
showUsage :: IO ()
showUsage = do
  putStrLn "huskc: no input files"

process :: [String] -> IO ()
process args = do
  let filename = args !! 0
  env <- liftIO $ nullEnv
  result <- (runIOThrows $ liftM show $ compileSchemeFile env filename)
  case result of
   "" -> compileHaskellFile $ filename ++ ".out" -- TODO: just chop off file extension
   _ -> putStrLn result

compileSchemeFile :: Env -> String -> IOThrowsError LispVal
compileSchemeFile env filename = do
  comp <- compileLisp env filename "run"
  outH <- liftIO $ openFile "_tmp.hs" WriteMode
  _ <- liftIO $ writeList outH header
  _ <- liftIO $ writeList outH $ map show comp
  _ <- liftIO $ hClose outH
  if not (null comp)
     then return $ Nil "" -- Dummy value
     else throwError $ Default "Empty file" --putStrLn "empty file"

compileHaskellFile :: String -> IO() --ThrowsError LispVal
compileHaskellFile filename = do
  let ghc = "ghc" -- Need to make configurable??
      exe = "test" -- TODO: allow as cmd line parameter
  compileStatus <- system $ ghc ++ " -cpp --make -package ghc -fglasgow-exts -o " ++ filename ++ " _tmp.hs hs-src/Language/Scheme/Primitives.hs hs-src/Language/Scheme/Parser.hs hs-src/Language/Scheme/Numerical.hs hs-src/Language/Scheme/Core.hs hs-src/Language/Scheme/Macro.hs hs-src/Language/Scheme/FFI.hs hs-src/Language/Scheme/Macro/Matches.hs"

-- TODO: delete intermediate hs files if requested

  case compileStatus of
    ExitFailure _code -> exitWith compileStatus
    ExitSuccess -> return ()

writeList outH (l : ls) = do
  hPutStrLn outH l
  writeList outH ls
writeList outH _ = do
  hPutStr outH ""


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
