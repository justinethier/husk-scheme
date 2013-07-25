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
import Language.Scheme.Util
import Language.Scheme.Variables -- Scheme variable operations
--import Control.Monad (when)
import Control.Monad.Error
import qualified Data.Char as DC
import qualified Data.List as DL
import System.Cmd (system)
import System.Console.GetOpt
import qualified System.Console.Haskeline as HL
import qualified System.Console.Haskeline.Completion as HLC
import System.Environment
import System.Exit (ExitCode (..), exitWith, exitFailure)
import System.IO

main :: IO ()
main = do 
  args <- getArgs

  let (actions, nonOpts, msgs) = getOpt Permute options args
  opts <- foldl (>>=) (return defaultOptions) actions
  let Options {optSchemeRev = schemeRev} = opts

  if null nonOpts 
     then do 
       showBanner
       runRepl schemeRev
     else runOne schemeRev nonOpts

-- Command line options section
data Options = Options {
    optSchemeRev :: String -- RxRS version
    }

-- |Default values for the command line options
defaultOptions :: Options
defaultOptions = Options {
    optSchemeRev = "5"
    }
options :: [OptDescr (Options -> IO Options)]
options = [
  Option ['r'] ["revision"] (ReqArg writeRxRSVersion "Scheme") "scheme RxRS version",
  Option ['h', '?'] ["help"] (NoArg showHelp) "show usage information"
  ]

writeRxRSVersion arg opt = return opt { optSchemeRev = arg }

showHelp :: Options -> IO Options
showHelp _ = do
  putStrLn "Usage: huski [options] [file]"
  putStrLn ""
  putStrLn "  huski is the husk scheme interpreter."
  putStrLn ""
  putStrLn "  File is a scheme source file to execute. If no file is specified"
  putStrLn "  the husk REPL will be started."
  putStrLn ""
  putStrLn "  Options may be any of the following:"
  putStrLn ""
  putStrLn "  -h -? --help      Display this information"
-- TODO: specify scheme rev via command line
--  putStrLn "  -r --revision     Specify the scheme revision to use:"
--  putStrLn ""
--  putStrLn "                      5 - r5rs (default)"
--  putStrLn "                      7 - r7rs small"
  putStrLn ""
  exitWith ExitSuccess

-- REPL Section
flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

-- |Execute a single scheme file from the command line
runOne :: String -> [String] -> IO ()
runOne _ args = do
  env <- r5rsEnv >>= flip extendEnv
                          [((varNamespace, "args"),
                           List $ map String $ drop 1 args)]

  result <- (runIOThrows $ liftM show $ evalLisp env (List [Atom "load", String (args !! 0)]))
  case result of
    Just errMsg -> putStrLn errMsg
    _  -> do 
      -- Call into (main) if it exists...
      alreadyDefined <- liftIO $ isBound env "main"
      let argv = List $ map String $ args
      when alreadyDefined (do 
        mainResult <- (runIOThrows $ liftM show $ evalLisp env (List [Atom "main", List [Atom "quote", argv]]))
        case mainResult of
          Just errMsg -> putStrLn errMsg
          _  -> return ())

-- |Start the REPL (interactive interpreter)
runRepl :: String -> IO ()
runRepl _ = do
    env <- r5rsEnv

    let settings = HL.Settings (completeScheme env) Nothing True
    HL.runInputT settings (loop env)
    --runInputT defaultSettings (loop env)
    where
        loop :: Env -> HL.InputT IO ()
        loop env = do
            minput <- HL.getInputLine "huski> "
            case minput of
                Nothing -> return ()
                Just "quit" -> return ()
                Just "" -> loop env -- FUTURE: integrate with strip to ignore inputs of just whitespace
                Just input -> do result <- liftIO (evalString env input)
                                 if (length result) > 0
                                    then do HL.outputStrLn result
                                            loop env
                                    else loop env

completeScheme env (lnL@('"' : _), lnR) = do
    -- TODO: this could be alot better, should complete if there are chars
    liftIO $ HLC.completeFilename (lnL, lnR)
completeScheme env (lnL, lnR) = do
   complete $ readAtom lnL
 where
  -- TODO: complete ('"' : _) = liftIO $ HLC.completeFilename (lnL, lnR)
  complete p = do
   let pre = reverse p 
-- !!
-- TODO: if in middle of a string, try file completion
-- !!

   -- Get list of possible completions from ENV
   xps <- recExportsFromEnv env
   let xps' = filter (\ (Atom a) -> DL.isPrefixOf pre a) xps
   let xpCs = map (\ (Atom a) -> HL.Completion a a False) xps'

   -- Get unused portion of the left-hand string
   let unusedLnL = case DL.stripPrefix (reverse pre) lnL of
                     Just s -> s
                     Nothing -> lnL
   return (unusedLnL, xpCs)

-- Read until the end of the current symbol (atom), if there is one
-- TODO: this could probably be made much better
readAtom (c:cs)
-- TODO:    | c == '"' = []
    | c == '(' = []
    | DC.isSpace(c) = []
    | otherwise = (c : readAtom cs)
readAtom [] = []
-- End REPL Section

-- Begin Util section, of generic functions

{- Remove leading/trailing white space from a string; based on corresponding Python function
   Code taken from: http://gimbo.org.uk/blog/2007/04/20/splitting-a-string-in-haskell/ -}
strip :: String -> String
strip s = dropWhile ws $ reverse $ dropWhile ws $ reverse s
    where ws = (`elem` [' ', '\n', '\t', '\r'])

-- End Util
