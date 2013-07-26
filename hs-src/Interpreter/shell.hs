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
import qualified Language.Scheme.Core as LSC -- Scheme Interpreter
import Language.Scheme.Types                 -- Scheme data types
import qualified Language.Scheme.Util as LSU (strip)
import qualified Language.Scheme.Variables as LSV -- Scheme variable operations
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
       LSC.showBanner
       runRepl schemeRev
     else runOne schemeRev nonOpts

--
-- Command line options section
--

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

--
-- REPL Section
--

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

-- |Execute a single scheme file from the command line
runOne :: String -> [String] -> IO ()
runOne _ args = do
  env <- LSC.r5rsEnv >>= flip LSV.extendEnv
                          [((LSV.varNamespace, "args"),
                           List $ map String $ drop 1 args)]

  result <- (LSC.runIOThrows $ liftM show $ 
             LSC.evalLisp env (List [Atom "load", String (args !! 0)]))
  case result of
    Just errMsg -> putStrLn errMsg
    _  -> do 
      -- Call into (main) if it exists...
      alreadyDefined <- liftIO $ LSV.isBound env "main"
      let argv = List $ map String $ args
      when alreadyDefined (do 
        mainResult <- (LSC.runIOThrows $ liftM show $ 
                       LSC.evalLisp env (List [Atom "main", List [Atom "quote", argv]]))
        case mainResult of
          Just errMsg -> putStrLn errMsg
          _  -> return ())

-- |Start the REPL (interactive interpreter)
runRepl :: String -> IO ()
runRepl _ = do
    env <- LSC.r5rsEnv

    let settings = HL.Settings (completeScheme env) Nothing True
    HL.runInputT settings (loop env)
    where
        loop :: Env -> HL.InputT IO ()
        loop env = do
            minput <- HL.getInputLine "huski> "
            case minput of
                Nothing -> return ()
                Just i -> do 
                  case LSU.strip i of
                    "quit" -> return ()
                    "" -> loop env -- ignore inputs of just whitespace
                    input -> do
                        result <- liftIO (LSC.evalString env input)
                        if (length result) > 0
                           then do HL.outputStrLn result
                                   loop env
                           else loop env

-- |Auto-complete using scheme symbols
completeScheme env (lnL, lnR) = do
   complete $ reverse $ readAtom lnL
 where
  complete ('"' : _) = do
    -- Special case, inside a string it seems more
    -- useful to autocomplete filenames
    liftIO $ HLC.completeFilename (lnL, lnR)

  complete pre = do
   -- Get list of possible completions from ENV
   xps <- LSV.recExportsFromEnv env
   let allDefs = xps ++ specialForms
   let allDefs' = filter (\ (Atom a) -> DL.isPrefixOf pre a) allDefs
   let comps = map (\ (Atom a) -> HL.Completion a a False) allDefs'

   -- Get unused portion of the left-hand string
   let unusedLnL = case DL.stripPrefix (reverse pre) lnL of
                     Just s -> s
                     Nothing -> lnL
   return (unusedLnL, comps)

  -- Not loaded into an env, so we need to list them here
  specialForms = map (\ s -> Atom s) [ 
       "define"  
     , "define-syntax" 
     , "expand"
     , "hash-table-delete!"
     , "hash-table-set!"
     , "if"
     , "lambda"
     , "let-syntax" 
     , "letrec-syntax" 
     , "quote"
     , "set!"
     , "set-car!"
     , "set-cdr!"
     , "string-set!"
     , "vector-set!"]

  -- Read until the end of the current symbol (atom), if there is one.
  -- There is also a special case for files if a double-quote is found.
  readAtom (c:cs)
    | c == '"' = ['"'] -- Save to indicate file completion to caller
    | c == '(' = []
    | c == '[' = []
    | DC.isSpace(c) = []
    | otherwise = (c : readAtom cs)
  readAtom [] = []
