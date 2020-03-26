{- |
Module      : Main
Copyright   : Justin Ethier
Licence     : MIT (see LICENSE in the distribution)

Maintainer  : github.com/justinethier
Stability   : experimental
Portability : portable

This file implements a REPL /shell/ to host the interpreter, and also
allows execution of stand-alone files containing Scheme code.
-}

module Main where
import qualified Language.Scheme.Core as LSC -- Scheme Interpreter
import Language.Scheme.Types                 -- Scheme data types
import qualified Language.Scheme.Util as LSU (countAllLetters, countLetters, strip)
import qualified Language.Scheme.Variables as LSV -- Scheme variable operations
import Control.Monad.Except
import qualified Data.Char as DC
import qualified Data.List as DL
import Data.Maybe (fromMaybe)
import System.Console.GetOpt
import qualified System.Console.Haskeline as HL
import qualified System.Console.Haskeline.Completion as HLC
import System.Environment
import System.Exit (exitSuccess)
import System.IO

main :: IO ()
main = do 
  args <- getArgs

  let (actions, nonOpts, _) = getOpt Permute options args
  opts <- foldl (>>=) (return defaultOptions) actions
  let Options {optInter = interactive, optSchemeRev = schemeRev} = opts

  if null nonOpts
     then do 
       LSC.showBanner
       env <- liftIO $ getRuntimeEnv schemeRev
       runRepl env
     else do
         runOne (getRuntimeEnv schemeRev) nonOpts interactive

--
-- Command line options section
--

data Options = Options {
    optInter :: Bool,
    optSchemeRev :: String -- RxRS version
    }

-- |Default values for the command line options
defaultOptions :: Options
defaultOptions = Options {
    optInter = False,
    optSchemeRev = "5"
    }
options :: [OptDescr (Options -> IO Options)]
options = [
  Option ['i'] ["interactive"] (NoArg getInter) "load file and run REPL",
  Option ['r'] ["revision"] (ReqArg writeRxRSVersion "Scheme") "scheme RxRS version",
  Option ['h', '?'] ["help"] (NoArg showHelp) "show usage information"
  ]
 where
  getInter opt = return opt { optInter = True }
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
  putStrLn "  -h, --help      Display this information"
  putStrLn "  -i              Start interactive REPL after file is executed. This"
  putStrLn "                  option has no effect if a file is not specified. "
--  putStrLn "  --revision rev   Specify the scheme revision to use:"
--  putStrLn ""
--  putStrLn "                     5 - r5rs (default)"
--  putStrLn "                     7 - r7rs small"
  putStrLn ""
  exitSuccess

--
-- REPL Section
--

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

getRuntimeEnv :: String -> IO Env
getRuntimeEnv "7" = LSC.r7rsEnv
getRuntimeEnv _ = LSC.r5rsEnv

-- |Execute a single scheme file from the command line
runOne :: IO Env -> [String] -> Bool -> IO ()
runOne initEnv args interactive = do
  env <- initEnv >>= flip LSV.extendEnv
                          [((LSV.varNamespace, "args"),
                           List $ map String $ drop 1 args)]

  result <- (LSC.runIOThrows $ liftM show $ 
             LSC.evalLisp env (List [Atom "load", String (head args)]))
  _ <- case result of
    Just errMsg -> putStrLn errMsg
    _  -> do 
      -- Call into (main) if it exists...
      alreadyDefined <- liftIO $ LSV.isBound env "main"
      let argv = List $ map String args
      when alreadyDefined (do 
        mainResult <- (LSC.runIOThrows $ liftM show $ 
                       LSC.evalLisp env (List [Atom "main", List [Atom "quote", argv]]))
        case mainResult of
          Just errMsg -> putStrLn errMsg
          _  -> return ())
  when interactive (do
    runRepl env)

-- |Start the REPL (interactive interpreter)
runRepl :: Env -> IO ()
runRepl env' = do
    let settings = HL.Settings (completeScheme env') Nothing True
    HL.runInputT settings (loop env')
    where
        -- Main REPL loop
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
                        inputLines <- getMultiLine [input]
                        let input' = unlines inputLines
                        result <- liftIO (LSC.evalString env input')
                        if not (null result)
                           then do HL.outputStrLn result
                                   loop env
                           else loop env

        -- Read another input line, if necessary
        getMultiLine previous = do
          if test previous
            then do
              mb_input <- HL.getInputLine ""
              case mb_input of
                Nothing -> return previous
                Just input -> getMultiLine $ previous ++ [input]
            else return previous

        -- Check if we need another input line
        -- This just does a bare minimum, and could be more robust
        test ls = do
          let cOpen  = LSU.countAllLetters '(' ls
              cClose = LSU.countAllLetters ')' ls
          cOpen > cClose

-- |Auto-complete using scheme symbols
completeScheme :: Env -> (String, String) 
               -> IO (String, [HLC.Completion])
-- Right after a ')' it seems more useful to autocomplete next closed parenthesis
completeScheme _ (lnL@(')':_), _) = do
  let cOpen  = LSU.countLetters '(' lnL
      cClose = LSU.countLetters ')' lnL
  if cOpen > cClose
   then return (lnL, [HL.Completion ")" ")" False])
   else return (lnL, [])
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
   let unusedLnL = fromMaybe lnL (DL.stripPrefix (reverse pre) lnL)
   return (unusedLnL, comps)

  -- Not loaded into an env, so we need to list them here
  specialForms = map Atom [ 
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
    | DC.isSpace c = []
    | otherwise = (c : readAtom cs)
  readAtom [] = []
