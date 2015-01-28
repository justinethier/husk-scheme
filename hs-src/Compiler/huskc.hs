{- |
Module      : Main
Copyright   : Justin Ethier
Licence     : MIT (see LICENSE in the distribution)

Maintainer  : github.com/justinethier
Portability : portable

A front-end for the husk compiler
-}

module Main where
import Paths_husk_scheme
import Language.Scheme.Compiler
import Language.Scheme.Compiler.Types
import qualified Language.Scheme.Core
import Language.Scheme.Types     -- Scheme data types
import Language.Scheme.Variables -- Scheme variable operations
import Control.Monad.Error
import Data.Maybe (fromMaybe)
import System.Console.GetOpt
import System.FilePath (dropExtension)
import System.Environment
import System.Exit (ExitCode (..), exitSuccess, exitWith)
import System.IO
import System.Process (system)

main :: IO ()
main = do 

  -- Read command line args and process options
  args <- getArgs
  let (actions, nonOpts, _) = getOpt Permute options args
  opts <- foldl (>>=) (return defaultOptions) actions
  let Options {optOutput = output, optLibs = lib, optDynamic = dynamic, optCustomOptions = extra, optSchemeRev = langrev} = opts

  let debugOpt = False

  if null nonOpts
     then showUsage
     else do
        let inFile = head nonOpts
            outHaskell = (dropExtension inFile) ++ ".hs"
            outExec = fromMaybe (dropExtension inFile) output
            extraOpts = fromMaybe "" extra 
        process inFile outHaskell outExec lib dynamic extraOpts langrev debugOpt

-- 
-- For an explanation of the command line options code, see:
-- http://leiffrenzel.de/papers/commandline-options-in-haskell.html
--

-- |Data type to handle command line options that take parameters
data Options = Options {
    optOutput :: Maybe String, -- Executable file to write
    optLibs :: Bool, -- Debug flag, whether to compile standard libraries
    optDynamic :: Bool, -- Flag for dynamic linking of compiled executable
    optCustomOptions :: Maybe String, -- Custom options to ghc
    optSchemeRev :: String -- Scheme Language version
    }

-- |Default values for the command line options
defaultOptions :: Options
defaultOptions = Options {
    optOutput = Nothing,
    optLibs = True,
    optDynamic = False,
    optCustomOptions = Nothing, 
    optSchemeRev = "5"
    }

-- |Command line options
options :: [OptDescr (Options -> IO Options)]
options = [
  Option ['r'] ["revision"] (ReqArg writeRxRSVersion "Scheme") "scheme RxRS version",
  Option ['V'] ["version"] (NoArg showVersion) "show version number",
  Option ['h', '?'] ["help"] (NoArg showHelp) "show usage information",
  Option ['o'] ["output"] (ReqArg writeExec "FILE") "output file to write",
  Option ['d'] ["dynamic"] (NoArg getDynamic) "use dynamic linking for the compiled executable",
  Option ['x'] ["extra"] (ReqArg getExtraArgs "Args") "extra arguments to ghc",

  Option [] ["debug"] (NoArg showDebug) "show debug information",
  Option [] ["nolibs"] (NoArg getNoLibs) "a DEBUG option to use interpreted libraries instead of compiling them"
  ]
 where
  writeRxRSVersion arg opt = return opt { optSchemeRev = arg }
  -- |Determine executable file to write. 
  --  This version just takes a name from the command line option
  writeExec arg opt = return opt { optOutput = Just arg }
  getNoLibs opt = return opt { optLibs = False }
  getDynamic opt = return opt { optDynamic = True }
  getExtraArgs arg opt = return opt { optCustomOptions = Just arg }

-- TODO: would nice to have this as well as a /real/ usage printout, perhaps via --help

-- |Print a usage message
showUsage :: IO ()
showUsage = do
  putStrLn "huskc: no input files"

showHelp :: Options -> IO Options
showHelp _ = do
  putStrLn "Usage: huskc [options] file"
  putStrLn ""
  putStrLn "  Options:"
  putStrLn ""
  putStrLn "  -h, --help                Display this information"
  putStrLn "  -V, --version             Display husk version information"
--  putStrLn "  --revision rev        Specify the scheme revision to use:"
--  putStrLn ""
--  putStrLn "                          5 - r5rs (default)"
--  putStrLn "                          7 - r7rs small"
--  putStrLn ""
  putStrLn "  -o, --output filename     Write executable to the given filename"
  putStrLn "  -d, --dynamic             Use dynamic Haskell libraries, if available"
  putStrLn "                            (Requires libraries built via --enable-shared)"
  putStrLn "  -x, --extra args          Pass extra arguments directly to ghc"
  putStrLn ""
  exitSuccess

-- |Print debug information
showDebug :: Options -> IO Options
showDebug _ = do
  stdlib <- getDataFileName "lib/stdlib.scm"
  putStrLn $ "stdlib: " ++ stdlib
  putStrLn ""
  exitSuccess

-- |Print version information
showVersion :: Options -> IO Options
showVersion _ = do
  Language.Scheme.Core.showBanner
  exitSuccess

-- |High level code to compile the given file
process :: String -> String -> String -> Bool -> Bool -> String -> String -> Bool -> IO ()
process inFile outHaskell outExec libs dynamic extraArgs langrev debugOpt = do
  env <- case langrev of
            "7" -> Language.Scheme.Core.r7rsEnv'
            _ -> Language.Scheme.Core.r5rsEnv'
  stdlib <- getDataFileName "lib/stdlib.scm"
  srfi55 <- getDataFileName "lib/srfi/srfi-55.scm" -- (require-extension)

  let stdlibArg = if libs
                     then Just stdlib
                     else Nothing

  result <- (Language.Scheme.Core.runIOThrows $ liftM show $ compileSchemeFile env stdlibArg srfi55 inFile outHaskell langrev debugOpt)
  case result of
   Just errMsg -> putStrLn errMsg
   _ -> compileHaskellFile outHaskell outExec dynamic extraArgs

-- |Compile a scheme file to haskell
compileSchemeFile :: Env -> Maybe String -> String -> String -> String -> String -> Bool -> IOThrowsError LispVal
compileSchemeFile env stdlib srfi55 filename outHaskell langrev _ = do
  let conv :: LispVal -> String
      conv (String s) = s
      conv l = show l
      compileLibraries = case stdlib of
        Just _ -> True
        _ -> False

  -- TODO: clean this up later
  --moduleFile <- liftIO $ getDataFileName "lib/modules.scm"

  (String nextFunc, libsC, libSrfi55C, _) <- case (stdlib, langrev) of
    (Just stdlib', "5") -> do
      -- TODO: it is only temporary to compile the standard library each time. It should be 
      --       precompiled and just added during the ghc compilation
      libsC <- compileLisp env stdlib' "run" (Just "exec55")
      libSrfi55C <- compileLisp env srfi55 "exec55" (Just "exec55_3")
      --libModules <- compileLisp env moduleFile "exec55_2" (Just "exec55_3")
      liftIO $ Language.Scheme.Core.registerExtensions env getDataFileName
      return (String "exec", libsC, libSrfi55C, []) --libModules)
    (_, _) -> return (String "run", [], [], [])

  -- Initialize the compiler module and begin
  _ <- initializeCompiler env
  execC <- compileLisp env filename nextFunc Nothing

  -- Append any additional import modules
  List imports <- getNamespacedVar env 't' {-"internal"-} "imports"
  let moreHeaderImports = map conv imports

  outH <- liftIO $ openFile outHaskell WriteMode
  _ <- liftIO $ writeList outH headerComment
  _ <- liftIO $ writeList outH headerModule
  _ <- liftIO $ writeList outH $ map (\modl -> "import " ++ modl ++ " ") $ headerImports ++ moreHeaderImports
  filepath <- liftIO $ getDataFileName ""
  _ <- liftIO $ writeList outH $ header filepath compileLibraries langrev
  _ <- liftIO $ case compileLibraries of
    True -> do
      _ <- writeList outH $ map show libsC
      _ <- hPutStrLn outH " ------ END OF STDLIB ------"
      _ <- writeList outH $ map show libSrfi55C
      hPutStrLn outH " ------ END OF SRFI 55 ------"
      -- _ <- writeList outH $ map show libModules
      hPutStrLn outH " ------ END OF MODULES ------"
    False -> do
      hPutStrLn outH "exec _ _ _ _ = return $ Nil \"\"" -- Placeholder
  _ <- liftIO $ writeList outH $ map show execC
  _ <- liftIO $ hClose outH
  if not (null execC)
     then return $ Nil "" -- Dummy value
     else throwError $ Default "Empty file" --putStrLn "empty file"

-- |Compile the intermediate haskell file using GHC
compileHaskellFile :: String -> String -> Bool -> String -> IO() --ThrowsError LispVal
compileHaskellFile hsInFile objOutFile dynamic extraArgs = do
  let ghc = "ghc" -- Need to make configurable??
      dynamicArg = if dynamic then "-dynamic" else ""
  compileStatus <- system $ ghc ++ " " ++ dynamicArg ++ " " ++ extraArgs ++ " -cpp --make -package ghc -o " ++ objOutFile ++ " " ++ hsInFile

-- TODO: delete intermediate hs files if requested

  case compileStatus of
    ExitFailure _code -> exitWith compileStatus
    ExitSuccess -> return ()

-- |Helper function to write a list of abstract Haskell code to file
writeList :: Handle -> [String] -> IO ()
writeList outH (l : ls) = do
  hPutStrLn outH l
  writeList outH ls
writeList outH _ = do
  hPutStr outH ""


