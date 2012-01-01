{- |
Module      : Language.Scheme.Core
Copyright   : Justin Ethier
Licence     : MIT (see LICENSE in the distribution)

Maintainer  : github.com/justinethier
Stability   : experimental
Portability : portable

This module contains an experimental compiler of Scheme to Haskell 
-}

module Language.Scheme.Compiler where 
{-    (
      evalLisp
    , evalString
    , evalAndPrint
    , primitiveBindings
    ) where -}
{-
import qualified Language.Scheme.FFI
import qualified Language.Scheme.Macro
-}
import Language.Scheme.Numerical
import Language.Scheme.Parser
import Language.Scheme.Primitives
import Language.Scheme.Types
import Language.Scheme.Variables
import Control.Monad.Error
{-
import Data.Array
import qualified Data.Map
-}
import System.IO

header :: [String]
header = [
   "module Main where "
 , "import Language.Scheme.Numerical "
 , "import Language.Scheme.Primitives "
 , "import Language.Scheme.Types     -- Scheme data types "
 , "--import Language.Scheme.Variables -- Scheme variable operations "
 , "import Control.Monad.Error "
 , "import System.IO "
 , " "
 , "main :: IO String "
 , "main = do "
 , "  (runIOThrows $ liftM show $ run) "
 , " "
 , "run :: IOThrowsError LispVal "
 , "run = do "]

compileLisp :: Env -> String -> IOThrowsError LispVal
compileLisp env filename = do
  -- TODO: below does not really work when compiling an expression that evaluates to a value (eg: 1)
  comp <- load filename >>= mapM (compile env)
  outH <- liftIO $ openFile "_tmp.hs" WriteMode
  _ <- liftIO $ writeList outH header
  _ <- liftIO $ writeList outH comp
  _ <- liftIO $ hClose outH
  return $ String "" -- Dummy value
{-  if not (null comp)
     then do
     else putStrLn "empty file"
-}

writeList outH (l : ls) = do
  hPutStrLn outH l
  writeList outH ls
writeList outH _ = do
  hPutStr outH ""

compile :: Env -> LispVal -> IOThrowsError String 
compile _ (Number n) = return $ "Number " ++ (show n)

compile env args@(List _ : _) = do
 -- look up the function
 -- compile each arg (see prepareApply)
 -- emit haskell code to call the function, passing in each arg as parameters

{-
TODO:

- a function to load a scheme file
- a function to compile a line of scheme code to haskell
  TODO: can compilation happen one line at a time? for now that may be a good enough start

  assume compilation will need to use gensym/name-mangling to emit functions and maintain a mapping of
  previously-defined variables, in order to interleave them into the compiled program

  will need to emit equivalents to continueEval, apply, and other forms from the interpreter. we will need to
  maintain CPS within the compiled code

TBD: can we compile directly to haskell, or do we need an intermediate representation?
     maybe a direct compile is good enough for the initial proof-of-concept

-}
