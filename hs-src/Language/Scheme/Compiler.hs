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
import qualified Data.List
{-
import Data.Array
import qualified Data.Map
-}
import System.IO

compiledPrimitives :: [(String, String)]
compiledPrimitives = [
  ("write", "writeProc (\\ port obj -> hPrint port obj)")
 ,("+", "numAdd")
 ,("-", "numSub")
 ,("*", "numMul")
 ,("/", "numDiv")]

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
compile _ (Atom a) = return $ "Atom " ++ a

-- TODO: this is not good enough; a line of scheme may need to be compiled into many lines of haskell,
--  for example

compile env args@(List (Atom func : params)) = do
  -- TODO: all of this needs to be based off of the equivalent code from Core.hs, and generated code needs to be in CPS
  -- however, this is a simple example of how this might work within a compiler...
--  cfunc <- lookup
  case lookup func compiledPrimitives of
    (Just a) -> do
      ps <- mapM (compile env) params
      return $ a ++ " [ " ++ (Data.List.intercalate ", " ps) ++ " ] "
--      return $ a ++ " [ " ++ (unwords ps) ++ " ] "
    Nothing -> throwError $ Default $ "Function definition not found: " ++ func 

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
