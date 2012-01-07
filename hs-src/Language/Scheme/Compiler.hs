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

-- TODO: will probably need to differentiate functions that are in the IO monad 
--  and pure ones that are not (such as numAdd)
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
 , "import Language.Scheme.Core "
 , "import Language.Scheme.Numerical "
 , "import Language.Scheme.Primitives "
 , "import Language.Scheme.Types     -- Scheme data types "
 , "import Language.Scheme.Variables -- Scheme variable operations "
 , "import Control.Monad.Error "
 , "import System.IO "
 , " "
 , "-- Stubs from core; these should eventually be rolled into a common module "
 , "apply :: LispVal -> LispVal -> [LispVal] -> IOThrowsError LispVal "
 , "apply cont (PrimitiveFunc func) args = do "
 , "  result <- liftThrows $ func args "
 , "  case cont of "
 , "    Continuation cEnv _ _ _ _ -> continueEval cEnv cont result "
 , "    _ -> return result "
 , "apply cont (IOFunc func) args = do "
 , "  result <- func args "
 , "  case cont of "
 , "    Continuation cEnv _ _ _ _ -> continueEval cEnv cont result "
 , "    _ -> return result "
 , "continueEval :: Env -> LispVal -> LispVal -> IOThrowsError LispVal "
 , "continueEval _ "
 , "            (Continuation cEnv (Just (HaskellBody func funcArgs)) "
 , "                               (Just (Continuation cce cnc ccc _ cdynwind)) "
 , "                                xargs _) "
 , "             val = func cEnv (Continuation cce cnc ccc xargs cdynwind) val funcArgs "
 , "continueEval _ (Continuation cEnv Nothing (Just cCont) _ _) val = continueEval cEnv cCont val "
 , "continueEval _ (Continuation _ Nothing Nothing _ _) val = return val "
 , "continueEval _ _ _ = throwError $ Default \"Internal error in continueEval\" "
 , "main :: IO () "
 , "main = do "
 , "  env <- primitiveBindings "
 , "  (runIOThrows $ liftM show $ run env (makeNullContinuation env) (Nil \"\") Nothing) >>= putStr "
 , " "
 , "run :: Env -> LispVal -> LispVal -> Maybe [LispVal] -> IOThrowsError LispVal "
 , "run env cont _ _ = do "]

compileLisp :: Env -> String -> IOThrowsError LispVal
compileLisp env filename = do
  -- TODO: below does not really work when compiling an expression that evaluates to a value (eg: 1)
  comp <- load filename >>= compileBlock env [] --mapM (compile env)
  outH <- liftIO $ openFile "_tmp.hs" WriteMode
  _ <- liftIO $ writeList outH header
  _ <- liftIO $ writeList outH comp
  _ <- liftIO $ hClose outH
  return $ Nil "" -- Dummy value
{-  if not (null comp)
     then do
     else putStrLn "empty file"
-}

writeList outH (l : ls) = do
  hPutStrLn outH l
  writeList outH ls
writeList outH _ = do
  hPutStr outH ""

compileBlock :: Env -> [String] -> [LispVal] -> IOThrowsError [String]
compileBlock env result code@(c:cs) = do

-- TODO: may need to pass a 'level' variable for indentation

  compiled <- compile env c
  compileBlock env (result ++ [compiled]) cs
compileBlock env result [] = return result
{-
compileBlock - need to use explicit recursion to transform a block of code, because
 later lines may depend on previous ones (is this true?)

bs
 - compile the first expression
 - need to save result to gensym'd var if not last line
 - need to lift up the result if not pure
-}

compile :: Env -> LispVal -> IOThrowsError String 
compile _ (Number n) = return $ "return $ Number " ++ (show n)
compile _ (Atom a) = return $ "getVar env \"" ++ a ++ "\"" --"Atom " ++ a

-- TODO: this is not good enough; a line of scheme may need to be compiled into many lines of haskell,
--  for example
-- _gensym :: String -> iothrowserror lispval
compile env args@(List (func : params)) = do
  f <- compile env func
  f <- return $ "  x1 <- " ++ f
  Atom nextFunc <- _gensym "f"
  c <- return $ "\n  continueEval env (makeCPSWArgs env cont " ++ nextFunc ++ " [x1]) $ Nil \"\""
  rest <- compileArgs nextFunc params
  return $ f ++ c ++ rest
 where 
  compileArgs :: String -> [LispVal] -> IOThrowsError String
  compileArgs thisFunc args = do
    case args of
      [] -> do
        fdef <- return $ "\n" ++ thisFunc ++ " env cont value (Just (a:as)) = do " 
        apl <- return $ "\n  apply cont a as "
        return $ fdef ++ apl
      [a] -> do
        fdef <- return $ "\n" ++ thisFunc ++ " env cont value (Just args) = do " 
        f <- compile env a
        f <- return $ "\n  x1 <- " ++ f
        Atom nextFunc <- _gensym "f"
        c <- return $ "\n  continueEval env (makeCPSWArgs env cont " ++ nextFunc ++ " $ args ++ [x1]) $ Nil \"\""
        rest <- compileArgs nextFunc [] 
        return $ fdef ++ f ++ c ++ rest
      (a : as) -> do
        fdef <- return $ "\n" ++ thisFunc ++ " env cont value (Just args) = do " 
        f <- compile env a
        f <- return $ "\n  x1 <- " ++ f
        Atom nextFunc <- _gensym "f"
        c <- return $ "\n  continueEval env (makeCPSWArgs env cont " ++ nextFunc ++ " $ args ++ [x1]) $ Nil \"\""
        rest <- compileArgs nextFunc as 
        return $ fdef ++ f ++ c ++ rest

  -- TODO: continueEval
{- TODO:
  case lookup func compiledPrimitives of
    (Just a) -> do
      ps <- mapM (compile env) params
      return $ a ++ " [ " ++ (Data.List.intercalate ", " ps) ++ " ] "
--      return $ a ++ " [ " ++ (unwords ps) ++ " ] "
    Nothing -> throwError $ Default $ "Function definition not found: " ++ func 
-}

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
