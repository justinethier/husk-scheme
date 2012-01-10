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
import Debug.Trace

-- A very basic type to store a Haskell AST
-- The compiler performs the following transformations:
-- Scheme AST (LispVal) -> Haskell AST (HaskAST) -> Compiled Code (String)
data HaskAST = AstAssignM String HaskAST
  | AstFunction {astfName :: String,
--                 astfType :: String,
                 astfArgs :: String,
                 astfCode :: [HaskAST]
                } 
 | AstValue String
 | AstContinuation {astcNext :: String,
                    astcArgs :: String
                   }
-- | AstNewline String -- Is this a hack?

showValAST :: HaskAST -> String
showValAST (AstAssignM var val) = "  " ++ var ++ " <- " ++ show val
showValAST (AstFunction name args code) = do
  let header = "\n" ++ name ++ args ++ " = do "
  let body = unwords . map (\x -> "\n" ++ x ) $ map showValAST code
  header ++ body 
showValAST (AstValue v) = v
showValAST (AstContinuation nextFunc args) = "  continueEval env (makeCPSWArgs env cont " ++ nextFunc ++ " " ++ args ++ ") $ Nil \"\""

instance Show HaskAST where show = showValAST

-- OBSOLETE CODE:
---- TODO: will probably need to differentiate functions that are in the IO monad 
----  and pure ones that are not (such as numAdd)
--compiledPrimitives :: [(String, String)]
--compiledPrimitives = [
--  ("write", "writeProc (\\ port obj -> hPrint port obj)")
-- ,("+", "numAdd")
-- ,("-", "numSub")
-- ,("*", "numMul")
-- ,("/", "numDiv")]

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
  _ <- liftIO $ writeList outH $ map show comp
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

compileBlock :: Env -> [HaskAST] -> [LispVal] -> IOThrowsError [HaskAST]
compileBlock env result code@(c:cs) = do

-- TODO: may need to pass a 'level' variable for indentation

  compiled <- compile env c
  compileBlock env (result ++ compiled) cs
compileBlock env result [] = return result
{-
compileBlock - need to use explicit recursion to transform a block of code, because
 later lines may depend on previous ones (is this true?)

bs
 - compile the first expression
 - need to save result to gensym'd var if not last line
 - need to lift up the result if not pure
-}

{-
findNextContinuation :: [HaskAST] -> Maybe HaskAST
findNextContinuation (h@(AstContinuation _ _) : hs) = Just h
findNextContinuation (_ : hs) = findNextContinuation hs
findNextContinuation _ = Nothing
  -}

compile :: Env -> LispVal -> IOThrowsError [HaskAST]
compile _ (Number n) = return [AstValue $ "  return $ Number " ++ (show n)]
compile _ (Atom a) = return [AstValue $ "  getVar env \"" ++ a ++ "\""] --"Atom " ++ a

-- TODO: this is not good enough; a line of scheme may need to be compiled into many lines of haskell,
--  for example
-- _gensym :: String -> iothrowserror lispval
compile env args@(List (func : params)) = do
  _comp <- compile env func
  
--  case (trace ("_comp = " ++ show _comp) _comp) of
  case _comp of
    [comp] -> do
      f <- return $ AstAssignM "x1" comp
      Atom nextFunc <- _gensym "f"
      c <- return $ AstContinuation nextFunc "[x1]"
      rest <- compileArgs nextFunc False params
      return $ [f, c] ++ rest -- TODO: a hack
{-    code@(_ : _) -> do
-- TODO: search code for the continuation    AstFunction name args code -> do
--      Just (AstContinuation cNextFunc _) <- findNextContinuation code
      Atom stubFunc <- _gensym "f"
      Atom nextFunc <- _gensym "f"
-- f1 - would be next func that comp will call into
-- f5 - would be nextFunc
--  continueEval env (makeCPS env (makeCPSWArgs env cont f5 args) f1) $ Nil ""
      c <- return $ AstValue $ "  continueEval env (makeCPS env cont " ++ nextFunc ++ " args) " ++ stubFunc ++ ") $ Nil\"\""  
      f <- return $ AstValue $ stubFunc ++ " env cont _ _ = do "
      rest <- compileArgs nextFunc params
      return (c : rest)
  -}    
 where 
  compileArgs :: String -> Bool -> [LispVal] -> IOThrowsError [HaskAST]
  compileArgs thisFunc applyUseValue args = do
    case args of
      [] -> do
        if applyUseValue
           then return $ [AstFunction thisFunc " env cont value (Just (a:as)) " [AstValue "  apply cont a $ as ++ [value] "]]
           else return $ [AstFunction thisFunc " env cont value (Just (a:as)) " [AstValue "  apply cont a as "]]
      [a] -> do
        _comp <- compile env a
        -- Use this below to splice in a call to another function      
        case (trace ("_comp = " ++ show _comp) _comp) of
          [comp] -> do
            Atom nextFunc <- _gensym "f"
            rest <- compileArgs nextFunc False [] 
            return $ [AstFunction thisFunc " env cont value (Just args) " 
                                  [AstAssignM "x1" $ comp,
                                   AstContinuation nextFunc "(args ++ [x1])"]
                     ] ++ rest
          code@(_ : _) -> do
            -- If another func is being called, we detect and splice it in...
            Atom stubFunc <- _gensym "f"
            Atom nextFunc <- _gensym "f"
            c <- return $ AstValue $ "  continueEval env (makeCPS env (makeCPSWArgs env cont " ++ nextFunc ++ " args) " ++ stubFunc ++ ") $ Nil\"\""  
            f <- return $ AstValue $ thisFunc ++ " env cont _ (Just args) = do "

            rest <- compileArgs nextFunc True [] 
            return $ [ f, c, 
                       AstFunction stubFunc " env cont _ _ " []
                     ] ++ code ++ rest

            
      (a : as) -> do
        _comp <- compile env a
        -- TODO: use this below to splice in a call to another function      
        case (trace ("_comp = " ++ show _comp) _comp) of
          comp -> do --[comp] -> do
            Atom nextFunc <- _gensym "f"
            rest <- compileArgs nextFunc False as 
            return $ [AstFunction thisFunc " env cont value (Just args) " 
                                  [AstAssignM "x1" $ head comp, -- TODO: a hack
                                   AstContinuation nextFunc "(args ++ [x1])"]
                     ] ++ rest
