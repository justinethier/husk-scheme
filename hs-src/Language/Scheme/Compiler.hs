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

-- TODO: this is too limiting, this is an 'internal' continuation. most should take a value and pass it along, not args
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
compileBlock env result code@[c] = do
  compiled <- compile env c Nothing
  return $ result ++ compiled
compileBlock env result code@(c:cs) = do
  Atom nextFunc <- _gensym "f"
  compiled <- compile env c (Just nextFunc)
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

-- TODO: could everything just be regular function calls except when a continuation is 'added to the stack' via a makeCPS(makeCPSWArgs ...) ?? I think this could be made more efficient

compile :: Env -> LispVal -> Maybe String -> IOThrowsError [HaskAST]
compile _ (Bool b) _ = return [AstValue $ "  return $ Bool " ++ (show b)]
compile _ (Number n) _ = return [AstValue $ "  return $ Number " ++ (show n)]
compile _ (Atom a) _ = return [AstValue $ "  getVar env \"" ++ a ++ "\""] --"Atom " ++ a
--compile env (List [Atom "quote", val]) = return [AstValue $ "  continueEval env cont -- TODO: how to get the literal val?

-- TODO: this does not work, and is a big mess to boot...
compile env args@(List [Atom "if", predic, conseq, alt]) fForNextExpression = do
 Atom symPredicate <- _gensym "ifPredic"
 Atom symCheckPredicate <- _gensym "compiledIfPredicate"
 Atom symConsequence <- _gensym "compiledConsequence"
 Atom symAlternate <- _gensym "compiledAlternative"
-- Atom compiledFunc <- _gensym "ifComp"
 f <- return $ [AstValue $ "  bound <- liftIO $ isRecBound env \"if\"",
       AstValue $ "  if bound ",
       AstValue $ "     then throwError $ NotImplemented \"prepareApply env cont args\" ", -- if is bound to a variable in this scope; call into it
       AstValue $ "     else do " ++ symPredicate ++ " env (makeCPS env cont " ++ symCheckPredicate ++ ") (Nil \"\") [] "
       ]
 predicCompiled <- compile env predic fForNextExpression


-- !!!!!!!!!!!!!!!!!!!
-- TODO: this seems like a common pattern, and is used in conseq/alt below. 
--   we should extract it into a function

 compPredicate <- case predicCompiled of
    [comp] -> return $ AstFunction symPredicate " env cont _ _ " 
                         [AstAssignM "x1" $ comp,
                          AstValue $ "  continueEval env cont x1 "]
    comp@(_ : _) -> return $ AstFunction symPredicate " env cont _ _ " comp

 compCheckPredicate <- return $ AstFunction symCheckPredicate " env cont result _ " [
    AstValue $ "  case result of ",
    AstValue $ "    Bool False -> " ++ symAlternate ++ " env cont (Nil \"\") [] ",
    AstValue $ "    _ -> " ++ symConsequence ++ " env cont (Nil \"\") [] "]
 
 conseqCompiled <- compile env conseq fForNextExpression
 compConsequence <- case conseqCompiled of
   [comp] -> return $ AstFunction symConsequence " env cont _ _ " 
                         [AstAssignM "x1" $ comp,
                          AstValue $ "  continueEval env cont x1 "]
   _ -> return $ AstFunction symConsequence " env cont _ _ " conseqCompiled

 altCompiled <- compile env alt fForNextExpression
 compAlternate <- case altCompiled of
   [comp] -> return $ AstFunction symAlternate " env cont _ _ " 
                         [AstAssignM "x1" $ comp,
                          AstValue $ "  continueEval env cont x1 "]
   _ -> return $ AstFunction symAlternate " env cont _ _ " altCompiled

 return $ f ++ [compPredicate, compCheckPredicate, compConsequence, compAlternate] 
 

compile env args@(List (_ : _)) fForNextExpression = compileApply env args fForNextExpression

compileApply :: Env -> LispVal -> Maybe String -> IOThrowsError [HaskAST]
compileApply env args@(List (func : params)) fForNextExpression = do
  _comp <- compile env func Nothing
  
--  case (trace ("_comp = " ++ show _comp) _comp) of
  case _comp of
    [comp] -> do
      f <- return $ AstAssignM "x1" comp
      Atom nextFunc <- _gensym "f"
      c <- return $ AstContinuation nextFunc "[x1]"
      rest <- compileArgs nextFunc False params
      return $ [f, c] ++ rest -- TODO: a hack
{-
 - TODO: if there is an unevaluated function instead of a function instance,
 -      then we need to compile that function first and proceed with its value
 -
 - code@(_ : _) -> do
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
  compileArgs thisFunc thisFuncUseValue args = do
    case args of
      [] -> do
           -- The basic idea is that if there is a next expression, call into it as a new continuation
           -- instead of calling into cont
           case fForNextExpression of
             Nothing -> return $ [
               AstFunction thisFunc 
                " env cont (Nil _) (Just (a:as)) " [AstValue "  apply cont a as "],
               AstFunction thisFunc 
                " env cont value (Just (a:as)) " [AstValue "  apply cont a $ as ++ [value] "]]
             Just fnextExpr -> return $ [
               AstFunction thisFunc 
                " env cont (Nil _) (Just (a:as)) " [AstValue $ "  apply (makeCPS env cont " ++ fnextExpr ++ ") a as "],
               AstFunction thisFunc 
                " env cont value (Just (a:as)) " [AstValue $ "  apply (makeCPS env cont " ++ fnextExpr ++ ") a $ as ++ [value] "],
               AstFunction fnextExpr " env cont _ _ " []]
      (a:as) -> do
        _comp <- compile env a Nothing
        -- Use this below to splice in a call to another function      
--        case (trace ("_comp = " ++ show _comp) _comp) of
        case _comp of
          [comp] -> do
            let nfArgs = if thisFuncUseValue
                            then "(args ++ [value] ++ [x1])"
                            else "(args ++ [x1])"
            Atom nextFunc <- _gensym "f"
            rest <- compileArgs nextFunc False as 
            return $ [AstFunction thisFunc " env cont value (Just args) " 
                                  [AstAssignM "x1" $ comp,
                                   AstContinuation nextFunc nfArgs]
                     ] ++ rest
          code@(_ : _) -> do
            -- If another func is being called, we detect and splice it in...
            Atom stubFunc <- _gensym "f"
            Atom nextFunc <- _gensym "f"

            -- Flag below means that the expression's value matters, add it to args
            f <- if thisFuncUseValue
                    then return $ AstValue $ thisFunc ++ " env cont value (Just args) = do "
                    else return $ AstValue $ thisFunc ++ " env cont _ (Just args) = do "
            c <- if thisFuncUseValue
                    then return $ AstValue $ "  continueEval env (makeCPS env (makeCPSWArgs env cont " ++ nextFunc ++ " $ args ++ [value]) " ++ stubFunc ++ ") $ Nil\"\""  
                    else return $ AstValue $ "  continueEval env (makeCPS env (makeCPSWArgs env cont " ++ nextFunc ++ " args) " ++ stubFunc ++ ") $ Nil\"\""  

            -- True indicates nextFunc needs to use value arg passed into it
            rest <- compileArgs nextFunc True as
            return $ [ f, c, 
                       AstFunction stubFunc " env cont _ _ " []
                     ] ++ code ++ rest

