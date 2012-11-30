{- |
Module      : Language.Scheme.Compiler
Copyright   : Justin Ethier
Licence     : MIT (see LICENSE in the distribution)

Maintainer  : github.com/justinethier
Stability   : experimental
Portability : portable

This module contains an experimental Scheme to Haskell compiler. 

The compiler performs the following transformations:

> Scheme AST (LispVal) => Haskell AST (HaskAST) => Compiled Code (String)

The GHC compiler is then used to create a native executable.

At present, the focus has just been on creating a compiler that will
generate correct, working code. Many optimizations could and need to
be made for time and space...

-}

module Language.Scheme.Compiler where 
import qualified Language.Scheme.Core (apply)
import qualified Language.Scheme.Macro
import Language.Scheme.Primitives
import Language.Scheme.Types
import Language.Scheme.Variables
import Control.Monad.Error
import qualified Data.Array
import Data.Complex
import qualified Data.List
import qualified Data.Map
import Data.Ratio
--import Debug.Trace

-- A type to store options passed to compile
-- eventually all of this might be able to be integrated into a Compile monad
data CompOpts = CompileOptions {
    coptsThisFunc :: String,
    coptsThisFuncUseValue :: Bool,
    coptsThisFuncUseArgs :: Bool,
    coptsNextFunc :: Maybe String
    }
--DefaultCompileOptions :: String -> CompileOpts 
defaultCompileOptions :: String -> CompOpts
defaultCompileOptions thisFunc = CompileOptions thisFunc False False Nothing

createAstFunc :: CompOpts -> [HaskAST] -> HaskAST 
createAstFunc (CompileOptions thisFunc useVal useArgs _) funcBody = do
  let val = case useVal of
              True -> "value"
              _ -> "_"
      args = case useArgs of
               True -> "(Just args)"
               _ -> "_"
  AstFunction thisFunc (" env cont " ++ val ++ " " ++ args ++ " ") funcBody

createAstCont :: CompOpts -> String -> String -> HaskAST
createAstCont (CompileOptions _ _ _ (Just nextFunc)) var indentation = do
  AstValue $ indentation ++ "  continueEval env (makeCPS env cont " ++ nextFunc ++ ") " ++ var
createAstCont (CompileOptions _ _ _ Nothing) var indentation = do
  AstValue $ indentation ++ "  continueEval env cont " ++ var

-- A very basic type to store a Haskell AST
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

showValAST :: HaskAST -> String
showValAST (AstAssignM var val) = "  " ++ var ++ " <- " ++ show val
showValAST (AstFunction name args code) = do
  let fheader = "\n" ++ name ++ args ++ " = do "
  let fbody = unwords . map (\x -> "\n" ++ x ) $ map showValAST code
  fheader ++ fbody 
showValAST (AstValue v) = v

-- TODO: this is too limiting, this is an 'internal' continuation. most should take a value and pass it along, not args
showValAST (AstContinuation nextFunc args) = "  continueEval env (makeCPSWArgs env cont " ++ nextFunc ++ " " ++ args ++ ") $ Nil \"\""

instance Show HaskAST where show = showValAST

joinL :: forall a. [[a]] -> [a] -> [a]
joinL ls sep = concat $ Data.List.intersperse sep ls

astToHaskellStr :: LispVal -> String 
astToHaskellStr (String s) = "String " ++ show s
astToHaskellStr (Char c) = "Char " ++ show c
astToHaskellStr (Atom a) = "Atom " ++ show a
astToHaskellStr (Number n) = "Number (" ++ show n ++ ")"
astToHaskellStr (Complex c) = "Complex $ (" ++ (show $ realPart c) ++ ") :+ (" ++ (show $ imagPart c) ++ ")"
astToHaskellStr (Rational r) = "Rational $ (" ++ (show $ numerator r) ++ ") % (" ++ (show $ denominator r) ++ ")"
astToHaskellStr (Float f) = "Float (" ++ show f ++ ")"
astToHaskellStr (Bool True) = "Bool True"
astToHaskellStr (Bool False) = "Bool False"
astToHaskellStr (HashTable ht) = do
 let ls = Data.Map.toList ht 
     conv (a, b) = "(" ++ astToHaskellStr a ++ "," ++ astToHaskellStr b ++ ")"
 "HashTable $ Data.Map.fromList $ [" ++ joinL (map conv ls) "," ++ "]"
astToHaskellStr (Vector v) = do
  let ls = Data.Array.elems v
      size = (length ls) - 1
  "Vector (listArray (0, " ++ show size ++ ")" ++ "[" ++ joinL (map astToHaskellStr ls) "," ++ "])"
astToHaskellStr (List ls) = "List [" ++ joinL (map astToHaskellStr ls) "," ++ "]"
astToHaskellStr (DottedList ls l) = 
  "DottedList [" ++ joinL (map astToHaskellStr ls) "," ++ "] $ " ++ astToHaskellStr l

header, headerModule, headerImports :: [String]
headerModule = ["module Main where "]
headerImports = [
   "Language.Scheme.Core "
 , "Language.Scheme.Numerical "
 , "Language.Scheme.Primitives "
 , "Language.Scheme.Types     -- Scheme data types "
 , "Language.Scheme.Variables -- Scheme variable operations "
 , "Control.Monad.Error "
 , "Data.Array "
 , "Data.Complex "
 , " qualified Data.Map "
 , "Data.Ratio "
 , "System.IO "]
header = [
   " "
 , "main :: IO () "
 , "main = do "
 , "  env <- primitiveBindings "
 , "  result <- (runIOThrows $ liftM show $ run env (makeNullContinuation env) (Nil \"\") Nothing) "
 , "  case result of "
 , "    Just errMsg -> putStrLn errMsg "
 , "    _ -> return () "
 , " "]

-- NOTE: the following type is used for all functions generated by the compiler: 
-- , "run :: Env -> LispVal -> LispVal -> Maybe [LispVal] -> IOThrowsError LispVal "


-- Define imports var here as an empty list.
-- This list is appended to by (load-ffi) instances,
-- and the imports are explicitly added later on...
initializeCompiler :: Env -> IOThrowsError [HaskAST]
initializeCompiler env = do
  _ <- defineNamespacedVar env "internal" "imports" $ List []
  return []


compileLisp :: Env -> String -> String -> Maybe String -> IOThrowsError [HaskAST]
compileLisp env filename entryPoint exitPoint = load filename >>= compileBlock entryPoint exitPoint env []
-- compileBlock
--
-- Note: Uses explicit recursion to transform a block of code, because
--  later lines may depend on previous ones
compileBlock :: String -> Maybe String -> Env -> [HaskAST] -> [LispVal] -> IOThrowsError [HaskAST]
compileBlock symThisFunc symLastFunc env result [c] = do
  compiled <- mcompile env c $ CompileOptions symThisFunc False False symLastFunc 
  return $ result ++ compiled
compileBlock symThisFunc symLastFunc env result (c:cs) = do
  Atom symNextFunc <- _gensym "f"
  compiled <- mcompile env c $ CompileOptions symThisFunc False False (Just symNextFunc)
  compileBlock symNextFunc symLastFunc env (result ++ compiled) cs
compileBlock _ _ _ result [] = return result

-- TODO: could everything just be regular function calls except when a continuation is 'added to the stack' via a makeCPS(makeCPSWArgs ...) ?? I think this could be made more efficient

-- Helper function to compile expressions consisting of a scalar
compileScalar :: String -> CompOpts -> IOThrowsError [HaskAST]
compileScalar val copts = do 
  f <- return $ AstAssignM "x1" $ AstValue val 
  c <- return $ createAstCont copts "x1" ""
  return [createAstFunc copts [f, c]]

compileLambdaList :: [LispVal] -> IOThrowsError String
compileLambdaList l = do
  serialized <- mapM serialize l 
  return $ "[" ++ concat (Data.List.intersperse "," serialized) ++ "]"
 where serialize (Atom a) = return $ (show a)
       serialize a = throwError $ Default $ "invalid parameter to lambda list: " ++ show a

compile :: Env -> LispVal -> CompOpts -> IOThrowsError [HaskAST]
compile _ (Nil n) copts = compileScalar ("  return $ Nil " ++ (show n)) copts
compile _ (String s) copts = compileScalar ("  return $ String " ++ (show s)) copts
compile _ (Char c) copts = compileScalar ("  return $ Char " ++ (show c)) copts
compile _ (Complex c) copts = compileScalar ("  return $ Complex $ (" ++ (show $ realPart c) ++ ") :+ (" ++ (show $ imagPart c) ++ ")") copts
compile _ (Float f) copts = compileScalar ("  return $ Float (" ++ (show f) ++ ")") copts
compile _ (Rational r) copts = compileScalar ("  return $ Rational $ (" ++ (show $ numerator r) ++ ") % (" ++ (show $ denominator r) ++ ")") copts 
compile _ (Number n) copts = compileScalar ("  return $ Number (" ++ (show n) ++ ")") copts
compile _ (Bool b) copts = compileScalar ("  return $ Bool " ++ (show b)) copts
-- TODO: eval env cont val@(HashTable _) = continueEval env cont val
compile _ v@(Vector _) copts = compileScalar (" return $ " ++ astToHaskellStr v) copts
compile _ ht@(HashTable _) copts = compileScalar (" return $ " ++ astToHaskellStr ht) copts
compile _ (Atom a) copts = do
 c <- return $ createAstCont copts "val" ""
 return [createAstFunc copts [
   AstValue $ "  v <- getVar env \"" ++ a ++ "\"",
   AstValue $ "  val <- return $ case v of",
   AstValue $ "    List _ -> Pointer \"" ++ a ++ "\" env",
   AstValue $ "    DottedList _ _ -> Pointer \"" ++ a ++ "\" env",
   AstValue $ "    String _ -> Pointer \"" ++ a ++ "\" env",
   AstValue $ "    Vector _ -> Pointer \"" ++ a ++ "\" env",
   AstValue $ "    HashTable _ -> Pointer \"" ++ a ++ "\" env",
   AstValue $ "    _ -> v"], c]

compile _ (List [Atom "quote", val]) copts = compileScalar (" return $ " ++ astToHaskellStr val) copts

compile env (List [Atom "expand",  _body]) copts = do
  -- TODO: check if expand has been rebound?
  val <- Language.Scheme.Macro.expand env False _body Language.Scheme.Core.apply
  compileScalar (" return $ " ++ astToHaskellStr val) copts

compile env (List (Atom "let-syntax" : List _bindings : _body)) copts = do
  -- TODO: check if let-syntax has been rebound?
  bodyEnv <- liftIO $ extendEnv env []
  _ <- Language.Scheme.Macro.loadMacros env bodyEnv Nothing False _bindings
  -- Expand whole body as a single continuous macro, to ensure hygiene
  expanded <- Language.Scheme.Macro.expand bodyEnv False (List _body) Language.Scheme.Core.apply
  divertVars bodyEnv expanded copts func
 where 
   func bodyEnv' expanded' copts' = do
     case expanded' of
       List e -> compile bodyEnv' (List $ Atom "begin" : e) copts'
       e -> compile bodyEnv' e copts'

compile env (List (Atom "letrec-syntax" : List _bindings : _body)) copts = do
  -- TODO: check if let-syntax has been rebound?
  bodyEnv <- liftIO $ extendEnv env []
  _ <- Language.Scheme.Macro.loadMacros bodyEnv bodyEnv Nothing False _bindings
  -- Expand whole body as a single continuous macro, to ensure hygiene
  expanded <- Language.Scheme.Macro.expand bodyEnv False (List _body) Language.Scheme.Core.apply
  divertVars bodyEnv expanded copts func
 where 
   func bodyEnv' expanded' copts' = do
     case expanded' of
       List e -> compile bodyEnv' (List $ Atom "begin" : e) copts'
       e -> compile bodyEnv' e copts'

compile env (List [Atom "define-syntax", Atom keyword,
  (List [Atom "er-macro-transformer", 
    (List (Atom "lambda" : List fparams : fbody))])])
  copts = do

  -- TODO: same as below, these defs will eventually need to 
  -- be made available during runtime as well as compile time
  f <- makeNormalFunc env fparams fbody 
  _ <- defineNamespacedVar env macroNamespace keyword $ SyntaxExplicitRenaming f
  compileScalar ("  return $ Nil \"\"") copts 

compile env (List [Atom "define-syntax", Atom keyword, (List (Atom "syntax-rules" : (List identifiers : rules)))]) copts = do
--
-- TODO:
--
-- macros will eventually need to introduce a definition in both the compiler's env (so macros can be processed at compile time) and in the program's env (so dynamically injected code has access to the macro). That said, the priority is compile-time processing.
--
-- TBD: how the heck to serialize a Syntax object for use in the compiled code, since
--    Syntax has embedded env's... perhaps the first step is to do it with a null env
--
  _ <- defineNamespacedVar env macroNamespace keyword $ Syntax (Just env) Nothing False identifiers rules
  compileScalar ("  return $ Nil \"\"") copts 

compile env (List [Atom "if", predic, conseq]) copts = 
 compile env (List [Atom "if", predic, conseq, Nil ""]) copts

compile env (List [Atom "if", predic, conseq, alt]) copts@(CompileOptions _ _ _ nextFunc) = do
 -- FUTURE: think about it, these could probably be part of compileExpr
 Atom symPredicate <- _gensym "ifPredic"
 Atom symCheckPredicate <- _gensym "compiledIfPredicate"
 Atom symConsequence <- _gensym "compiledConsequence"
 Atom symAlternate <- _gensym "compiledAlternative"
 -- Entry point; ensure if is not rebound
 f <- return $ [AstValue $ "  bound <- liftIO $ isRecBound env \"if\"",
       AstValue $ "  if bound ",
       AstValue $ "     then throwError $ NotImplemented \"prepareApply env cont args\" ", -- if is bound to a variable in this scope; call into it
       AstValue $ "     else do " ++ symPredicate ++ " env (makeCPS env cont " ++ symCheckPredicate ++ ") (Nil \"\") [] "
       ]
 -- Compile expression for if's args
 compPredicate <- compileExpr env predic symPredicate Nothing      -- Do not want to call into nextFunc in the middle of (if)
 compConsequence <- compileExpr env conseq symConsequence nextFunc -- pick up at nextFunc after consequence
 compAlternate <- compileExpr env alt symAlternate nextFunc        -- or...pick up at nextFunc after alternate
 -- Special case because we need to check the predicate's value
 compCheckPredicate <- return $ AstFunction symCheckPredicate " env cont result _ " [
    AstValue $ "  case result of ",
    AstValue $ "    Bool False -> " ++ symAlternate ++ " env cont (Nil \"\") [] ",
    AstValue $ "    _ -> " ++ symConsequence ++ " env cont (Nil \"\") [] "]
 -- Join compiled code together
 return $ [createAstFunc copts f] ++ compPredicate ++ [compCheckPredicate] ++ compConsequence ++ compAlternate

compile env (List [Atom "set!", Atom var, form]) copts@(CompileOptions _ _ _ _) = do
 Atom symDefine <- _gensym "setFunc"
 Atom symMakeDefine <- _gensym "setFuncMakeSet"

 -- Store var in huskc's env for macro processing
 --
 -- !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 -- TODO: this is going to cause problems for er macros
 --
 -- TODO: changed this to a 'defineVar' for now, because without lambda forms inserting
 --       defined variables, using setVar will cause an error when trying to set a
 --       lambda var...
 _ <- defineVar env var form -- TODO: setVar (per above comment)

 entryPt <- compileSpecialFormEntryPoint "set!" symDefine copts
 compDefine <- compileExpr env form symDefine $ Just symMakeDefine
 compMakeDefine <- return $ AstFunction symMakeDefine " env cont result _ " [
    AstValue $ "  _ <- setVar env \"" ++ var ++ "\" result",
    createAstCont copts "result" ""]
 return $ [entryPt] ++ compDefine ++ [compMakeDefine]

compile _ (List [Atom "set!", nonvar, _]) copts = do 
 f <- compileSpecialForm "set!" ("throwError $ TypeMismatch \"variable\" $ String \"" ++ (show nonvar) ++ "\"")  copts
 return [f]
compile _ (List (Atom "set!" : args)) copts = do
 f <- compileSpecialForm "set!" ("throwError $ NumArgs 2 $ [String \"" ++ (show args) ++ "\"]") copts -- TODO: Cheesy to use a string, but fine for now...
 return [f]

compile env (List [Atom "define", Atom var, form]) copts@(CompileOptions _ _ _ _) = do
 Atom symDefine <- _gensym "defineFuncDefine"
 Atom symMakeDefine <- _gensym "defineFuncMakeDef"

 -- Store var in huskc's env for macro processing (and same for other vers of define)
 _ <- defineVar env var form

 -- Entry point; ensure var is not rebound
 f <- return $ [AstValue $ "  bound <- liftIO $ isRecBound env \"define\"",
       AstValue $ "  if bound ",
       AstValue $ "     then throwError $ NotImplemented \"prepareApply env cont args\" ", -- if is bound to a variable in this scope; call into it
       AstValue $ "     else do " ++ symDefine ++ " env cont (Nil \"\") []" ]
 compDefine <- compileExpr env form symDefine $ Just symMakeDefine
 compMakeDefine <- return $ AstFunction symMakeDefine " env cont result _ " [
    AstValue $ "  _ <- defineVar env \"" ++ var ++ "\" result",
    createAstCont copts "result" ""]
 return $ [createAstFunc copts f] ++ compDefine ++ [compMakeDefine]

compile env (List (Atom "define" : List (Atom var : fparams) : fbody)) copts@(CompileOptions _ _ _ _) = do
 Atom symCallfunc <- _gensym "defineFuncEntryPt"
 compiledParams <- compileLambdaList fparams
 compiledBody <- compileBlock symCallfunc Nothing env [] fbody

 -- Store var in huskc's env for macro processing (and same for other vers of define)
 _ <- makeNormalFunc env fparams fbody >>= defineVar env var

 -- Entry point; ensure var is not rebound
 f <- return $ [AstValue $ "  bound <- liftIO $ isRecBound env \"define\"",
       AstValue $ "  if bound ",
       AstValue $ "     then throwError $ NotImplemented \"prepareApply env cont args\" ", -- if is bound to a variable in this scope; call into it
       AstValue $ "     else do result <- makeNormalHFunc env (" ++ compiledParams ++ ") " ++ symCallfunc,
       AstValue $ "             _ <- defineVar env \"" ++ var ++ "\" result ",
       createAstCont copts "result" "           "
       ]
 return $ [createAstFunc copts f] ++ compiledBody

compile env (List (Atom "define" : DottedList (Atom var : fparams) varargs : fbody)) copts@(CompileOptions _ _ _ _) = do
 Atom symCallfunc <- _gensym "defineFuncEntryPt"
 compiledParams <- compileLambdaList fparams
 compiledBody <- compileBlock symCallfunc Nothing env [] fbody

 -- Store var in huskc's env for macro processing (and same for other vers of define)
 _ <- makeVarargs varargs env fparams fbody >>= defineVar env var

 -- Entry point; ensure var is not rebound
 f <- return $ [AstValue $ "  bound <- liftIO $ isRecBound env \"define\"",
       AstValue $ "  if bound ",
       AstValue $ "     then throwError $ NotImplemented \"prepareApply env cont args\" ", -- if is bound to a variable in this scope; call into it
       AstValue $ "     else do result <- makeHVarargs (" ++ astToHaskellStr varargs ++ ") env (" ++ compiledParams ++ ") " ++ symCallfunc,
       AstValue $ "             _ <- defineVar env \"" ++ var ++ "\" result ",
       createAstCont copts "result" "           "
       ]
 return $ [createAstFunc copts f] ++ compiledBody



compile env (List (Atom "lambda" : List fparams : fbody)) copts@(CompileOptions _ _ _ _) = do
 Atom symCallfunc <- _gensym "lambdaFuncEntryPt"
 compiledParams <- compileLambdaList fparams

-- TODO: need to extend Env below when compiling body?
-- TODO: need to bind lambda params in the extended env, for purposes of macro processing

 compiledBody <- compileBlock symCallfunc Nothing env [] fbody

 -- Entry point; ensure var is not rebound
-- TODO: will probably end up creating a common function for this,
--       since it is almost the same as in "if"
 f <- return $ [AstValue $ "  bound <- liftIO $ isRecBound env \"lambda\"",
       AstValue $ "  if bound ",
       AstValue $ "     then throwError $ NotImplemented \"prepareApply env cont args\" ", -- if is bound to a variable in this scope; call into it
       AstValue $ "     else do result <- makeNormalHFunc env (" ++ compiledParams ++ ") " ++ symCallfunc,
       createAstCont copts "result" "           "
       ]
 return $ [createAstFunc copts f] ++ compiledBody

compile env (List (Atom "lambda" : DottedList fparams varargs : fbody)) copts@(CompileOptions _ _ _ _) = do
 Atom symCallfunc <- _gensym "lambdaFuncEntryPt"
 compiledParams <- compileLambdaList fparams

-- TODO: need to extend Env below when compiling body?
-- TODO: need to bind lambda params in the extended env, for purposes of macro processing

 compiledBody <- compileBlock symCallfunc Nothing env [] fbody

 -- Entry point; ensure var is not rebound
 f <- return $ [AstValue $ "  bound <- liftIO $ isRecBound env \"lambda\"",
       AstValue $ "  if bound ",
       AstValue $ "     then throwError $ NotImplemented \"prepareApply env cont args\" ", -- if is bound to a variable in this scope; call into it
       AstValue $ "     else do result <- makeHVarargs (" ++ astToHaskellStr varargs ++ ") env (" ++ compiledParams ++ ") " ++ symCallfunc,
       createAstCont copts "result" "           "
       ]
 return $ [createAstFunc copts f] ++ compiledBody

compile env (List (Atom "lambda" : varargs@(Atom _) : fbody)) copts@(CompileOptions _ _ _ _) = do
 Atom symCallfunc <- _gensym "lambdaFuncEntryPt"

-- TODO: need to extend Env below when compiling body?
-- TODO: need to bind lambda params in the extended env, for purposes of macro processing

 compiledBody <- compileBlock symCallfunc Nothing env [] fbody

 -- Entry point; ensure var is not rebound
 f <- return $ [AstValue $ "  bound <- liftIO $ isRecBound env \"lambda\"",
       AstValue $ "  if bound ",
       AstValue $ "     then throwError $ NotImplemented \"prepareApply env cont args\" ", -- if is bound to a variable in this scope; call into it
       AstValue $ "     else do result <- makeHVarargs (" ++ astToHaskellStr varargs ++ ") env [] " ++ symCallfunc,
       createAstCont copts "result" "           "
       ]
 return $ [createAstFunc copts f] ++ compiledBody
compile env (List [Atom "string-set!", Atom var, i, character]) copts = do
 Atom symDefine <- _gensym "stringSetFunc"
 Atom symMakeDefine <- _gensym "stringSetFuncMakeSet"

 entryPt <- compileSpecialFormEntryPoint "string-set!" symDefine copts
 compDefine <- compileExpr env i symDefine $ Just symMakeDefine
 compMakeDefine <- return $ AstFunction symMakeDefine " env cont idx _ " [
    AstValue $ "  tmp <- getVar env \"" ++ var ++ "\"",
    AstValue $ "  derefValue <- recDerefPtrs tmp",
    -- TODO: not entirely correct below; should compile the character argument rather
    --       than directly inserting it into the compiled code...
    AstValue $ "  result <- substr (derefValue, (" ++ astToHaskellStr(character) ++ "), idx)",
    AstValue $ "  _ <- updateObject env \"" ++ var ++ "\" result",
    createAstCont copts "result" ""]
 return $ [entryPt] ++ compDefine ++ [compMakeDefine]

-- TODO: eval env cont args@(List [Atom "string-set!" , nonvar , _ , _ ]) = do
-- TODO: eval env cont fargs@(List (Atom "string-set!" : args)) = do 

compile env (List [Atom "set-car!", Atom var, argObj]) copts = do
 Atom symGetVar <- _gensym "setCarGetVar"
 Atom symCompiledObj <- _gensym "setCarCompiledObj"
 Atom symObj <- _gensym "setCarObj"
 Atom symDoSet <- _gensym "setCarDoSet"

 -- Code to all into next continuation from copts, if one exists
 let finalContinuation = case copts of
       (CompileOptions _ _ _ (Just nextFunc)) -> "continueEval e (makeCPS e c " ++ nextFunc ++ ")\n"
       _ -> "continueEval e c\n"

 -- Entry point that allows set-car! to be redefined
 entryPt <- compileSpecialFormEntryPoint "set-car!" symGetVar copts

 -- Function to read existing var
 compGetVar <- return $ AstFunction symGetVar " env cont idx _ " [
    AstValue $ "  result <- getVar env \"" ++ var ++ "\"",
    AstValue $ "  derefValue <- recDerefPtrs result",
    AstValue $ "  " ++ symObj ++ " env cont derefValue Nothing "]

 -- Compiled version of argObj
 compiledObj <- compileExpr env argObj symCompiledObj Nothing 

 -- Function to check looked-up var and call into appropriate handlers; based on code from Core
 --
 -- This is so verbose because we need to have overloads of symObj to deal with many possible inputs.
 -- FUTURE: consider making these functions part of the runtime.
 compObj <- return $ AstValue $ "" ++
              symObj ++ " :: Env -> LispVal -> LispVal -> Maybe [LispVal] -> IOThrowsError LispVal\n" ++
              symObj ++ " _ _ obj@(List []) _ = throwError $ TypeMismatch \"pair\" obj\n" ++
-- TODO: below, we want to make sure obj is of the right type. if so, compile obj and call into the "set" 
--       function below to do the actual set-car
              symObj ++ " e c obj@(List (_ : _)) _ = " ++ symCompiledObj ++ " e (makeCPSWArgs e c " ++ symDoSet ++ " [obj]) (Nil \"\") Nothing\n" ++
              symObj ++ " e c obj@(DottedList _ _) _ = " ++ symCompiledObj ++ " e (makeCPSWArgs e c " ++ symDoSet ++ " [obj]) (Nil \"\") Nothing\n" ++
              symObj ++ " _ _ obj _ = throwError $ TypeMismatch \"pair\" obj\n"

 -- Function to do the actual (set!), based on code from Core
 --
 -- This is so verbose because we need to have overloads of symObj to deal with many possible inputs.
 -- FUTURE: consider making these functions part of the runtime.
 compDoSet <- return $ AstValue $ "" ++
              symDoSet ++ " :: Env -> LispVal -> LispVal -> Maybe [LispVal] -> IOThrowsError LispVal\n" ++
              symDoSet ++ " e c obj (Just [List (_ : ls)]) = updateObject e \"" ++ var ++ "\" (List (obj : ls)) >>= " ++ finalContinuation ++
              symDoSet ++ " e c obj (Just [DottedList (_ : ls) l]) = updateObject e \"" ++ var ++ "\" (DottedList (obj : ls) l) >>= " ++ finalContinuation ++
              symDoSet ++ " _ _ _ _ = throwError $ InternalError \"Unexpected argument to " ++ symDoSet ++ "\"\n"

 -- Return a list of all the compiled code
 return $ [entryPt, compGetVar, compObj, compDoSet] ++ compiledObj

-- TODO: eval env cont args@(List [Atom "set-car!" , nonvar , _ ]) = do
-- TODO: eval env cont fargs@(List (Atom "set-car!" : args)) = do

compile env (List [Atom "set-cdr!", Atom var, argObj]) copts = do
 Atom symGetVar <- _gensym "setCdrGetVar"
 Atom symCompiledObj <- _gensym "setCdrCompiledObj"
 Atom symObj <- _gensym "setCdrObj"
 Atom symDoSet <- _gensym "setCdrDoSet"

 -- Code to all into next continuation from copts, if one exists
 let finalContinuation = case copts of
       (CompileOptions _ _ _ (Just nextFunc)) -> "continueEval e (makeCPS e c " ++ nextFunc ++ ")\n"
       _ -> "continueEval e c\n"

 -- Entry point that allows set-car! to be redefined
 entryPt <- compileSpecialFormEntryPoint "set-car!" symGetVar copts

 -- Function to read existing var
 compGetVar <- return $ AstFunction symGetVar " env cont idx _ " [
    AstValue $ "  result <- getVar env \"" ++ var ++ "\"",
    AstValue $ "  derefValue <- recDerefPtrs result",
    AstValue $ "  " ++ symObj ++ " env cont derefValue Nothing "]

 -- Compiled version of argObj
 compiledObj <- compileExpr env argObj symCompiledObj Nothing 

 -- Function to check looked-up var and call into appropriate handlers; based on code from Core
 --
 -- This is so verbose because we need to have overloads of symObj to deal with many possible inputs.
 -- FUTURE: consider making these functions part of the runtime.
 compObj <- return $ AstValue $ "" ++
              symObj ++ " :: Env -> LispVal -> LispVal -> Maybe [LispVal] -> IOThrowsError LispVal\n" ++
              symObj ++ " _ _ obj@(List []) _ = throwError $ TypeMismatch \"pair\" obj\n" ++
-- TODO: below, we want to make sure obj is of the right type. if so, compile obj and call into the "set" 
--       function below to do the actual set-car
              symObj ++ " e c obj@(List (_ : _)) _ = " ++ symCompiledObj ++ " e (makeCPSWArgs e c " ++ symDoSet ++ " [obj]) (Nil \"\") Nothing\n" ++
              symObj ++ " e c obj@(DottedList _ _) _ = " ++ symCompiledObj ++ " e (makeCPSWArgs e c " ++ symDoSet ++ " [obj]) (Nil \"\") Nothing\n" ++
              symObj ++ " _ _ obj _ = throwError $ TypeMismatch \"pair\" obj\n"

 -- Function to do the actual (set!), based on code from Core
 --
 -- This is so verbose because we need to have overloads of symObj to deal with many possible inputs.
 -- FUTURE: consider making these functions part of the runtime.
 compDoSet <- return $ AstValue $ "" ++
              symDoSet ++ " :: Env -> LispVal -> LispVal -> Maybe [LispVal] -> IOThrowsError LispVal\n" ++
              symDoSet ++ " e c obj (Just [List (l : _)]) = do\n" ++
                          "   l' <- recDerefPtrs l\n" ++
                          "   obj' <- recDerefPtrs obj\n" ++
                          "   (liftThrows $ cons [l', obj']) >>= updateObject e \"" ++ var ++ "\" >>= " ++ finalContinuation ++
              symDoSet ++ " e c obj (Just [DottedList (l : _) _]) = do\n" ++
                          "   l' <- recDerefPtrs l\n" ++
                          "   obj' <- recDerefPtrs obj\n" ++
                          "   (liftThrows $ cons [l', obj']) >>= updateObject e \"" ++ var ++ "\" >>= " ++ finalContinuation ++
              symDoSet ++ " _ _ _ _ = throwError $ InternalError \"Unexpected argument to " ++ symDoSet ++ "\"\n"

 -- Return a list of all the compiled code
 return $ [entryPt, compGetVar, compObj, compDoSet] ++ compiledObj

-- TODO: eval env cont args@(List [Atom "set-cdr!" , nonvar , _ ]) = do
-- TODO: eval env cont fargs@(List (Atom "set-cdr!" : args)) = do
compile env (List [Atom "vector-set!", Atom var, i, object]) copts = do
 Atom symCompiledIdx <- _gensym "vectorSetIdx"
 Atom symCompiledObj <- _gensym "vectorSetObj"
 Atom symUpdateVec <- _gensym "vectorSetUpdate"
 Atom symIdxWrapper <- _gensym "vectorSetIdxWrapper"

 -- Entry point that allows this form to be redefined
 entryPt <- compileSpecialFormEntryPoint "vector-set!" symCompiledIdx copts
 -- Compile index, then use a wrapper to pass it as an arg while compiling obj
 compiledIdx <- compileExpr env i symCompiledIdx (Just symIdxWrapper) 
 compiledIdxWrapper <- return $ AstFunction symIdxWrapper " env cont idx _ " [
    AstValue $ "  " ++ symCompiledObj ++ " env (makeCPSWArgs env cont " ++ symUpdateVec ++ " [idx]) (Nil \"\") Nothing " ]
 compiledObj <- compileExpr env object symCompiledObj Nothing
 -- Do actual update
 compiledUpdate <- return $ AstFunction symUpdateVec " env cont obj (Just [idx]) " [
    AstValue $ "  vec <- getVar env \"" ++ var ++ "\"",
    AstValue $ "  result <- updateVector vec idx obj >>= updateObject env \"" ++ var ++ "\"",
    createAstCont copts "result" ""]

 return $ [entryPt, compiledIdxWrapper, compiledUpdate] ++ compiledIdx ++ compiledObj

-- TODO: eval env cont args@(List [Atom "vector-set!" , nonvar , _ , _]) = do 
-- TODO: eval env cont fargs@(List (Atom "vector-set!" : args)) = do 

compile env (List [Atom "hash-table-set!", Atom var, rkey, rvalue]) copts = do
 Atom symCompiledIdx <- _gensym "hashTableSetIdx"
 Atom symCompiledObj <- _gensym "hashTableSetObj"
 Atom symUpdateVec <- _gensym "hashTableSetUpdate"
 Atom symIdxWrapper <- _gensym "hashTableSetIdxWrapper"

 -- Entry point that allows this form to be redefined
 entryPt <- compileSpecialFormEntryPoint "hash-table-set!" symCompiledIdx copts
 -- Compile index, then use a wrapper to pass it as an arg while compiling obj
 compiledIdx <- compileExpr env rkey symCompiledIdx (Just symIdxWrapper) 
 compiledIdxWrapper <- return $ AstFunction symIdxWrapper " env cont idx _ " [
    AstValue $ "  " ++ symCompiledObj ++ " env (makeCPSWArgs env cont " ++ symUpdateVec ++ " [idx]) (Nil \"\") Nothing " ]
 compiledObj <- compileExpr env rvalue symCompiledObj Nothing
 -- Do actual update
 compiledUpdate <- return $ AstFunction symUpdateVec " env cont obj (Just [rkey]) " [
    -- TODO: this should be more robust, than just assuming ht is a HashTable
    AstValue $ "  HashTable ht <- getVar env \"" ++ var ++ "\"",
    AstValue $ "  HashTable ht' <- recDerefPtrs $ HashTable ht",
    AstValue $ "  result <- updateObject env \"" ++ var ++ "\" (HashTable $ Data.Map.insert rkey obj ht') ",
    createAstCont copts "result" ""]

 return $ [entryPt, compiledIdxWrapper, compiledUpdate] ++ compiledIdx ++ compiledObj
-- TODO: eval env cont args@(List [Atom "hash-table-set!" , nonvar , _ , _]) = do
-- TODO: eval env cont fargs@(List (Atom "hash-table-set!" : args)) = do

compile env (List [Atom "hash-table-delete!", Atom var, rkey]) copts = do
 Atom symCompiledIdx <- _gensym "hashTableDeleteIdx"
 Atom symDoDelete <- _gensym "hashTableDelete"

 -- Entry point that allows this form to be redefined
 entryPt <- compileSpecialFormEntryPoint "hash-table-delete!" symCompiledIdx copts
 -- Compile index, then use a wrapper to pass it as an arg while compiling obj
 compiledIdx <- compileExpr env rkey symCompiledIdx (Just symDoDelete) 
 -- Do actual update
 compiledUpdate <- return $ AstFunction symDoDelete " env cont rkey _ " [
    -- TODO: this should be more robust, than just assuming ht is a HashTable
    AstValue $ "  HashTable ht <- getVar env \"" ++ var ++ "\"",
    AstValue $ "  HashTable ht' <- recDerefPtrs $ HashTable ht",
    AstValue $ "  result <- updateObject env \"" ++ var ++ "\" (HashTable $ Data.Map.delete rkey ht') ",
    createAstCont copts "result" ""]

 return $ [entryPt, compiledUpdate] ++ compiledIdx
-- TODO: eval env cont fargs@(List (Atom "hash-table-delete!" : args)) = do

-- FUTURE: eventually it should be possible to evaluate the args instead of assuming
-- that they are all strings, but lets keep it simple for now
compile env (List [Atom "load-ffi", 
                        String moduleName, 
                        String externalFuncName, 
                        String internalFuncName]) copts = do
--  Atom symLoadFFI <- _gensym "loadFFI"

  -- Only append module again if it is not already in the list
  List l <- getNamespacedVar env "internal" "imports"
  _ <- if not ((String moduleName) `elem` l)
          then setNamespacedVar env "internal" "imports" $ List $ l ++ [String moduleName]
          else return $ String ""

  -- Pass along moduleName as another top-level import
  return [createAstFunc copts [
    AstValue $ "  result <- defineVar env \"" ++ 
        internalFuncName ++ "\" $ IOFunc " ++ 
        moduleName ++ "." ++ externalFuncName,
    createAstCont copts "result" ""]]

compile env args@(List (_ : _)) copts = mfunc env args compileApply copts 
compile _ badForm _ = throwError $ BadSpecialForm "Unrecognized special form" badForm

mcompile :: Env -> LispVal -> CompOpts -> IOThrowsError [HaskAST]
mcompile env lisp copts = mfunc env lisp compile copts
mfunc :: Env -> LispVal -> (Env -> LispVal -> CompOpts -> IOThrowsError [HaskAST]) -> CompOpts -> IOThrowsError [HaskAST] 
mfunc env lisp func copts = do
  expanded <- Language.Scheme.Macro.macroEval env lisp Language.Scheme.Core.apply
  divertVars env expanded copts func

-- |Do the actual insertion of diverted variables back to the 
--  compiled program.
divertVars 
    :: Env 
    -- ^ Current compile Environment
    -> LispVal
    -- ^ Lisp code after macro expansion
    -> CompOpts
    -- ^ Compiler options
    -> (Env -> LispVal -> CompOpts -> IOThrowsError [HaskAST])
    -- ^ Continuation to call into after vars are diverted
    -> IOThrowsError [HaskAST]
    -- ^ Code generated by the continuation, along with the code
    --   added to divert vars to the compiled program
divertVars env expanded copts@(CompileOptions tfnc uvar uargs nfnc) func = do
  vars <- Language.Scheme.Macro.getDivertedVars env
  case vars of 
    [] -> func env expanded copts
    _ -> do 
      Atom symNext <- _gensym "afterDivert"
      diverted <- compileDivertedVars symNext env vars copts
      rest <- func env expanded $ CompileOptions symNext uvar uargs nfnc
      return $ [diverted] ++ rest

-- |Take a list of variables diverted into env at compile time, and
--  divert them into the env at runtime
compileDivertedVars :: String -> Env -> [LispVal] -> CompOpts -> IOThrowsError HaskAST
compileDivertedVars 
  formNext env vars 
  copts@(CompileOptions thisFunc useVal useArgs nextFunc) = do
  let val = case useVal of
        True -> "value"
        _ -> "Nil \"\""
      args = case useArgs of
        True -> "(Just args)"
        _ -> "Nothing"
      comp (List [Atom renamed, Atom orig]) = do
        [AstValue $ "  v <- getVar env \"" ++ orig ++ "\"",
         AstValue $ "  _ <- defineVar env \"" ++ renamed ++ "\" v"]
      cvars = map comp vars 
      f = (concat cvars) ++ 
          [AstValue $ "  " ++ formNext ++ " env cont (" ++ val ++ ") " ++ args]
  return $ createAstFunc copts f

{- TODO: adapt for compilation
meval, mprepareApply :: Env -> LispVal -> LispVal -> IOThrowsError LispVal
meval env cont lisp = mfunc env cont lisp eval
mprepareApply env cont lisp = mfunc env cont lisp prepareApply
-}


-- TODO: a helper function to allow special forms to be redefined at runtime...
compileSpecialFormEntryPoint :: String -> String -> CompOpts -> IOThrowsError HaskAST
compileSpecialFormEntryPoint formName formSym copts = do
 compileSpecialForm formName ("do " ++ formSym ++ " env cont (Nil \"\") []") copts

compileSpecialForm :: String -> String -> CompOpts -> IOThrowsError HaskAST
compileSpecialForm formName formCode copts = do
 f <- return $ [AstValue $ "  bound <- liftIO $ isRecBound env \"" ++ formName ++ "\"",
       AstValue $ "  if bound ",
       AstValue $ "     then throwError $ NotImplemented \"prepareApply env cont args\" ", -- if is bound to a variable in this scope; call into it
       AstValue $ "     else " ++ formCode]
 return $ createAstFunc copts f



-- Compile an intermediate expression (such as an arg to if) and 
-- call into the next continuation with it's value
compileExpr :: Env -> LispVal -> String -> Maybe String -> IOThrowsError [HaskAST]
compileExpr env expr symThisFunc fForNextExpr = do
  mcompile env expr (CompileOptions symThisFunc False False fForNextExpr) 

-- |Compiles each argument to a function call, and then uses apply to call the function
compileApply :: Env -> LispVal -> CompOpts -> IOThrowsError [HaskAST]
compileApply env (List (func : fparams)) (CompileOptions coptsThis _ _ coptsNext) = do
  Atom stubFunc <- _gensym "applyStubF"
  Atom wrapperFunc <- _gensym "applyWrapper"
  Atom nextFunc <- _gensym "applyNextF"

  c <- return $ AstFunction coptsThis " env cont _ _ " [AstValue $ "  continueEval env (makeCPS env (makeCPS env cont " ++ wrapperFunc ++ ") " ++ stubFunc ++ ") $ Nil\"\""]  
  -- Use wrapper to pass high-order function (func) as an argument to apply
  wrapper <- return $ AstFunction wrapperFunc " env cont value _ " [AstValue $ "  continueEval env (makeCPSWArgs env cont " ++ nextFunc ++ " [value]) $ Nil \"\""]
  _comp <- mcompile env func $ CompileOptions stubFunc False False Nothing
  rest <- compileArgs nextFunc False fparams -- False since no value passed in this time

  return $ [c, wrapper ] ++ _comp ++ rest
 where 
  -- TODO: this pattern may need to be extracted into a common place for use in other similar
  --       situations, such as params to a lambda expression
  compileArgs :: String -> Bool -> [LispVal] -> IOThrowsError [HaskAST]
  compileArgs thisFunc thisFuncUseValue args = do
    case args of
      [] -> do
           -- The basic idea is that if there is a next expression, call into it as a new continuation
           -- instead of calling into cont
           case coptsNext of
             Nothing -> return $ [
               AstFunction thisFunc 
                " env cont (Nil _) (Just (a:as)) " [AstValue "  apply cont a as "],
               AstFunction thisFunc 
                " env cont value (Just (a:as)) " [AstValue "  apply cont a $ as ++ [value] "]]
             Just fnextExpr -> return $ [
               AstFunction thisFunc 
                " env cont (Nil _) (Just (a:as)) " [AstValue $ "  apply (makeCPS env cont " ++ fnextExpr ++ ") a as "],
               AstFunction thisFunc 
                " env cont value (Just (a:as)) " [AstValue $ "  apply (makeCPS env cont " ++ fnextExpr ++ ") a $ as ++ [value] "]]
      (a:as) -> do
        Atom stubFunc <- _gensym "applyFirstArg" -- Call into compiled stub
        Atom nextFunc <- _gensym "applyNextArg" -- Next func argument to execute...
        _comp <- mcompile env a $ CompileOptions stubFunc False False Nothing

        -- Flag below means that the expression's value matters, add it to args
        f <- if thisFuncUseValue
                then return $ AstValue $ thisFunc ++ " env cont value (Just args) = do "
                else return $ AstValue $ thisFunc ++ " env cont _ (Just args) = do "
        c <- if thisFuncUseValue
                then return $ AstValue $ "  continueEval env (makeCPS env (makeCPSWArgs env cont " ++ nextFunc ++ " $ args ++ [value]) " ++ stubFunc ++ ") $ Nil\"\""  
                else return $ AstValue $ "  continueEval env (makeCPS env (makeCPSWArgs env cont " ++ nextFunc ++ " args) " ++ stubFunc ++ ") $ Nil\"\""  

        rest <- compileArgs nextFunc True as -- True indicates nextFunc needs to use value arg passed into it
        return $ [ f, c] ++ _comp ++ rest

