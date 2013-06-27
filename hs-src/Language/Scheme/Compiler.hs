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
import qualified Language.Scheme.Core as LSC (apply, evalLisp, evalString, meval, r5rsEnv, version) 
import qualified Language.Scheme.Macro
import Language.Scheme.Primitives
import Language.Scheme.Types
import qualified Language.Scheme.Util (escapeBackslashes)
import Language.Scheme.Variables
import Control.Monad.Error
import qualified Data.Array
import qualified Data.ByteString as BS
import Data.Complex
import qualified Data.List
import qualified Data.Map
import Data.Ratio
import Data.Word
import Debug.Trace

-- |A type to store options passed to compile
--  eventually all of this might be able to be 
--  integrated into a Compile monad
data CompOpts = CompileOptions {
    coptsThisFunc :: String,
    coptsThisFuncUseValue :: Bool,
    coptsThisFuncUseArgs :: Bool,
    coptsNextFunc :: Maybe String
    }

defaultCompileOptions :: String -> CompOpts
defaultCompileOptions thisFunc = CompileOptions thisFunc False False Nothing

-- |Create code for a function
createAstFunc 
  :: CompOpts  -- ^ Compilation options
  -> [HaskAST] -- ^ Body of the function
  -> HaskAST -- ^ Complete function code
createAstFunc (CompileOptions thisFunc useVal useArgs _) funcBody = do
  let val = case useVal of
              True -> "value"
              _ -> "_"
      args = case useArgs of
               True -> "(Just args)"
               _ -> "_"
  AstFunction thisFunc (" env cont " ++ val ++ " " ++ args ++ " ") funcBody

-- |Create code for a continutation
createAstCont 
  :: CompOpts -- ^ Compilation options
  -> String -- ^ Value to send to the continuation
  -> String -- ^ Extra leading indentation (or blank string if none)
  -> HaskAST -- ^ Generated code
createAstCont (CompileOptions _ _ _ (Just nextFunc)) var indentation = do
  AstValue $ indentation ++ "  continueEval env (makeCPS env cont " ++ nextFunc ++ ") " ++ var
createAstCont (CompileOptions _ _ _ Nothing) var indentation = do
  AstValue $ indentation ++ "  continueEval env cont " ++ var

-- |A very basic type to store a Haskell AST.
--  FUTURE: is this even necessary? Would just a string be good enough?
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

showValAST (AstContinuation nextFunc args) =
    "  continueEval env (makeCPSWArgs env cont " ++ 
       nextFunc ++ " " ++ args ++ ") $ Nil \"\""

instance Show HaskAST where show = showValAST

-- |A utility function to join list members together
joinL 
  :: forall a. [[a]] -- ^ Original list-of-lists
  -> [a] -- ^ Separator 
  -> [a] -- ^ Joined list
joinL ls sep = concat $ Data.List.intersperse sep ls

-- |Convert abstract syntax tree to a string
ast2Str :: LispVal -> String 
ast2Str (String s) = "String " ++ show s
ast2Str (Char c) = "Char " ++ show c
ast2Str (Atom a) = "Atom " ++ show a
ast2Str (Number n) = "Number (" ++ show n ++ ")"
ast2Str (Complex c) = "Complex $ (" ++ (show $ realPart c) ++ ") :+ (" ++ (show $ imagPart c) ++ ")"
ast2Str (Rational r) = "Rational $ (" ++ (show $ numerator r) ++ ") % (" ++ (show $ denominator r) ++ ")"
ast2Str (Float f) = "Float (" ++ show f ++ ")"
ast2Str (Bool True) = "Bool True"
ast2Str (Bool False) = "Bool False"
ast2Str (HashTable ht) = do
 let ls = Data.Map.toList ht 
     conv (a, b) = "(" ++ ast2Str a ++ "," ++ ast2Str b ++ ")"
 "HashTable $ Data.Map.fromList $ [" ++ joinL (map conv ls) "," ++ "]"
ast2Str (Vector v) = do
  let ls = Data.Array.elems v
      size = (length ls) - 1
  "Vector (listArray (0, " ++ show size ++ ")" ++ "[" ++ joinL (map ast2Str ls) "," ++ "])"
ast2Str (ByteVector bv) = do
  let ls = BS.unpack bv
  "ByteVector ( BS.pack " ++ "[" ++ joinL (map show ls) "," ++ "])"
ast2Str (List ls) = "List [" ++ joinL (map ast2Str ls) "," ++ "]"
ast2Str (DottedList ls l) = 
  "DottedList [" ++ joinL (map ast2Str ls) "," ++ "] $ " ++ ast2Str l

-- |Convert a list of abstract syntax trees to a list of strings
asts2Str :: [LispVal] -> String
asts2Str ls = do
    "[" ++ (joinL (map ast2Str ls) ",") ++ "]"

headerComment, headerModule, headerImports :: [String]
headerComment = [
   "--"
 , "-- This file was automatically generated by the husk scheme compiler,"
 , "-- huskc version " ++ LSC.version
 , "--"]

headerModule = ["module Main where "]
headerImports = [
   "Language.Scheme.Core "
 , "Language.Scheme.Numerical "
 , "Language.Scheme.Primitives "
 , "Language.Scheme.Types     -- Scheme data types "
 , "Language.Scheme.Variables -- Scheme variable operations "
 , "Control.Monad.Error "
 , "Data.Array "
 , " qualified Data.ByteString as BS "
 , "Data.Complex "
 , " qualified Data.Map "
 , "Data.Ratio "
 , "Data.Word "
 , "System.IO "]

header :: String -> Bool -> [String]
header filepath useCompiledLibs = do
  let env = if useCompiledLibs
            then "primitiveBindings"
            else "r5rsEnv"
  [ " "
    , "-- |Get variable at runtime "
    , "getRTVar env var = do " 
    , "  v <- getVar env var " 
    , "  return $ case v of "
    , "    List _ -> Pointer var env "
    , "    DottedList _ _ -> Pointer var env "
    , "    String _ -> Pointer var env "
    , "    Vector _ -> Pointer var env "
    , "    ByteVector _ -> Pointer var env "
    , "    HashTable _ -> Pointer var env "
    , "    _ -> v "
    , " "
    , "applyWrapper env cont (Nil _) (Just (a:as))  = do "
    , "  apply cont a as "
    , " "
    , "applyWrapper env cont value (Just (a:as))  = do "
    , "  apply cont a $ as ++ [value] "
    , " "
    , "getDataFileName' :: FilePath -> IO FilePath "
    , "getDataFileName' name = return $ \"" ++ (Language.Scheme.Util.escapeBackslashes filepath) ++ "\" ++ name "
    , " "
    , "exec55_3 env cont _ _ = do "
    , "  liftIO $ registerExtensions env getDataFileName' "
    , "  continueEval env (makeCPS env cont exec) (Nil \"\")"
    , " "
    , "main :: IO () "
    , "main = do "
    , "  env <- " ++ env ++ " "
    , "  result <- (runIOThrows $ liftM show $ hsInit env (makeNullContinuation env) (Nil \"\") Nothing) "
    , "  case result of "
    , "    Just errMsg -> putStrLn errMsg "
    , "    _ -> return () "
    , " "
    , "hsInit env cont _ _ = do "
    , "  _ <- defineVar env \" modules \" $ HashTable $ Data.Map.fromList [] "
    , "  run env cont (Nil \"\") []"
    , " "]

-- NOTE: the following type is used for all functions generated by the compiler: 
-- , "run :: Env -> LispVal -> LispVal -> Maybe [LispVal] -> IOThrowsError LispVal "


-- Define imports var here as an empty list.
-- This list is appended to by (load-ffi) instances,
-- and the imports are explicitly added later on...
initializeCompiler :: Env -> IOThrowsError [HaskAST]
initializeCompiler env = do
  _ <- defineNamespacedVar env 't' {-"internal"-} "imports" $ List []
  return []


compileLisp :: Env -> String -> String -> Maybe String -> IOThrowsError [HaskAST]
compileLisp env filename entryPoint exitPoint = load filename >>= compileBlock entryPoint exitPoint env []
-- compileBlock
--
-- Note: Uses explicit recursion to transform a block of code, because
--  later lines may depend on previous ones
compileBlock :: String -> Maybe String -> Env -> [HaskAST] -> [LispVal] -> IOThrowsError [HaskAST]
compileBlock symThisFunc symLastFunc env result lisps = do
  _ <- defineTopLevelVars env lisps
  _compileBlock symThisFunc symLastFunc env result lisps

_compileBlock :: String -> Maybe String -> Env -> [HaskAST] -> [LispVal] -> IOThrowsError [HaskAST]
_compileBlock symThisFunc symLastFunc env result [c] = do
  compiled <- mcompile env c $ CompileOptions symThisFunc False False symLastFunc 
  return $ result ++ compiled
_compileBlock symThisFunc symLastFunc env result (c:cs) = do
  Atom symNextFunc <- _gensym "f"
  compiled <- mcompile env c $ CompileOptions symThisFunc False False (Just symNextFunc)
  _compileBlock symNextFunc symLastFunc env (result ++ compiled) cs
_compileBlock _ _ _ result [] = return result

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

-- |Add lambda variables to the compiler's environment
defineLambdaVars :: Env -> [LispVal] -> IOThrowsError LispVal
defineLambdaVars env (Atom v : vs) = do
    _ <- defineVar env v $ Number 0 -- For now it is good enough to define it, actual value does not matter
    defineLambdaVars env vs
defineLambdaVars env (_ : vs) = defineLambdaVars env vs
defineLambdaVars env [] = return $ Nil ""

-- |Find all variables defined at "this" level and load their symbols into
--  the environment. This allows the compiler validation to work even 
--  though a variable is used in a sub-form before it is defined further
--  on down in the program
defineTopLevelVars :: Env -> [LispVal] -> IOThrowsError LispVal
defineTopLevelVars env (List [Atom "define", Atom var, form] : ls) = do
    _ <- defineTopLevelVar env var
    defineTopLevelVars env ls
defineTopLevelVars env ((List (Atom "define" : List (Atom var : _) : _)) : ls) = do
    _ <- defineTopLevelVar env var
    defineTopLevelVars env ls
defineTopLevelVars env ((List (Atom "define" : DottedList (Atom var : _) _ : _)) : ls) = do
    _ <- defineTopLevelVar env var
    defineTopLevelVars env ls
defineTopLevelVars env (_ : ls) = defineTopLevelVars env ls
defineTopLevelVars _ _ = return nullLisp 

defineTopLevelVar env var = do
  defineVar env var $ Number 0 -- Actual value not loaded at the moment 


-- Module section
-- TODO: move this into another sub-file, once it is more mature


-- Like evalLisp, but preserve pointers in the output
eval :: Env -> LispVal -> IOThrowsError LispVal
eval env lisp = do
  LSC.meval env (makeNullContinuation env) lisp


-- Top-level import
importTL env metaEnv (m : ms) copts@(CompileOptions thisFunc _ _ lastFunc) = do
    Atom symImport <- _gensym "importFnc"

    -- Resolve import
    List (moduleName : imports) <- LSC.evalLisp metaEnv $ 
         List [Atom  "resolve-import", List [Atom "quote", m]]
    -- Load module
    code <- loadModule metaEnv moduleName $ CompileOptions thisFunc False False (Just symImport)

    -- Get module env, and import module env into env
    LispEnv modEnv <- LSC.evalLisp metaEnv $ List [Atom "module-env", List [Atom "find-module", List [Atom "quote", moduleName]]]
    _ <- eval env $ List [Atom "%import", LispEnv env, LispEnv modEnv, List [Atom "quote", List imports], Bool False]

    importFunc <- return $ [
        -- fromEnv is a LispEnv passed in as the 'value' parameter
        AstValue $ "  _ <- evalLisp env $ List [Atom \"%import\", LispEnv env, value, List [Atom \"quote\", " ++ (ast2Str $ List imports) ++ "], Bool False]",
        createAstCont (CompileOptions symImport False False lastFunc) "(Nil \"\")" ""]

-- TODO: this is not good enough, need to compile the %import as well, using symImport...
    return $ [createAstFunc (CompileOptions symImport True False lastFunc) importFunc] ++ code

-- TODO: importTL env [m] - special case for last one (?)
-- END module section

-- TODO: should return module vector?
-- but maybe not, because our eval-module will need to compile the
-- module, so we may need to do something special like return a 
-- tuple containing both pieces of information. Or do we even need
-- the module vector, since we can just look it up by name in importTL
--
loadModule metaEnv name copts@(CompileOptions thisFunc _ _ lastFunc) = do
    -- Get the module definition, or load it from file if necessary
    mod' <- eval metaEnv $ List [Atom "find-module", List [Atom "quote", name]]
    case mod' of
        Bool False -> return [] -- Even possible to reach this line?
        _ -> do
             mod <- recDerefPtrs mod'
             modEnv <- LSC.evalLisp metaEnv $ List [Atom "module-env", mod]
             case modEnv of
                Bool False -> do
                {-
                    Control flow for compiled code:

                     - create new env
                     - call into func directly to load it
                     - return new env and save to memory
                     - continue on to lastFunc
                -}
                    Atom symStartLoadNewEnv <- _gensym "startLoadingNewEnvFnc"
                    Atom symEndLoadNewEnv <- _gensym "doneLoadingNewEnvFnc"

                    newEnvFunc <- return $ [
                        AstValue $ "  newEnv <- liftIO $ nullEnv",
                        AstValue $ "  _ <- " ++ symStartLoadNewEnv ++ " newEnv (makeNullContinuation newEnv) (Nil \"\") []",
-- TODO: need to store env in runtime memory with key of 'name'
--       that way it is available later if another module wants to import it
                        createAstCont copts "(LispEnv newEnv)" ""]
                    
                    -- Create new env for module, per eval-module
                    newEnv <- liftIO $ nullEnv
                    -- compile the module code, again per eval-module
                    result <- compileModule newEnv metaEnv name mod $
                        CompileOptions symStartLoadNewEnv False False (Just symEndLoadNewEnv)
                    modWEnv <- eval metaEnv $ List (Atom "module-env-set!" : mod' : [LispEnv newEnv]) 
                    -- Above does not update *modules* correctly, so we del/add below
                    _ <- eval metaEnv $ List [Atom "delete-module!", List [Atom "quote", name]]
                    _ <- eval metaEnv $ List [Atom "add-module!", List [Atom "quote", name], modWEnv]

                    return $ [createAstFunc copts newEnvFunc] ++
                             [createAstFunc (CompileOptions symEndLoadNewEnv False False Nothing) [AstValue "  return $ Nil \"\""]] ++
                             result
                _ -> return [] --mod

-- TODO: write compileModule here, it will be based off of the eval-module code from the meta-language, and can take cues from compileModule below
compileModule env metaEnv name mod copts@(CompileOptions thisFunc _ _ lastFunc) = do
    -- TODO: set mod meta-data to avoid cyclic references

    metaData <- LSC.evalLisp metaEnv $ List [Atom "module-meta-data", List [Atom "quote", mod]]
    cmd env metaEnv metaData [] copts

-- Compile module directive, rename it later (TODO)
cmd env metaEnv (List ((List (Atom "begin" : code)) : ls)) result copts@(CompileOptions thisFunc _ _ lastFunc) = do
    compileBlock thisFunc lastFunc env [] code
cmd env metaEnv (List (_ : ls)) result copts = 
    cmd env metaEnv (List ls) result copts
cmd _ _ _ result _ = return result




compile :: Env -> LispVal -> CompOpts -> IOThrowsError [HaskAST]
-- Experimenting with r7rs library support
compile env ast@(List (Atom "import" : mods)) copts@(CompileOptions thisFunc _ _ lastFunc) = do
    LispEnv meta <- getVar env "*meta-env*"
    importTL env meta mods copts
    

{- Testing code:
    envTmp <- liftIO $ LSC.r5rsEnv
    _ <- LSC.evalLisp envTmp ast --(trace ("debug - evaluating " ++ (show ast)) ast)

    List modules' <- getVar meta "*modules*" >>= recDerefPtrs
    let modules = reverse modules'
    --compileModules meta (trace ("compileModules: " ++ show modules) modules) thisFunc lastFunc
    compileModules meta modules thisFunc lastFunc
-}
    -- TODO: I think modules can be compiled in reverse order, since this is the order they are added by the evaluator

    -- TODO: can use (module-exports) from meta-language to get export lists
    -- actually, might be smarter to do the same thing repl-import does, process
    -- each import statement, using resolve-import to get the import list.
    -- then process the module accordingly.
    --
    -- should leverage the meta-language as much as possible to avoid duplicating effort

    -- Will probably need to 'cheat' somehow for modules such as r5rs that are really just an environment,
    -- since the compiler works on lisp expressions and not environments. may need to figure out what
    -- environment it is somehow and bake that into the compiled code


    -- TODO: may have trouble with a module that itself calls `import`, since eval-module duplicates
    -- the code used in repl-import. may need a special case for these imports
    -- ^ then again, not sure that matters because the interpreter loads all of these
    --   imports into *modules*


  {- notes on module compilation:
  in order to compile a module, we first need to introduce an env. per eval-module,
  we can use (make-environment) unless the module contains an env, in which case
  we use that (probably this is only applicable to built-in modules such as scheme r5rs).
  then all code is compiled using this env, much like a lambda (except will need to
  create a new env instead of just passing along env like in a lambda def)
  
  we probably need to 
  maintain a *modules* data structure at runtime in the compiled code, so after a module
  is compiled and executed, it's exports are available to other modules

  assume %import statements will also have to be added to the compiled code, since a module
  that includes another will need to import it's exports, and likewise the top-level will 
  need to import the exports of each module it uses
  -}
  where
 compileModules meta modules@[m] symThisFunc symLastFunc = do
   compileModule meta m symThisFunc symLastFunc
 compileModules meta modules@(m : ms) symThisFunc symLastFunc = do
   Atom symNextFunc <- _gensym "mf"
   c <- compileModule meta m symThisFunc (Just symNextFunc)
   rest <- compileModules meta ms symNextFunc symLastFunc
   return $ c ++ rest
 compileModules _ [] _ _ = return []

 compileModule meta (DottedList [name@(List ns)] mod) symThisFunc symLastFunc = do

-- TODO: if  module has already been compiled, we just need to %import it's contents.
-- otherwise, we need to compile the module code and *then* %imports it's contents


   exports <- LSC.evalLisp meta $ List [Atom "%module-exports", mod]
   mEnv <- LSC.evalLisp meta $ List [Atom "module-env", mod]
   mData <- LSC.evalLisp meta $ List [Atom "module-meta-data", mod]
--   throwError $ Default ("module = " ++ (show ns) ++ ", exports = " ++ (show exports) ++ ", data = " ++ (show mData))
   --case (trace ("compileModule: " ++ show name) exports) of
   case (exports) of
    -- The special case of a module that is just an env, IE r5rs
    -- Just skip it for now
    --
    -- TODO: WTF is exec generated twice??
    Bool False -> do
        let copts' = CompileOptions symThisFunc False False symLastFunc
        let envStr = case name of
                        List [Atom "scheme", Atom "r5rs"] ->
                            "interaction-environment"
        return [createAstFunc copts' [
            -- TODO: need to identify env using module name, and
            -- add it to *modules* at runtime
--            AstValue $ "  LispEnv e <- evalLisp env $ List [Atom \"" ++ (show envStr) ++ "\"]"
            AstValue $ "  val <- defineVar env \"*modules*\" $ String \"TODO\""], 
            createAstCont copts' "val" ""]
    List es -> do
        case mData of
            List cs -> do
                let code = findBegin cs --(trace ("findBegin, cs = " ++ show cs) cs)
                compileBlock symThisFunc symLastFunc env [] code --(trace ("compileBlock: " ++ show code) code)
            err -> throwError $ Default $ "Unexpected meta data: " ++ show err
        -- TODO: should actually compile the whole module within
        -- its own env, and store the env in *modules* at runtime
    err -> throwError $ Default $ "Unexpected exports in compileModule: " ++ show err

-- TODO on above: need to figure out how to tweak copts so unique functions are created; probably need to 
-- all the appropriate function to update copts, and pass it along as a parameter to compileModule*
--
-- also how to figure out where to pick back up? does the outer copts already handle that? I suppose
-- it would have to in the current compiler

 -- Find the begin statement that contains the main module code
 findBegin (List (Atom "begin" : code) : cs) = code
 findBegin (c : cs) = findBegin cs
 findBegin _ = []

compile _ (Nil n) copts = compileScalar ("  return $ Nil " ++ (show n)) copts
compile _ (String s) copts = compileScalar ("  return $ String " ++ (show s)) copts
compile _ (Char c) copts = compileScalar ("  return $ Char " ++ (show c)) copts
compile _ (Complex c) copts = compileScalar ("  return $ Complex $ (" ++ (show $ realPart c) ++ ") :+ (" ++ (show $ imagPart c) ++ ")") copts
compile _ (Float f) copts = compileScalar ("  return $ Float (" ++ (show f) ++ ")") copts
compile _ (Rational r) copts = compileScalar ("  return $ Rational $ (" ++ (show $ numerator r) ++ ") % (" ++ (show $ denominator r) ++ ")") copts 
compile _ (Number n) copts = compileScalar ("  return $ Number (" ++ (show n) ++ ")") copts
compile _ (Bool b) copts = compileScalar ("  return $ Bool " ++ (show b)) copts
compile _ v@(Vector _) copts = compileScalar (" return $ " ++ ast2Str v) copts
compile _ v@(ByteVector _) copts = compileScalar (" return $ " ++ ast2Str v) copts
compile _ ht@(HashTable _) copts = compileScalar (" return $ " ++ ast2Str ht) copts
compile env (Atom a) copts = do
 isDefined <- liftIO $ isRecBound env a
 case isDefined of
   True -> do
     return [createAstFunc copts [
               AstValue $ "  val <- getRTVar env \"" ++ a ++ "\""], 
               createAstCont copts "val" ""]
   False -> throwError $ UnboundVar "Variable is not defined" a

compile _ (List [Atom "quote", val]) copts = compileScalar (" return $ " ++ ast2Str val) copts

compile env ast@(List [Atom "expand",  _body]) copts = do
  compileSpecialFormBody env ast copts (\ _ -> do
    val <- Language.Scheme.Macro.expand env False _body LSC.apply
    compileScalar (" return $ " ++ ast2Str val) copts)

compile env ast@(List (Atom "let-syntax" : List _bindings : _body)) copts = do
  compileSpecialFormBody env ast copts (\ _ -> do
    bodyEnv <- liftIO $ extendEnv env []
    _ <- Language.Scheme.Macro.loadMacros env bodyEnv Nothing False _bindings
    -- Expand whole body as a single continuous macro, to ensure hygiene
    expanded <- Language.Scheme.Macro.expand bodyEnv False (List _body) LSC.apply
    divertVars bodyEnv expanded copts compexp)
 where 
     -- Pick up execution here after expansion
     compexp bodyEnv' expanded' copts' = do
       case expanded' of
         List e -> compile bodyEnv' (List $ Atom "begin" : e) copts'
         e -> compile bodyEnv' e copts'

compile env ast@(List (Atom "letrec-syntax" : List _bindings : _body)) copts = do
  compileSpecialFormBody env ast copts (\ _ -> do
    bodyEnv <- liftIO $ extendEnv env []
    _ <- Language.Scheme.Macro.loadMacros bodyEnv bodyEnv Nothing False _bindings
    -- Expand whole body as a single continuous macro, to ensure hygiene
    expanded <- Language.Scheme.Macro.expand bodyEnv False (List _body) LSC.apply
    divertVars bodyEnv expanded copts compexp)
  where 
     -- Pick up execution here after expansion
     compexp bodyEnv' expanded' copts' = do
       case expanded' of
         List e -> compile bodyEnv' (List $ Atom "begin" : e) copts'
         e -> compile bodyEnv' e copts'

-- A non-standard way to rebind a macro to another keyword
compile env 
        ast@(List [Atom "define-syntax", 
                   Atom newKeyword,
                   Atom keyword]) 
        copts = do
  bound <- getNamespacedVar' env macroNamespace keyword
  case bound of
    Just m -> do
        defineNamespacedVar env macroNamespace newKeyword m
        compFunc <- return $ [
          AstValue $ "  bound <- getNamespacedVar' env macroNamespace \"" ++ keyword ++ "\"",
          AstValue $ "  case bound of ",
          AstValue $ "    Just m -> ",
          AstValue $ "      defineNamespacedVar env macroNamespace \"" ++ newKeyword ++ "\" m",
          AstValue $ "    Nothing -> throwError $ TypeMismatch \"macro\" $ Atom \"" ++ keyword ++ "\"",
          createAstCont copts "(Nil \"\")" ""]
        return $ [createAstFunc copts compFunc]
    Nothing -> throwError $ TypeMismatch "macro" $ Atom keyword

compile env ast@(List [Atom "define-syntax", Atom keyword,
  (List [Atom "er-macro-transformer", 
    (List (Atom "lambda" : List fparams : fbody))])])
  copts = do
  compileSpecialFormBody env ast copts (\ _ -> do
    let fparamsStr = asts2Str fparams
        fbodyStr = asts2Str fbody
  
    f <- makeNormalFunc env fparams fbody 
    _ <- defineNamespacedVar env macroNamespace keyword $ SyntaxExplicitRenaming f
  
    compFunc <- return $ [
      AstValue $ "  f <- makeNormalFunc env " ++ fparamsStr ++ " " ++ fbodyStr, 
      AstValue $ "  defineNamespacedVar env macroNamespace \"" ++ keyword ++ "\" $ SyntaxExplicitRenaming f",
      createAstCont copts "(Nil \"\")" ""]
    return $ [createAstFunc copts compFunc])

compile env lisp@(List [Atom "define-syntax", Atom keyword, 
    (List (Atom "syntax-rules" : (List identifiers : rules)))]) copts = do
  compileSpecialFormBody env lisp copts (\ _ -> do
    let idStr = asts2Str identifiers
        ruleStr = asts2Str rules
  
    -- Make macro available at compile time
    _ <- defineNamespacedVar env macroNamespace keyword $ 
           Syntax (Just env) Nothing False identifiers rules
  
    -- And load it at runtime as well
    -- Env should be identical to the one loaded at compile time...
    compileScalar 
      ("  defineNamespacedVar env macroNamespace \"" ++ keyword ++ 
       "\" $ Syntax (Just env) Nothing False " ++ idStr ++ " " ++ ruleStr) copts)

compile env ast@(List [Atom "if", predic, conseq]) copts = 
  compileSpecialFormBody env ast copts (\ _ -> do
    compile env (List [Atom "if", predic, conseq, Nil ""]) copts)

compile env ast@(List [Atom "if", predic, conseq, alt]) copts = do
  compileSpecialFormBody env ast copts (\ nextFunc -> do
    Atom symPredicate <- _gensym "ifPredic"
    Atom symCheckPredicate <- _gensym "compiledIfPredicate"
    Atom symConsequence <- _gensym "compiledConsequence"
    Atom symAlternate <- _gensym "compiledAlternative"

    -- Entry point; ensure if is not rebound
    f <- return [AstValue $ "  " ++ symPredicate ++
                            " env (makeCPS env cont " ++ symCheckPredicate ++ ") " ++ 
                            " (Nil \"\") [] "]
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
    return $ [createAstFunc copts f] ++ compPredicate ++ [compCheckPredicate] ++ compConsequence ++ compAlternate)

compile env ast@(List [Atom "set!", Atom var, form]) copts@(CompileOptions _ _ _ _) = do
  compileSpecialFormBody env ast copts (\ nextFunc -> do
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
    return $ [entryPt] ++ compDefine ++ [compMakeDefine])

compile env ast@(List [Atom "set!", nonvar, _]) copts = do 
  compileSpecialFormBody env ast copts (\ nextFunc -> do
    f <- compileSpecialForm "set!" ("throwError $ TypeMismatch \"variable\" $ String \"" ++ (show nonvar) ++ "\"")  copts
    return [f])
compile env ast@(List (Atom "set!" : args)) copts = do
  compileSpecialFormBody env ast copts (\ nextFunc -> do
    f <- compileSpecialForm "set!" ("throwError $ NumArgs 2 $ [String \"" ++ (show args) ++ "\"]") copts -- TODO: Cheesy to use a string, but fine for now...
    return [f])

compile env ast@(List [Atom "define", Atom var, form]) copts@(CompileOptions _ _ _ _) = do
  compileSpecialFormBody env ast copts (\ nextFunc -> do
    Atom symDefine <- _gensym "defineFuncDefine"
    Atom symMakeDefine <- _gensym "defineFuncMakeDef"
   
    -- Store var in huskc's env for macro processing (and same for other vers of define)
    _ <- defineVar env var form


    -- WORKAROUND #1
    -- Special case to support require-extension
    case form of
        List [Atom "current-environment"] -> 
            defineVar env var $ LispEnv env
        _ -> return $ Nil "" 
    -- End special case

   
    -- Entry point; ensure var is not rebound
    f <- return $ [
          AstValue $ "  " ++ symDefine ++ " env cont (Nil \"\") []" ]
    compDefine <- compileExpr env form symDefine $ Just symMakeDefine
    compMakeDefine <- return $ AstFunction symMakeDefine " env cont result _ " [
       AstValue $ "  _ <- defineVar env \"" ++ var ++ "\" result",
       createAstCont copts "result" ""]
    return $ [createAstFunc copts f] ++ compDefine ++ [compMakeDefine])

compile env ast@(List (Atom "define" : List (Atom var : fparams) : fbody)) copts@(CompileOptions _ _ _ _) = do
  compileSpecialFormBody env ast copts (\ nextFunc -> do
    bodyEnv <- liftIO $ extendEnv env []
    -- bind lambda params in the extended env
    _ <- defineLambdaVars bodyEnv (Atom var : fparams)
   
    Atom symCallfunc <- _gensym "defineFuncEntryPt"
    compiledParams <- compileLambdaList fparams
    compiledBody <- compileBlock symCallfunc Nothing bodyEnv [] fbody
   
    -- Cache macro expansions within function body
    ebody <- mapM (\ lisp -> Language.Scheme.Macro.macroEval env lisp LSC.apply) fbody
    -- Store var in huskc's env for macro processing (and same for other vers of define)
    _ <- makeNormalFunc env fparams ebody >>= defineVar env var
   
    -- Entry point; ensure var is not rebound
    f <- return $ [
          AstValue $ "  result <- makeNormalHFunc env (" ++ compiledParams ++ ") " ++ symCallfunc,
          AstValue $ "  _ <- defineVar env \"" ++ var ++ "\" result ",
          createAstCont copts "result" ""
          ]
    return $ [createAstFunc copts f] ++ compiledBody)

compile env ast@(List (Atom "define" : DottedList (Atom var : fparams) varargs : fbody)) copts@(CompileOptions _ _ _ _) = do
  compileSpecialFormBody env ast copts (\ nextFunc -> do
    bodyEnv <- liftIO $ extendEnv env []
    -- bind lambda params in the extended env
    _ <- defineLambdaVars bodyEnv $ (Atom var : fparams) ++ [varargs]
   
    Atom symCallfunc <- _gensym "defineFuncEntryPt"
    compiledParams <- compileLambdaList fparams
    compiledBody <- compileBlock symCallfunc Nothing bodyEnv [] fbody
   
    -- Store var in huskc's env for macro processing (and same for other vers of define)
    ebody <- mapM (\ lisp -> Language.Scheme.Macro.macroEval env lisp LSC.apply) fbody
    _ <- makeVarargs varargs env fparams ebody >>= defineVar env var
   
    -- Entry point; ensure var is not rebound
    f <- return $ [
          AstValue $ "  result <- makeHVarargs (" ++ ast2Str varargs ++ ") env (" ++ compiledParams ++ ") " ++ symCallfunc,
          AstValue $ "  _ <- defineVar env \"" ++ var ++ "\" result ",
          createAstCont copts "result" ""
          ]
    return $ [createAstFunc copts f] ++ compiledBody)

compile env ast@(List (Atom "lambda" : List fparams : fbody)) copts@(CompileOptions _ _ _ _) = do
  compileSpecialFormBody env ast copts (\ nextFunc -> do
    Atom symCallfunc <- _gensym "lambdaFuncEntryPt"
    compiledParams <- compileLambdaList fparams
   
    bodyEnv <- liftIO $ extendEnv env []
    -- bind lambda params in the extended env
    _ <- defineLambdaVars bodyEnv fparams
   
    compiledBody <- compileBlock symCallfunc Nothing bodyEnv [] fbody
   
    -- Entry point; ensure var is not rebound
   -- TODO: will probably end up creating a common function for this,
   --       since it is almost the same as in "if"
    f <- return $ [
          AstValue $ "  result <- makeNormalHFunc env (" ++ compiledParams ++ ") " ++ symCallfunc,
          createAstCont copts "result" ""
          ]
    return $ [createAstFunc copts f] ++ compiledBody)

compile env ast@(List (Atom "lambda" : DottedList fparams varargs : fbody)) copts@(CompileOptions _ _ _ _) = do
  compileSpecialFormBody env ast copts (\ nextFunc -> do
    Atom symCallfunc <- _gensym "lambdaFuncEntryPt"
    compiledParams <- compileLambdaList fparams
   
    bodyEnv <- liftIO $ extendEnv env []
    -- bind lambda params in the extended env
    _ <- defineLambdaVars bodyEnv $ fparams ++ [varargs]
   
    compiledBody <- compileBlock symCallfunc Nothing bodyEnv [] fbody
   
    -- Entry point; ensure var is not rebound
    f <- return $ [
          AstValue $ "  result <- makeHVarargs (" ++ ast2Str varargs ++ ") env (" ++ compiledParams ++ ") " ++ symCallfunc,
          createAstCont copts "result" ""
          ]
    return $ [createAstFunc copts f] ++ compiledBody)

compile env ast@(List (Atom "lambda" : varargs@(Atom _) : fbody)) copts@(CompileOptions _ _ _ _) = do
  compileSpecialFormBody env ast copts (\ nextFunc -> do
    Atom symCallfunc <- _gensym "lambdaFuncEntryPt"
   
    bodyEnv <- liftIO $ extendEnv env []
    -- bind lambda params in the extended env
    _ <- defineLambdaVars bodyEnv [varargs]
   
    compiledBody <- compileBlock symCallfunc Nothing bodyEnv [] fbody
   
    -- Entry point; ensure var is not rebound
    f <- return $ [
          AstValue $ "  result <- makeHVarargs (" ++ ast2Str varargs ++ ") env [] " ++ symCallfunc,
          createAstCont copts "result" ""
          ]
    return $ [createAstFunc copts f] ++ compiledBody)

compile env ast@(List [Atom "string-set!", Atom var, i, character]) copts = do
  compileSpecialFormBody env ast copts (\ nextFunc -> do
    Atom symDefine <- _gensym "stringSetFunc"
    Atom symMakeDefine <- _gensym "stringSetFuncMakeSet"
   
    entryPt <- compileSpecialFormEntryPoint "string-set!" symDefine copts
    compDefine <- compileExpr env i symDefine $ Just symMakeDefine
    compMakeDefine <- return $ AstFunction symMakeDefine " env cont idx _ " [
       AstValue $ "  tmp <- getVar env \"" ++ var ++ "\"",
       AstValue $ "  derefValue <- recDerefPtrs tmp",
       -- TODO: not entirely correct below; should compile the character argument rather
       --       than directly inserting it into the compiled code...
       AstValue $ "  result <- substr (derefValue, (" ++ ast2Str(character) ++ "), idx)",
       AstValue $ "  _ <- updateObject env \"" ++ var ++ "\" result",
       createAstCont copts "result" ""]
    return $ [entryPt] ++ compDefine ++ [compMakeDefine])

-- TODO: eval env cont args@(List [Atom "string-set!" , nonvar , _ , _ ]) = do
-- TODO: eval env cont fargs@(List (Atom "string-set!" : args)) = do 

compile env ast@(List [Atom "set-car!", Atom var, argObj]) copts = do
  compileSpecialFormBody env ast copts (\ nextFunc -> do
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
    return $ [entryPt, compGetVar, compObj, compDoSet] ++ compiledObj)

-- TODO: eval env cont args@(List [Atom "set-car!" , nonvar , _ ]) = do
-- TODO: eval env cont fargs@(List (Atom "set-car!" : args)) = do

compile env ast@(List [Atom "set-cdr!", Atom var, argObj]) copts = do
  compileSpecialFormBody env ast copts (\ nextFunc -> do
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
                             "   (cons [l', obj']) >>= updateObject e \"" ++ var ++ "\" >>= " ++ finalContinuation ++
                 symDoSet ++ " e c obj (Just [DottedList (l : _) _]) = do\n" ++
                             "   l' <- recDerefPtrs l\n" ++
                             "   obj' <- recDerefPtrs obj\n" ++
                             "   (cons [l', obj']) >>= updateObject e \"" ++ var ++ "\" >>= " ++ finalContinuation ++
                 symDoSet ++ " _ _ _ _ = throwError $ InternalError \"Unexpected argument to " ++ symDoSet ++ "\"\n"
   
    -- Return a list of all the compiled code
    return $ [entryPt, compGetVar, compObj, compDoSet] ++ compiledObj)

-- TODO: eval env cont args@(List [Atom "set-cdr!" , nonvar , _ ]) = do
-- TODO: eval env cont fargs@(List (Atom "set-cdr!" : args)) = do
compile env ast@(List [Atom "vector-set!", Atom var, i, object]) copts = do
  compileSpecialFormBody env ast copts (\ nextFunc -> do
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
   
    return $ [entryPt, compiledIdxWrapper, compiledUpdate] ++ compiledIdx ++ compiledObj)

-- TODO: eval env cont args@(List [Atom "vector-set!" , nonvar , _ , _]) = do 
-- TODO: eval env cont fargs@(List (Atom "vector-set!" : args)) = do 

compile env ast@(List [Atom "bytevector-u8-set!", Atom var, i, object]) copts = do
  compileSpecialFormBody env ast copts (\ nextFunc -> do
    Atom symCompiledIdx <- _gensym "bytevectorSetIdx"
    Atom symCompiledObj <- _gensym "bytevectorSetObj"
    Atom symUpdateVec <- _gensym "bytevectorSetUpdate"
    Atom symIdxWrapper <- _gensym "bytevectorSetIdxWrapper"
   
    -- Entry point that allows this form to be redefined
    entryPt <- compileSpecialFormEntryPoint "bytevector-u8-set!" symCompiledIdx copts
    -- Compile index, then use a wrapper to pass it as an arg while compiling obj
    compiledIdx <- compileExpr env i symCompiledIdx (Just symIdxWrapper) 
    compiledIdxWrapper <- return $ AstFunction symIdxWrapper " env cont idx _ " [
       AstValue $ "  " ++ symCompiledObj ++ " env (makeCPSWArgs env cont " ++ symUpdateVec ++ " [idx]) (Nil \"\") Nothing " ]
    compiledObj <- compileExpr env object symCompiledObj Nothing
    -- Do actual update
    compiledUpdate <- return $ AstFunction symUpdateVec " env cont obj (Just [idx]) " [
       AstValue $ "  vec <- getVar env \"" ++ var ++ "\"",
       AstValue $ "  result <- updateByteVector vec idx obj >>= updateObject env \"" ++ var ++ "\"",
       createAstCont copts "result" ""]
   
    return $ [entryPt, compiledIdxWrapper, compiledUpdate] ++ compiledIdx ++ compiledObj)

-- TODO: eval env cont args@(List [Atom "bytevector-u8-set!" , nonvar , _ , _]) = do 
-- TODO: eval env cont fargs@(List (Atom "bytevector-u8-set!" : args)) = do 

compile env ast@(List [Atom "hash-table-set!", Atom var, rkey, rvalue]) copts = do
  compileSpecialFormBody env ast copts (\ nextFunc -> do
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
   
    return $ [entryPt, compiledIdxWrapper, compiledUpdate] ++ compiledIdx ++ compiledObj)

-- TODO: eval env cont args@(List [Atom "hash-table-set!" , nonvar , _ , _]) = do
-- TODO: eval env cont fargs@(List (Atom "hash-table-set!" : args)) = do

compile env ast@(List [Atom "hash-table-delete!", Atom var, rkey]) copts = do
  compileSpecialFormBody env ast copts (\ nextFunc -> do
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
   
    return $ [entryPt, compiledUpdate] ++ compiledIdx)
-- TODO: eval env cont fargs@(List (Atom "hash-table-delete!" : args)) = do

compile env ast@(List (Atom "%import" : args)) copts = do
  compileSpecialFormBody env ast copts (\ nextFunc -> do
    throwError $ NotImplemented $ "%import, with args: " ++ show args)

compile env (List [a@(Atom "husk-interpreter?")]) copts = do
    mfunc env (List [a, Bool True]) compile copts 

compile env args@(List [Atom "load", filename, envSpec]) copts = do
  -- Explicitly do NOT call compileSpecialFormBody here, since load is not normally a special form

  -- F*ck it, just run the evaluator here since filename is req'd at compile time
  --String filename' <- LSC.evalLisp env filename
  fname <- LSC.evalLisp env filename
  case fname of
    -- Compile contents of the file
    String fn -> compileFile fn

    -- Unable to get filename at compile time, fall back to loading at runtime
    _ -> mfunc env args compileApply copts

 where 
 compileFile filename' = do
  Atom symEnv <- _gensym "loadEnv"
  Atom symLoad <- _gensym "load"
  compEnv <- compileExpr env envSpec symEnv
                         Nothing -- Return env to our custom func

  -- WORKAROUND #1
  -- Special case to support require-extension
  env' <- case envSpec of
               Atom a -> do
                   v <- getVar env a
                   case v of
                       LispEnv e -> return e
                       _ -> return env
               _ -> return env
  -- End special case

  compLoad <- compileLisp env' filename' symLoad Nothing
 
  -- Entry point
  f <- return $ [
    -- TODO: should do runtime error checking if something else
    --       besides a LispEnv is returned
    AstValue $ "  LispEnv e <- " ++ symEnv ++ " env (makeNullContinuation env) (Nil \"\") [] ",
    AstValue $ "  result <- " ++ symLoad ++ " e (makeNullContinuation e) (Nil \"\") Nothing",
    createAstCont copts "result" ""]
  -- Join compiled code together
  return $ [createAstFunc copts f] ++ compEnv ++ compLoad

compile env (List [Atom "load", filename]) copts = do -- TODO: allow filename from a var, support env optional arg
 -- TODO: error handling for string below
 String filename' <- LSC.evalLisp env filename
 Atom symEntryPt <- _gensym "load"
 result <- compileLisp env filename' symEntryPt Nothing
 return $ result ++ 
   [createAstFunc copts [
    AstValue $ "  result <- " ++ symEntryPt ++ " env (makeNullContinuation env) (Nil \"\") Nothing",
    createAstCont copts "result" ""]]

-- FUTURE: eventually it should be possible to evaluate the args instead of assuming
-- that they are all strings, but lets keep it simple for now
compile env (List [Atom "load-ffi", 
                        String moduleName, 
                        String externalFuncName, 
                        String internalFuncName]) copts = do
--  Atom symLoadFFI <- _gensym "loadFFI"

  -- Only append module again if it is not already in the list
  List l <- getNamespacedVar env 't' {-"internal"-} "imports"
  _ <- if not ((String moduleName) `elem` l)
          then setNamespacedVar env 't' {-"internal"-} "imports" $ List $ l ++ [String moduleName]
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
  expanded <- Language.Scheme.Macro.macroEval env lisp LSC.apply
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

compileSpecialFormEntryPoint :: String -> String -> CompOpts -> IOThrowsError HaskAST
compileSpecialFormEntryPoint formName formSym copts = do
 compileSpecialForm formName ("do " ++ formSym ++ " env cont (Nil \"\") []") copts

compileSpecialForm :: String -> String -> CompOpts -> IOThrowsError HaskAST
compileSpecialForm formName formCode copts = do
 f <- return $ [
       AstValue $ "  " ++ formCode]
 return $ createAstFunc copts f

-- |A wrapper for each special form that allows the form variable 
--  (EG: "if") to be redefined at compile time
compileSpecialFormBody env ast@(List (Atom fnc : args)) copts@(CompileOptions _ _ _ nextFunc) spForm = do
  isDefined <- liftIO $ isRecBound env fnc
  case isDefined of
    True -> mfunc env ast compileApply copts 
    False -> spForm nextFunc

-- Compile an intermediate expression (such as an arg to if) and 
-- call into the next continuation with it's value
compileExpr :: Env -> LispVal -> String -> Maybe String -> IOThrowsError [HaskAST]
compileExpr env expr symThisFunc fForNextExpr = do
  mcompile env expr (CompileOptions symThisFunc False False fForNextExpr) 

-- |Compile a function call
compileApply :: Env -> LispVal -> CompOpts -> IOThrowsError [HaskAST]
compileApply env (List (func : fparams)) copts@(CompileOptions coptsThis _ _ coptsNext) = do

--
-- TODO: it is probably possible to mix creating conts and not when there are func and non-func args.
--  

  primitive <- isPrim env func
  let literals = collectLiterals fparams 
      nonFunctionCalls = collectLiteralsAndVars fparams

  case (primitive, literals, nonFunctionCalls) of
     -- Primitive (non-I/O) function with literal args, 
     -- evaluate at compile time
     (Just primFunc, Just ls, _) -> do
       result <- LSC.apply 
        (makeNullContinuation env)
        primFunc
        ls

       return $ [createAstFunc copts [
         AstValue $ "  let result = " ++ (ast2Str result),
         createAstCont copts "result" ""]]

     -- Other function with literal args, no need to create a
     -- continuation chain. But this case may include I/O funcs and
     -- variables, so everything must be executed at runtime
     (_, _, Just ls) -> compileFuncLitArgs ls
     
     -- Any other function, do it the hard way...
     --
     -- Compile the function and each argument as a link in
     -- a chain of continuations.
     _ -> compileAllArgs func

 where 
  -- |Compile a function call that contains arguments that are not
  --  function calls executed at runtime.
  compileFuncLitArgs args = do
       -- Keep track of any variables since we need to do a
       -- 'getRtVar' lookup for each of them prior to apply
       let pack (Atom p : ps) strs vars i = do
             let varName = "v" ++ show i
             pack ps 
                  (strs ++ [varName]) 
                  (vars ++ [(p, varName)]) 
                  (i + 1)
           pack (p : ps) strs vars i = 
             pack ps 
                  (strs ++ [ast2Str p]) 
                  vars 
                  i
           pack [] strs vars _ = (strs, vars)
       let (paramStrs, vars) = pack args [] [] 0
       _compileFuncLitArgs func vars $ "[" ++ joinL paramStrs "," ++ "]"

  _compileFuncLitArgs func vars args = do
    Atom stubFunc <- _gensym "applyStubF"
    Atom nextFunc <- _gensym "applyNextF"

    c <- return $ 
      AstFunction coptsThis " env cont _ _ " [
        AstValue $ "  continueEval env (makeCPS env (makeCPS env cont " ++ nextFunc ++ ") " ++ stubFunc ++ ") $ Nil\"\""]  
    _comp <- mcompile env func $ CompileOptions stubFunc False False Nothing

    -- Haskell variables must be used to retrieve each atom from the env
    let varLines = 
          map (\ (rt, cp) -> 
                  AstValue $ "  " ++ cp ++ " <- getRTVar env \"" ++ rt ++ "\"")
              vars

    rest <- case coptsNext of
             Nothing -> return $ [
               AstFunction nextFunc
                " env cont value _ " $ varLines ++ [AstValue $ "  apply cont value " ++ args]]
             Just fnextExpr -> return $ [
               AstFunction nextFunc 
                " env cont value _ " $ varLines ++ [AstValue $ "  apply (makeCPS env cont " ++ fnextExpr ++ ") value " ++ args]]
    return $ [c] ++ _comp ++ rest

  -- |Compile function and args as a chain of continuations
  compileAllArgs func = do
    Atom stubFunc <- _gensym "applyStubF"
    Atom wrapperFunc <- _gensym "applyWrapper"
    Atom nextFunc <- _gensym "applyNextF"

    c <- return $ 
      AstFunction coptsThis " env cont _ _ " [
        AstValue $ "  continueEval env (makeCPS env (makeCPS env cont " ++ wrapperFunc ++ ") " ++ stubFunc ++ ") $ Nil\"\""]  
    -- Use wrapper to pass high-order function (func) as an argument to apply
    wrapper <- return $ 
      AstFunction wrapperFunc " env cont value _ " [
          AstValue $ "  continueEval env (makeCPSWArgs env cont " ++ nextFunc ++ " [value]) $ Nil \"\""]
    _comp <- mcompile env func $ CompileOptions stubFunc False False Nothing

    rest <- case fparams of
              [] -> do
                return [AstFunction 
                          nextFunc 
                          " env cont (Nil _) (Just (a:as)) "
                          [AstValue $ "  apply " ++ applyCont ++ " a as "],
                        AstFunction 
                          nextFunc 
                          " env cont value (Just (a:as)) " 
                          [AstValue $ "  apply " ++ applyCont ++ " a $ as ++ [value] "]]
              _ -> compileArgs nextFunc False fparams -- False since no value passed in this time
    return $ [c, wrapper ] ++ _comp ++ rest

  applyCont :: String
  applyCont = case coptsNext of
                Nothing -> "cont"
                Just fnextExpr -> "(makeCPS env cont " ++ fnextExpr ++ ")"

  -- |Compile each argument as its own continuation (lambda), and then
  --  call the function using "applyWrapper"
  compileArgs :: String -> Bool -> [LispVal] -> IOThrowsError [HaskAST]
  compileArgs thisFunc thisFuncUseValue args = do
    case args of
      (a:as) -> do
        let lastArg = null as
        Atom stubFunc <- _gensym "applyFirstArg" -- Call into compiled stub
        Atom nextFunc <- do
            case lastArg of
                True -> return $ Atom "applyWrapper" -- Use wrapper to call into 'apply'
                _ -> _gensym "applyNextArg" -- Next func argument to execute...
        _comp <- mcompile env a $ CompileOptions stubFunc False False Nothing

        -- Flag below means that the expression's value matters, add it to args
        f <- if thisFuncUseValue
                then return $ AstValue $ thisFunc ++ " env cont value (Just args) = do "
                else return $ AstValue $ thisFunc ++ " env cont _ (Just args) = do "
        c <- do
             let nextCont = case (lastArg, coptsNext) of
                                 (True, Just fnextExpr) -> "(makeCPS env cont " ++ fnextExpr ++ ")"
                                 _ -> "cont"
             if thisFuncUseValue
                then return $ AstValue $ "  continueEval env (makeCPS env (makeCPSWArgs env " ++ nextCont ++ " " ++
                                         nextFunc ++ " $ args ++ [value]) " ++ stubFunc ++ ") $ Nil\"\""  
                else return $ AstValue $ "  continueEval env (makeCPS env (makeCPSWArgs env " ++ nextCont ++ " " ++
                                         nextFunc ++ " args) " ++ stubFunc ++ ") $ Nil\"\""  

        rest <- case lastArg of
                     True -> return [] -- Using apply wrapper, so no more code
                     _ -> compileArgs nextFunc True as -- True indicates nextFunc needs to use value arg passed into it
        return $ [ f, c] ++ _comp ++ rest

      _ -> throwError $ TypeMismatch "nonempty list" $ List args

compileApply _ err _ = do
    throwError $ Default $ "compileApply - Unexpected argument: " ++ show err

-- |Determines if the given lispval is a primitive function
isPrim :: Env -> LispVal -> IOThrowsError (Maybe LispVal)
isPrim env (Atom func) = do
  val <- getVar env func >>= recDerefPtrs
  case val of
      p@(PrimitiveFunc _) -> return $ Just p
      _ -> return Nothing
isPrim _ p@(PrimitiveFunc _) = return $ Just p
isPrim _ _ = return Nothing

-- |Determine if the given list of expressions contains only literal identifiers
--  EG: 1, "2", etc. And return them if that is all that is found.
--
-- Atoms are a special case since they denote variables that will only be
-- available at runtime, so a flag is used to selectively include them.
--
_collectLiterals :: [LispVal] -> [LispVal] -> Bool -> (Maybe [LispVal])
_collectLiterals (List _ : _) _ _ = Nothing
_collectLiterals (Atom a : as) _ False = Nothing
_collectLiterals (a : as) nfs varFlag = _collectLiterals as (a : nfs) varFlag
_collectLiterals [] nfs _ = Just $ reverse nfs

-- Wrappers for the above function
collectLiterals, collectLiteralsAndVars :: [LispVal] -> (Maybe [LispVal])
collectLiteralsAndVars args = _collectLiterals args [] True
collectLiterals args = _collectLiterals args [] False
