{- |
Module      : Language.Scheme.Compiler.Libraries
Copyright   : Justin Ethier
Licence     : MIT (see LICENSE in the distribution)

Maintainer  : github.com/justinethier
Stability   : experimental
Portability : portable

This module contains support for compiling libraries of code.

-}

module Language.Scheme.Compiler.Libraries
    ( 
      importTL
    )
where 
import Language.Scheme.Compiler.Types
import qualified Language.Scheme.Core as LSC 
    (apply, evalLisp, evalString, meval, nullEnvWithImport, 
     primitiveBindings, r5rsEnv, version) 
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

-- |Runtime reference to module data structure
moduleRuntimeVar = " modules "

-- |Top-level import
importTL env metaEnv [m] lopts copts@(CompileOptions thisFunc _ _ lastFunc) = do
    _importTL env metaEnv m lopts copts
importTL env metaEnv (m : ms) lopts
         copts@(CompileOptions thisFunc _ _ lastFunc) = do
    Atom nextFunc <- _gensym "importTL"
    c <- _importTL env metaEnv m lopts $ CompileOptions thisFunc False False (Just nextFunc)
    rest <- importTL env metaEnv ms lopts $ CompileOptions nextFunc False False lastFunc
    stub <- case rest of 
        [] -> return [createFunctionStub nextFunc lastFunc]
        _ -> return []
    return $ c ++ rest ++ stub
importTL _ _ [] _ _ = return []

_importTL env metaEnv m lopts copts = do
    -- Resolve import
--TODO: pattern match failure here when compiling test-list.scm - something's up
    resolved <- LSC.evalLisp metaEnv $ 
         List [Atom  "resolve-import", List [Atom "quote", m]]
    case resolved of
        List (moduleName : imports) -> do
            importModule env metaEnv moduleName imports lopts copts
        DottedList [List moduleName] imports@(Bool False) -> do
            importModule env metaEnv (List moduleName) [imports] lopts copts
        err -> throwError $ TypeMismatch "module/import" err

importModule env metaEnv moduleName imports lopts copts@(CompileOptions thisFunc _ _ lastFunc) = do
    Atom symImport <- _gensym "importFnc"

    -- Load module
    code <- loadModule metaEnv moduleName lopts $ 
              CompileOptions thisFunc False False (Just symImport)
    
    -- Get module env, and import module env into env
--    LispEnv modEnv <- (trace ("importModule, name = " ++ (show moduleName) ++ " code length = " ++ (show $ length code)) LSC.evalLisp) metaEnv $ 
    LispEnv modEnv <- LSC.evalLisp metaEnv $ 
       List [Atom "module-env", List [Atom "find-module", List [Atom "quote", moduleName]]]
    _ <- eval env $ List [Atom "%import", LispEnv env, LispEnv modEnv, List [Atom "quote", List imports], Bool False]
    
    importFunc <- return $ [
        -- fromEnv is a LispEnv passed in as the 'value' parameter

        -- This is a hack to compile-in a full environment for the (scheme r5rs) import.
        -- TODO: This really should be handled by the add-module! that is executed during
        --  module initialization, instead of having a special case here
        case moduleName of
            List [Atom "scheme", Atom "r5rs"] -> AstValue $ "  r5 <- liftIO $ r5rsEnv\n  let value = LispEnv r5"

            _ -> AstValue $ "",
        -- end hack

        AstValue $ "  _ <- evalLisp env $ List [Atom \"%import\", LispEnv env, value, List [Atom \"quote\", " ++ (ast2Str $ List imports) ++ "], Bool False]",
        createAstCont (CompileOptions symImport False False lastFunc) "(value)" ""]
    
    -- thisFunc MUST be defined, so include a stub if there was nothing to import
    stub <- case code of
        [] -> return [createFunctionStub thisFunc (Just symImport)]
        _ -> return []

    return $ [createAstFunc (CompileOptions symImport True False lastFunc) 
                             importFunc] ++ 
             code ++ stub

-- TODO: should return module vector?
-- but maybe not, because our eval-module will need to compile the
-- module, so we may need to do something special like return a 
-- tuple containing both pieces of information. Or do we even need
-- the module vector, since we can just look it up by name in importTL
--
loadModule metaEnv name lopts copts@(CompileOptions thisFunc _ _ lastFunc) = do
    -- Get the module definition, or load it from file if necessary
    mod' <- eval metaEnv $ List [Atom "find-module", List [Atom "quote", name]]
--    case (trace ("loadModule " ++ (show name) ++ " = " ++ (show mod')) mod') of
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
                        AstValue $ "  newEnv <- liftIO $ nullEnvWithImport",
                        AstValue $ "  mods <- getVar env \"" ++ moduleRuntimeVar ++ "\"",
                        AstValue $ "  _ <- defineVar newEnv \"" ++ moduleRuntimeVar ++ "\" mods",
                        AstValue $ "  _ <- " ++ symStartLoadNewEnv ++ " newEnv (makeNullContinuation newEnv) (LispEnv env) []",
-- TODO: need to store env in runtime memory with key of 'name'
--       that way it is available later if another module wants to import it
                        AstValue $ "  _ <- evalLisp env $ List [Atom \"hash-table-set!\", Atom \"" ++ moduleRuntimeVar ++ "\", List [Atom \"quote\", " ++ (ast2Str name) ++ "], LispEnv newEnv]",
                        createAstCont copts "(LispEnv newEnv)" ""]
                    
                    -- Create new env for module, per eval-module
                    newEnv <- liftIO $ LSC.nullEnvWithImport
                    -- compile the module code, again per eval-module
                    result <- compileModule newEnv metaEnv name mod lopts $
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
compileModule env metaEnv name mod lopts copts@(CompileOptions thisFunc _ _ lastFunc) = do
    -- TODO: set mod meta-data to avoid cyclic references
    Atom afterImportsFnc <- _gensym "modAfterImport"
    Atom afterDirFunc <- _gensym "modAfterDir"

    metaData <- LSC.evalLisp metaEnv $ List [Atom "module-meta-data", List [Atom "quote", mod]]

    moduleImports <- csd env metaEnv metaData lopts $ 
        CompileOptions thisFunc False False (Just afterImportsFnc)
    moduleDirectives <- cmd env metaEnv metaData lopts $
        moduleDirsCopts moduleImports afterImportsFnc

    return $ moduleImports ++ 
             moduleDirectives ++ 
            (moduleStub moduleImports moduleDirectives afterImportsFnc)
 where 
  moduleDirsCopts modImps afterImportsFnc = do
-- if moduleImports is [] then use same copts for moduleDir
-- else, use copts (afterImportsFunc, lastFunc)
    case modImps of
        [] -> CompileOptions thisFunc False False (Just afterImportsFnc)
        _ -> CompileOptions afterImportsFnc False False lastFunc
  moduleStub modImps modDir afterImportsFnc = do
-- if moduleDir == [] and moduleimports == [] then add stub (this, last)
-- else if modDir == [] then addstub (afterimports, last)
-- else, no stub required
    case (modImps, modDir) of
        ([], []) -> [createFunctionStub thisFunc lastFunc]
        ([], _) -> [createFunctionStub afterImportsFnc lastFunc]
        (_, []) -> [createFunctionStub afterImportsFnc lastFunc]
        _ -> [] -- Both have code, no stub needed

-- Helper function to create an empty continuation
--
-- TODO: ideally stubs would not be necessary,
--       should refactor out at some point
--createFunctionStub :: String -> HaskAST
createFunctionStub thisFunc nextFunc = do
    createAstFunc (CompileOptions thisFunc True False Nothing)
                  [createAstCont (CompileOptions "" True False nextFunc) 
                                 "value" ""]

-- |Compile sub-modules. That is, modules that are imported by
--  another module in the (define-library) definition
-- TODO: consider consolidating with common code in cmd below
--csd env metaEnv (List (
csd env metaEnv (List ((List (Atom "import-immutable" : modules)) : ls)) 
    lopts copts = do
    -- Punt on this for now, although the meta-lang does the same thing
    csd env metaEnv (List ((List (Atom "import" : modules)) : ls)) lopts copts
csd env metaEnv (List ((List (Atom "import" : modules)) : ls)) lopts
    copts@(CompileOptions thisFunc _ _ lastFunc) = do
    Atom nextFunc <- _gensym "csdNext"
    code <- importTL env metaEnv modules lopts $ CompileOptions thisFunc False False (Just nextFunc)
    rest <- csd env metaEnv (List ls) lopts $ CompileOptions nextFunc False False lastFunc 
    stub <- case rest of 
        [] -> return [createFunctionStub nextFunc lastFunc]
        _ -> return []
    return $ code ++ rest ++ stub
csd env metaEnv (List (_ : ls)) lopts copts = 
    csd env metaEnv (List ls) lopts copts
csd _ _ _ _ (CompileOptions thisFunc _ _ lastFunc) = 
    -- TODO: not good enough, need to return a stub w/lastFunc
    -- does it just have to be named thisfunc, and call into lastfunc?

    return []

-- Compile module directive, rename it later (TODO)
-- TODO: cmd env metaEnv (List ((List (Atom "include" : code)) : ls)) copts = do
-- TBD
-- TODO: cmd env metaEnv (List ((List (Atom "include-ci" : code)) : ls)) copts = do
-- TBD
cmd env metaEnv (List ((List (Atom "body" : code)) : ls)) lopts copts = do
    cmd env metaEnv (List ((List (Atom "begin" : code)) : ls)) lopts copts
cmd env metaEnv 
       (List ((List (Atom "begin" : code)) : ls)) 
        lopts@(CompileLibraryOptions compileBlock)
        copts@(CompileOptions thisFunc _ _ lastFunc) = do
    Atom nextFunc <- _gensym "csdNext"
    code <- compileBlock thisFunc (Just nextFunc) env [] code
    rest <- cmd env metaEnv (List ls) lopts $ CompileOptions nextFunc False False lastFunc
    stub <- case rest of 
        [] -> return [createFunctionStub nextFunc lastFunc]
        _ -> return []
    return $ code ++ rest ++ stub
cmd env metaEnv (List (_ : ls)) lopts copts = 
    cmd env metaEnv (List ls) lopts copts
cmd _ _ _ _ copts = return []

-- |Like evalLisp, but preserve pointers in the output
eval :: Env -> LispVal -> IOThrowsError LispVal
eval env lisp = do
  LSC.meval env (makeNullContinuation env) lisp

