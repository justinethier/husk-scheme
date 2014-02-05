{- |
Module      : Language.Scheme.Compiler.Libraries
Copyright   : Justin Ethier
Licence     : MIT (see LICENSE in the distribution)

Maintainer  : github.com/justinethier
Stability   : experimental
Portability : portable

This module contains support for compiling libraries of scheme code.

-}

module Language.Scheme.Compiler.Libraries
    ( 
      importAll
    )
where 
import Language.Scheme.Compiler.Types
import qualified Language.Scheme.Core as LSC 
    (evalLisp, findFileOrLib, meval, nullEnvWithImport)
import Language.Scheme.Primitives
import Language.Scheme.Types
import Language.Scheme.Variables
import Control.Monad.Error

-- |Import all given modules and generate code for them
importAll 
    :: Env 
    -- ^ Compilation environment
    -> Env 
    -- ^ Compilation meta environment, containing code from modules.scm
    -> [LispVal]
    -- ^ Modules to import
    -> CompLibOpts
    -- ^ Misc options required by compiler library functions
    -> CompOpts
    -- ^ Misc options required by compiler functions
    -> IOThrowsError [HaskAST]
    -- ^ Compiled code
importAll env metaEnv [m] lopts 
          copts@(CompileOptions _ _ _ _) = do
    _importAll env metaEnv m lopts copts
importAll env metaEnv (m : ms) lopts
          (CompileOptions thisFunc _ _ lastFunc) = do
    Atom nextFunc <- _gensym "importAll"
    c <- _importAll env metaEnv m lopts $ 
                    CompileOptions thisFunc False False (Just nextFunc)
    rest <- importAll env metaEnv ms lopts $
                      CompileOptions nextFunc False False lastFunc
    stub <- case rest of 
        [] -> return [createFunctionStub nextFunc lastFunc]
        _ -> return []
    return $ c ++ rest ++ stub
importAll _ _ [] _ _ = return []

_importAll :: Env
           -> Env
           -> LispVal
           -> CompLibOpts
           -> CompOpts
           -> ErrorT LispError IO [HaskAST]
_importAll env metaEnv m lopts copts = do
    -- Resolve import
    resolved <- LSC.evalLisp metaEnv $ 
         List [Atom  "resolve-import", List [Atom "quote", m]]
    case resolved of
        List (moduleName : imports) -> do
            importModule env metaEnv moduleName imports lopts copts
        DottedList [List moduleName] imports@(Bool False) -> do
            importModule env metaEnv (List moduleName) [imports] lopts copts
        err -> throwError $ TypeMismatch "module/import" err

-- |Import a single module
importModule :: Env
             -> Env
             -> LispVal
             -> [LispVal]
             -> CompLibOpts
             -> CompOpts
             -> ErrorT LispError IO [HaskAST]
importModule env metaEnv moduleName imports lopts 
             (CompileOptions thisFunc _ _ lastFunc) = do
    Atom symImport <- _gensym "importFnc"

    -- Load module
    code <- loadModule metaEnv moduleName lopts $ 
              CompileOptions thisFunc False False (Just symImport)
    
    -- Get module env, and import module env into env
    LispEnv modEnv <- LSC.evalLisp metaEnv $ 
       List [Atom "module-env", List [Atom "find-module", List [Atom "quote", moduleName]]]
    _ <- eval env $ List [Atom "%import", 
                          LispEnv env, 
                          LispEnv modEnv, 
                          List [Atom "quote", List imports], 
                          Bool False]
    
    importFunc <- return $ [
        -- fromEnv is a LispEnv passed in as the 'value' parameter.
        -- But the source of 'value' is different depending on the 
        -- context, so we call into this function to figure it out
        codeToGetFromEnv moduleName code,
        AstValue $ "  _ <- evalLisp env $ List [Atom \"%import\", LispEnv env, value, List [Atom \"quote\", " ++ 
                  (ast2Str $ List imports) ++ "], Bool False]",
        createAstCont (CompileOptions symImport False False lastFunc) "(value)" ""]
    
    -- thisFunc MUST be defined, so include a stub if there was nothing to import
    stub <- case code of
        [] -> return [createFunctionStub thisFunc (Just symImport)]
        _ -> return []

    return $ [createAstFunc (CompileOptions symImport True False lastFunc) 
                             importFunc] ++ code ++ stub
 where 
  --
  -- The import's "from" env can come from many places; this function
  -- figures that out and creates a new 'value' if necessary to send
  -- the proper value to %import in the above code
  --
  codeToGetFromEnv (List [Atom "scheme", Atom "r5rs"]) _ = do
     -- This is a hack to compile-in a full environment for the (scheme r5rs) import.
     --
     -- TODO: This really should be handled by the add-module! that is executed during
     --  module initialization, instead of having a special case here
     AstValue $ "  r5 <- liftIO $ r5rsEnv\n  let value = LispEnv r5"
  codeToGetFromEnv (List [Atom "scheme"]) _ = do
     -- hack to compile-in full env for the (scheme) import by r7rs
     AstValue $ "  r7 <- liftIO $ r7rsEnv\n  let value = LispEnv r7"
  codeToGetFromEnv (List [Atom "scheme", Atom "time", Atom "posix"]) _ = do
     AstValue $ "  e <- liftIO $ r7rsTimeEnv\n  let value = LispEnv e"
  codeToGetFromEnv name [] = do
     -- No code was generated because module was loaded previously, so retrieve
     -- it from runtime memory
     AstValue $ "  value <- evalLisp env $ List [Atom \"hash-table-ref\", Atom \"" ++ 
                moduleRuntimeVar ++ "\", List [Atom \"quote\", " ++ 
               (ast2Str name) ++ "]]" 

  codeToGetFromEnv _ _ = AstValue $ ""

-- | Load module into memory and generate compiled code
loadModule
    :: Env 
    -- ^ Compilation meta environment, containing code from modules.scm
    -> LispVal
    -- ^ Name of the module to load
    -> CompLibOpts
    -- ^ Misc options required by compiler library functions
    -> CompOpts
    -- ^ Misc options required by compiler functions
    -> IOThrowsError [HaskAST]
    -- ^ Compiled code, or an empty list if the module was already compiled
    --   and loaded into memory
loadModule metaEnv name lopts copts@(CompileOptions _ _ _ _) = do
    -- Get the module definition, or load it from file if necessary
    _mod' <- eval metaEnv $ List [Atom "find-module", List [Atom "quote", name]]
    case _mod' of
        Bool False -> return [] -- Even possible to reach this line?
        _ -> do
             _mod <- recDerefPtrs _mod'
             modEnv <- LSC.evalLisp metaEnv $ List [Atom "module-env", _mod]
             case modEnv of
                Bool False -> do
                {-------------------------------------------
                    Control flow for compiled code:

                     - create new env
                     - call into func directly to load it
                     - return new env and save to memory
                     - continue on to lastFunc
                --------------------------------------------}
                    Atom symStartLoadNewEnv <- _gensym "startLoadingNewEnvFnc"
                    Atom symEndLoadNewEnv <- _gensym "doneLoadingNewEnvFnc"

                    newEnvFunc <- return $ [
                        AstValue $ "  newEnv <- liftIO $ nullEnvWithImport",
                        AstValue $ "  _ <- defineVar newEnv \"" ++ moduleRuntimeVar ++ 
                                       "\" $ Pointer \"" ++ moduleRuntimeVar ++ "\" env",
                        AstValue $ "  _ <- " ++ symStartLoadNewEnv ++ 
                                   " newEnv (makeNullContinuation newEnv) (LispEnv env) (Just [])",
                        -- Save loaded module into runtime memory in case
                        -- it gets included somewhere else later on
                        AstValue $ "  _ <- evalLisp env $ List [Atom \"hash-table-set!\", Atom \"" ++ 
                                   moduleRuntimeVar ++ "\", List [Atom \"quote\", " ++
                                  (ast2Str name) ++ "], LispEnv newEnv]",
                        createAstCont copts "(LispEnv newEnv)" ""]
                    
                    -- Create new env for module, per eval-module
                    newEnv <- liftIO $ LSC.nullEnvWithImport
                    -- compile the module code, again per eval-module
                    result <- compileModule newEnv metaEnv name _mod lopts $
                        CompileOptions symStartLoadNewEnv False False (Just symEndLoadNewEnv)
                    modWEnv <- eval metaEnv $ List (Atom "module-env-set!" : _mod' : [LispEnv newEnv]) 
                    -- Above does not update *modules* correctly, so we del/add below
                    _ <- eval metaEnv $ List [Atom "delete-module!", List [Atom "quote", name]]
                    _ <- eval metaEnv $ List [Atom "add-module!", List [Atom "quote", name], modWEnv]

                    return $ 
                     [createAstFunc copts newEnvFunc] ++
                     [createAstFunc (CompileOptions symEndLoadNewEnv False False Nothing)
                                    [AstValue "  return $ Nil \"\""]] ++
                     result
                _ -> return [] --_mod

-- |Compile the given module, using metadata loaded into memory.
--  This code is based off of eval-module from the meta language.
compileModule :: Env
              -> Env
              -> LispVal
              -> LispVal
              -> CompLibOpts
              -> CompOpts
              -> ErrorT LispError IO [HaskAST]
compileModule env metaEnv name _mod lopts 
              (CompileOptions thisFunc _ _ lastFunc) = do
    -- TODO: set mod meta-data to avoid cyclic references
    -- see modules.scm for how this is done by the interpreter
    Atom afterImportsFnc <- _gensym "modAfterImport"
    --Atom afterDirFunc <- _gensym "modAfterDir"

    metaData <- LSC.evalLisp metaEnv $ 
                  List [Atom "module-meta-data", List [Atom "quote", _mod]]

    moduleImports <- cmpSubMod env metaEnv metaData lopts $ 
        CompileOptions thisFunc False False (Just afterImportsFnc)
    moduleDirectives <- cmpModExpr env metaEnv name metaData lopts $
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
createFunctionStub :: String -> Maybe String -> HaskAST
createFunctionStub thisFunc nextFunc = do
    createAstFunc (CompileOptions thisFunc True False Nothing)
                  [createAstCont (CompileOptions "" True False nextFunc) 
                                 "value" ""]

-- |Compile sub-modules. That is, modules that are imported by
--  another module in the (define-library) definition
cmpSubMod :: Env
          -> Env
          -> LispVal
          -> CompLibOpts
          -> CompOpts
          -> ErrorT LispError IO [HaskAST]
cmpSubMod env metaEnv (List ((List (Atom "import-immutable" : modules)) : ls)) 
    lopts copts = do
    -- Punt on this for now, although the meta-lang does the same thing
    cmpSubMod env metaEnv 
              (List ((List (Atom "import" : modules)) : ls)) 
              lopts copts
cmpSubMod env metaEnv (List ((List (Atom "import" : modules)) : ls)) lopts
    (CompileOptions thisFunc _ _ lastFunc) = do
    Atom nextFunc <- _gensym "cmpSubMod"
    code <- importAll env metaEnv modules lopts $ 
              CompileOptions thisFunc False False (Just nextFunc)
    rest <- cmpSubMod env metaEnv (List ls) lopts $ 
              CompileOptions nextFunc False False lastFunc 
    stub <- case rest of 
        [] -> return [createFunctionStub nextFunc lastFunc]
        _ -> return []
    return $ code ++ rest ++ stub
cmpSubMod env metaEnv (List (_ : ls)) lopts copts = 
    cmpSubMod env metaEnv (List ls) lopts copts
cmpSubMod _ _ _ _ (CompileOptions thisFunc _ _ lastFunc) = 
    return [createFunctionStub thisFunc lastFunc]

-- |Compile module directives (expressions) in a module definition
cmpModExpr :: Env
           -> Env
           -> LispVal
           -> LispVal
           -> CompLibOpts
           -> CompOpts
           -> ErrorT LispError IO [HaskAST]
cmpModExpr env metaEnv name (List ((List (Atom "include" : files)) : ls)) 
    lopts@(CompileLibraryOptions _ compileLisp)
    (CompileOptions thisFunc _ _ lastFunc) = do
    dir <- LSC.evalLisp metaEnv $ List [Atom "module-name-prefix", 
                                        List [Atom "quote", name]]
-- TODO: this pattern is common with the one below in "begin", 
--       should consolidate (or at least consider doing so)
    Atom nextFunc <- _gensym "includeNext"
    code <- includeAll env dir files compileInc lopts $ 
                       CompileOptions thisFunc False False (Just nextFunc)
    rest <- cmpModExpr env metaEnv name (List ls) lopts $ 
                CompileOptions nextFunc False False lastFunc
    stub <- case rest of 
        [] -> return [createFunctionStub nextFunc lastFunc]
        _ -> return []
    return $ code ++ rest ++ stub
 where 
  compileInc (String dir) (String filename) entry exit = do
    let path = dir ++ filename
    path' <- LSC.findFileOrLib path
    compileLisp env path' entry exit
  compileInc _ _ _ _ = throwError $ InternalError ""

cmpModExpr env metaEnv name (List ((List (Atom "include-ci" : code)) : ls)) lopts copts = do
    -- NOTE: per r7rs, ci should insert a fold-case directive. But husk does
    -- not support that, so just do a regular include for now
    cmpModExpr env metaEnv name
       (List ((List (Atom "include" : code)) : ls)) lopts copts
cmpModExpr env metaEnv name (List ((List (Atom "body" : code)) : ls)) lopts copts = do
    cmpModExpr env metaEnv name
       (List ((List (Atom "begin" : code)) : ls)) lopts copts

cmpModExpr env metaEnv name
       (List ((List (Atom "begin" : code')) : ls)) 
        lopts@(CompileLibraryOptions compileBlock _)
        (CompileOptions thisFunc _ _ lastFunc) = do
    Atom nextFunc <- _gensym "cmpSubModNext"
    code <- compileBlock thisFunc (Just nextFunc) env [] code'
    rest <- cmpModExpr env metaEnv name (List ls) lopts $ 
                CompileOptions nextFunc False False lastFunc
    stub <- case rest of 
        [] -> return [createFunctionStub nextFunc lastFunc]
        _ -> return []
    return $ code ++ rest ++ stub
cmpModExpr env metaEnv name (List (_ : ls)) lopts copts = 
    cmpModExpr env metaEnv name (List ls) lopts copts
cmpModExpr _ _ _ _ _ (CompileOptions thisFunc _ _ lastFunc) =
    return [createFunctionStub thisFunc lastFunc]

-- |Include one or more files for compilation
-- TODO: this pattern is used elsewhere (IE, importAll). could be generalized
includeAll :: forall t t1 t2 t3.
              t
              -> t3
              -> [t2]
              -> (t3
                  -> t2 -> String -> Maybe String -> ErrorT LispError IO [HaskAST])
              -> t1
              -> CompOpts
              -> ErrorT LispError IO [HaskAST]
includeAll _ dir [file] include _ --lopts
          (CompileOptions thisFunc _ _ lastFunc) = do
    include dir file thisFunc lastFunc
includeAll env dir (f : fs) include lopts
           (CompileOptions thisFunc _ _ lastFunc) = do
    Atom nextFunc <- _gensym "includeAll"
    c <- include dir f thisFunc (Just nextFunc)
    rest <- includeAll env dir fs include lopts $
                       CompileOptions nextFunc False False lastFunc
    stub <- case rest of 
        [] -> return [createFunctionStub nextFunc lastFunc]
        _ -> return []
    return $ c ++ rest ++ stub
includeAll _ _ [] _ _ _ = return []

-- |Like evalLisp, but preserve pointers in the output
eval :: Env -> LispVal -> IOThrowsError LispVal
eval env lisp = do
  LSC.meval env (makeNullContinuation env) lisp

