{- |
Module      : Language.Scheme.Variables
Copyright   : Justin Ethier
Licence     : MIT (see LICENSE in the distribution)

Maintainer  : github.com/justinethier
Stability   : experimental
Portability : portable

husk scheme interpreter

A lightweight dialect of R5RS scheme.

This module contains code for working with Scheme variables.

-}

module Language.Scheme.Variables where
import Language.Scheme.Types
import Control.Monad.Error
import Data.IORef

{- Experimental code:
-- From: http://rafaelbarreto.com/2011/08/21/comparing-objects-by-memory-location-in-haskell/
import Foreign
isMemoryEquivalent :: a -> a -> IO Bool
isMemoryEquivalent obj1 obj2 = do
  obj1Ptr <- newStablePtr obj1
  obj2Ptr <- newStablePtr obj2
  let result = obj1Ptr == obj2Ptr
  freeStablePtr obj1Ptr
  freeStablePtr obj2Ptr
  return result

-- Using above, search an env for a variable definition, but stop if the upperEnv is
-- reached before the variable
isNamespacedRecBoundWUpper :: Env -> Env -> String -> String -> IO Bool
isNamespacedRecBoundWUpper upperEnvRef envRef namespace var = do 
  areEnvsEqual <- liftIO $ isMemoryEquivalent upperEnvRef envRef
  if areEnvsEqual
     then return False
     else do
         found <- liftIO $ isNamespacedBound envRef namespace var
         if found
            then return True 
            else case parentEnv envRef of
                      (Just par) -> isNamespacedRecBoundWUpper upperEnvRef par namespace var
                      Nothing -> return False -- Var never found
-}

-- |Show the contents of an environment
printEnv :: Env -> IO String
printEnv env = do
  binds <- liftIO $ readIORef $ bindings env
  l <- mapM showVar binds 
  return $ unlines l
 where 
  showVar ((_, name), val) = do
    v <- liftIO $ readIORef val
    return $ name ++ ": " ++ show v

-- |Create a deep copy of an environment
copyEnv :: Env -> IO Env
copyEnv env = do
  binds <- liftIO $ readIORef $ bindings env
  bindingList <- mapM addBinding binds >>= newIORef
  return $ Environment (parentEnv env) bindingList -- TODO: recursively create a copy of parent also?
 where addBinding ((namespace, name), val) = do --ref <- newIORef $ liftIO $ readIORef val
                                                x <- liftIO $ readIORef val
                                                ref <- newIORef x
                                                return ((namespace, name), ref)

-- |Extend given environment by binding a series of values to a new environment.
extendEnv :: Env -> [((String, String), LispVal)] -> IO Env
extendEnv envRef abindings = do bindinglist <- mapM addBinding abindings >>= newIORef
                                return $ Environment (Just envRef) bindinglist
 where addBinding ((namespace, name), val) = do ref <- newIORef val
                                                return ((namespace, name), ref)

-- Recursively search environments to find one that contains var
findNamespacedEnv :: Env -> String -> String -> IO (Maybe Env)
findNamespacedEnv envRef namespace var = do
  found <- liftIO $ isNamespacedBound envRef namespace var
  if found
     then return (Just envRef)
     else case parentEnv envRef of
               (Just par) -> findNamespacedEnv par namespace var
               Nothing -> return Nothing

-- |Determine if a variable is bound in the default namespace
isBound :: Env -> String -> IO Bool
isBound envRef var = isNamespacedBound envRef varNamespace var

-- |Determine if a variable is bound in the default namespace, in this env or a parent
isRecBound :: Env -> String -> IO Bool
isRecBound envRef var = isNamespacedRecBound envRef varNamespace var

-- |Determine if a variable is bound in a given namespace
isNamespacedBound :: Env -> String -> String -> IO Bool
isNamespacedBound envRef namespace var = (readIORef $ bindings envRef) >>= return . maybe False (const True) . lookup (namespace, var)

-- TODO: should isNamespacedBound be replaced with this? Probably, but one step at a time...
isNamespacedRecBound :: Env -> String -> String -> IO Bool
isNamespacedRecBound envRef namespace var = do
  env <- findNamespacedEnv envRef namespace var
  case env of
    (Just e) -> isNamespacedBound e namespace var
    Nothing -> return False

-- |Retrieve the value of a variable defined in the default namespace
getVar :: Env -> String -> IOThrowsError LispVal
getVar envRef var = getNamespacedVar envRef varNamespace var

-- |Retrieve the value of a variable defined in a given namespace
getNamespacedVar :: Env -> String -> String -> IOThrowsError LispVal
getNamespacedVar envRef
                 namespace
                 var = do binds <- liftIO $ readIORef $ bindings envRef
                          case lookup (namespace, var) binds of
                            (Just a) -> liftIO $ readIORef a
                            Nothing -> case parentEnv envRef of
                                         (Just par) -> getNamespacedVar par namespace var
                                         Nothing -> (throwError $ UnboundVar "Getting an unbound variable" var)


-- |Set a variable in the default namespace
setVar, defineVar :: Env -> String -> LispVal -> IOThrowsError LispVal
setVar envRef var value = setNamespacedVar envRef varNamespace var value

-- ^Bind a variable in the default namespace
defineVar envRef var value = defineNamespacedVar envRef varNamespace var value

-- |Set a variable in a given namespace
setNamespacedVar :: Env -> String -> String -> LispVal -> IOThrowsError LispVal
setNamespacedVar envRef
                 namespace
                 var value = do env <- liftIO $ readIORef $ bindings envRef
                                case lookup (namespace, var) env of
                                  (Just a) -> do -- vprime <- liftIO $ readIORef a
                                                 liftIO $ writeIORef a value
                                                 return value
                                  Nothing -> case parentEnv envRef of
                                              (Just par) -> setNamespacedVar par namespace var value
                                              Nothing -> throwError $ UnboundVar "Setting an unbound variable: " var

-- |Bind a variable in the given namespace
defineNamespacedVar :: Env -> String -> String -> LispVal -> IOThrowsError LispVal
defineNamespacedVar envRef
                    namespace
                    var value = do
  alreadyDefined <- liftIO $ isNamespacedBound envRef namespace var
  if alreadyDefined
    then setNamespacedVar envRef namespace var value >> return value
    else liftIO $ do
       valueRef <- newIORef value
       env <- readIORef $ bindings envRef
       writeIORef (bindings envRef) (((namespace, var), valueRef) : env)
       return value
