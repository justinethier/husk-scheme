{-
 - husk scheme
 - Variables
 -
 - This file contains code for working with Scheme variables
 -
 - @author Justin Ethier
 -
 - -}
module Scheme.Variables where
import Scheme.Types
import Control.Monad
import Control.Monad.Error
import Data.IORef

-- |Extend given environment by binding a series of values to a new environment.
extendEnv :: Env -> [((String, String), LispVal)] -> IO Env
extendEnv envRef bindings = do bindinglist <- mapM (\((namespace, name), val) ->
                                                    do ref <- newIORef val
                                                       return ((namespace, name), ref)) bindings
                                              >>= newIORef
                               return $ Environment (Just envRef) bindinglist

{-
-- Old implementation, left for the moment for reference purposes only:
--
-- |Bind a series of values to the given environment.
--
-- Input is of form: @(namespaceName, variableName), variableValue@
bindVars :: Env -> [((String, String), LispVal)] -> IO Env
bindVars envRef abindings = (readIORef $ bindings envRef) >>= myExtendEnv abindings >>= newIORef
  where myExtendEnv bindings env = liftM  (++ env) (mapM addBinding bindings)
        addBinding (var, value) = do ref <- newIORef value
                                     return (var, ref)
-}

-- |Determine if a variable is bound in the default namespace
isBound :: Env -> String -> IO Bool
isBound envRef var = isNamespacedBound envRef varNamespace var

-- |Determine if a variable is bound in a given namespace
isNamespacedBound :: Env -> String -> String -> IO Bool
isNamespacedBound envRef namespace var = (readIORef $ bindings envRef) >>= return . maybe False (const True) . lookup (namespace, var)

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
                                  (Just a) -> do vprime <- liftIO $ readIORef a
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
