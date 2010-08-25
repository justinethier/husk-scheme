{-
 - skim-scheme
 - Variables
 -
 - This file contains code for working with Scheme variables
 -
 - @author Justin Ethier
 -
 - -}
module Skim.Variables where
import Skim.Types
import Control.Monad
import Control.Monad.Error
import Data.IORef

-- Determine if a variable is bound in the "variable" namespace
isBound :: Env -> String -> IO Bool
isBound envRef var = isNamespacedBound envRef varNamespace var

-- Determine if a variable is bound in the given namespace
isNamespacedBound :: Env -> String -> String -> IO Bool
isNamespacedBound envRef namespace var = readIORef envRef >>= return . maybe False (const True) . lookup (namespace, var)

getVar :: Env -> String -> IOThrowsError LispVal
getVar envRef var = getNamespacedVar envRef varNamespace var

getNamespacedVar :: Env -> String -> String -> IOThrowsError LispVal
getNamespacedVar envRef
                 namespace
                 var = do env <- liftIO $ readIORef envRef
                          maybe (throwError $ UnboundVar "Getting an unbound variable" var)
                                (liftIO . readIORef)
                                (lookup (namespace, var) env)

setVar, defineVar :: Env -> String -> LispVal -> IOThrowsError LispVal
setVar envRef var value = setNamespacedVar envRef varNamespace var value
defineVar envRef var value = defineNamespacedVar envRef varNamespace var value

setNamespacedVar :: Env -> String -> String -> LispVal -> IOThrowsError LispVal
setNamespacedVar envRef 
                 namespace
                 var value = do env <- liftIO $ readIORef envRef
                                maybe (throwError $ UnboundVar "Setting an unbound variable: " var)
                                      (liftIO . (flip writeIORef value))
                                      (lookup (namespace, var) env)
                                return value

defineNamespacedVar :: Env -> String -> String -> LispVal -> IOThrowsError LispVal
defineNamespacedVar envRef 
                    namespace 
                    var value = do
  alreadyDefined <- liftIO $ isNamespacedBound envRef namespace var
  if alreadyDefined
    then setNamespacedVar envRef namespace var value >> return value
    else liftIO $ do
       valueRef <- newIORef value
       env <- readIORef envRef
       writeIORef envRef (((namespace, var), valueRef) : env)
       return value

bindVars :: Env -> [((String, String), LispVal)] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
  where extendEnv bindings env = liftM  (++ env) (mapM addBinding bindings)
        addBinding (var, value) = do ref <- newIORef value
                                     return (var, ref)

