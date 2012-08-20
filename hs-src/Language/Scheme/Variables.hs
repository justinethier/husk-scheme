{- |
Module      : Language.Scheme.Variables
Copyright   : Justin Ethier
Licence     : MIT (see LICENSE in the distribution)

Maintainer  : github.com/justinethier
Stability   : experimental
Portability : portable

This module contains code for working with Scheme variables,
and the environments that contain them.

-}

module Language.Scheme.Variables 
    (
    -- * Environments
      printEnv
    , copyEnv
    , extendEnv
    , findNamespacedEnv
    -- * Getters
    , getVar
    , getNamespacedVar 
    -- * Setters
    , defineVar
    , setVar
    , setNamespacedVar
    , setVarByAddress 
    , defineNamespacedVar
    -- * Predicates
    , isBound
    , isRecBound
    , isNamespacedBound
    , isNamespacedRecBound 
    ) where
import Language.Scheme.Types
import Control.Monad.Error
import Data.IORef
import qualified Data.Map
import Debug.Trace

{- Experimental code:
-- From: http://rafaelbarreto.com/2011/08/21/comparing-objects-by-memory-location-in-haskell/
-}
import Foreign
isMemoryEquivalent :: a -> a -> IO Bool
isMemoryEquivalent obj1 obj2 = do
  obj1Ptr <- newStablePtr obj1
  obj2Ptr <- newStablePtr obj2
  let result = obj1Ptr == obj2Ptr
  freeStablePtr obj1Ptr
  freeStablePtr obj2Ptr
  return result
{-
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
printEnv :: Env         -- ^Environment
         -> IO String   -- ^Contents of the env as a string
printEnv env = do
  binds <- liftIO $ readIORef $ bindings env
  l <- mapM showVar $ Data.Map.toList binds 
  return $ unlines l
 where 
  showVar ((_, name), val) = do
    v <- liftIO $ readIORef val
    return $ name ++ ": " ++ show v

-- |Create a deep copy of an environment
copyEnv :: Env      -- ^ Source environment
        -> IO Env   -- ^ A copy of the source environment
copyEnv env = do
  binds <- liftIO $ readIORef $ bindings env
--  bindingList <- mapM addBinding binds >>= newIORef
  bindingListT <- mapM addBinding $ Data.Map.toList binds
  bindingList <- newIORef $ Data.Map.fromList bindingListT
  return $ Environment (parentEnv env) (outerEnv env) bindingList
 where addBinding ((namespace, name), val) = do 
         x <- liftIO $ readIORef val
         ref <- newIORef x
         return ((namespace, name), ref)

-- |Extend given environment by binding a series of values to a new environment.

-- TODO: should be able to use Data.Map.fromList to ease construction of new Env
extendEnv :: Env -- ^ Environment 
          -> Maybe Env -- ^ Environment of use
          -> [((String, String), LispVal)] -- ^ Extensions to the environment
          -> IO Env -- ^ Extended environment
extendEnv envRef useEnv abindings = do 
  bindinglistT <- (mapM addBinding abindings)
  bindinglist <- newIORef $ Data.Map.fromList bindinglistT
  return $ Environment (Just envRef) useEnv bindinglist
 where addBinding ((namespace, name), val) = do
         ref <- newIORef val
         return ((namespace, name), ref)

-- |Recursively search environments to find one that contains the given variable.
findNamespacedEnv 
    :: Env      -- ^Environment to begin the search; 
                --  parent env's will be searched as well.
    -> String   -- ^Namespace
    -> String   -- ^Variable
    -> IO (Maybe Env) -- ^Environment, or Nothing if there was no match.
findNamespacedEnv envRef namespace var = do
  found <- liftIO $ isNamespacedBound envRef namespace var
  if found
     then return (Just envRef)
     else case parentEnv envRef of
               (Just par) -> findNamespacedEnv par namespace var
               Nothing -> return Nothing

-- |Determine if a variable is bound in the default namespace
isBound :: Env      -- ^ Environment
        -> String   -- ^ Variable
        -> IO Bool  -- ^ True if the variable is bound
isBound envRef var = isNamespacedBound envRef varNamespace var

-- |Determine if a variable is bound in the default namespace, 
--  in this environment or one of its parents.
isRecBound :: Env      -- ^ Environment
           -> String   -- ^ Variable
           -> IO Bool  -- ^ True if the variable is bound
isRecBound envRef var = isNamespacedRecBound envRef varNamespace var

-- |Determine if a variable is bound in a given namespace
isNamespacedBound 
    :: Env      -- ^ Environment
    -> String   -- ^ Namespace
    -> String   -- ^ Variable
    -> IO Bool  -- ^ True if the variable is bound
isNamespacedBound envRef namespace var = 
    (readIORef $ bindings envRef) >>= return . Data.Map.member (namespace, var)

-- |Determine if a variable is bound in a given namespace
--  or a parent of the given environment.
isNamespacedRecBound 
    :: Env      -- ^ Environment
    -> String   -- ^ Namespace
    -> String   -- ^ Variable
    -> IO Bool  -- ^ True if the variable is bound
isNamespacedRecBound envRef namespace var = do
  env <- findNamespacedEnv envRef namespace var
  case env of
    (Just e) -> isNamespacedBound e namespace var
    Nothing -> return False

-- |Retrieve the value of a variable defined in the default namespace
getVar :: Env       -- ^ Environment
       -> String    -- ^ Variable
       -> IOThrowsError LispVal -- ^ Contents of the variable
getVar envRef var = getNamespacedVar envRef varNamespace var

-- |Retrieve the value of a variable defined in a given namespace
getNamespacedVar :: Env     -- ^ Environment
                 -> String  -- ^ Namespace
                 -> String  -- ^ Variable
                 -> IOThrowsError LispVal -- ^ Contents of the variable
getNamespacedVar envRef
                 namespace
                 var = do binds <- liftIO $ readIORef $ bindings envRef
                          case Data.Map.lookup (namespace, var) binds of
                            (Just a) -> liftIO $ readIORef a
                            Nothing -> case parentEnv envRef of
                                         (Just par) -> getNamespacedVar par namespace var
                                         Nothing -> (throwError $ UnboundVar "Getting an unbound variable" var)


-- |Set a variable in the default namespace
setVar
    :: Env      -- ^ Environment
    -> String   -- ^ Variable
    -> LispVal  -- ^ Value
    -> IOThrowsError LispVal -- ^ Value
setVar envRef var value = setNamespacedVar envRef varNamespace var value

-- |Bind a variable in the default namespace
defineVar
    :: Env      -- ^ Environment
    -> String   -- ^ Variable
    -> LispVal  -- ^ Value
    -> IOThrowsError LispVal -- ^ Value
defineVar envRef var value = defineNamespacedVar envRef varNamespace var value

-- |Set a variable in a given namespace
setNamespacedVar 
    :: Env      -- ^ Environment 
    -> String   -- ^ Namespace
    -> String   -- ^ Variable
    -> LispVal  -- ^ Value
    -> IOThrowsError LispVal   -- ^ Value
setNamespacedVar envRef
                 namespace
                 var value = do env <- liftIO $ readIORef $ bindings envRef
                                case Data.Map.lookup (namespace, var) env of
                                  (Just a) -> do -- vprime <- liftIO $ readIORef a
                                                 liftIO $ writeIORef a value
                                                 return value
                                  Nothing -> case parentEnv envRef of
                                              (Just par) -> setNamespacedVar par namespace var value
                                              Nothing -> throwError $ UnboundVar "Setting an unbound variable: " var


-- |Recursively search env for a memory location,
--  and if found update the var at that location with the given one.
setVarByAddress 
    :: Env      -- ^ Environment 
    -> Integer  -- ^ Memory address
    -> LispVal  -- ^ Value
    -> IOThrowsError LispVal   -- ^ Value
setVarByAddress envRef mloc value = 
  setNamespacedVarByAddress envRef varNamespace mloc value

{-
TODO: high-level goals
Want to get a list of all env's that could reference mloc, so we have a chance to update it
in order to get the list, need to take:
  current env
  search parent env tree
  search outer env tree

both searches are recursive
TBD: how to accumulate found env's to make search as efficient as possible?
-}
_combineEnvs :: Env -> IO [Env]
_combineEnvs env = do
 let scope = _combineParentEnvs env []
 outerScope <- case outerEnv env of
   Just outer -> _combineOuterEnvs outer []
   Nothing -> return []
 return $ scope ++ outerScope

_combineParentEnvs :: Env -> [Env] -> [Env]
_combineParentEnvs env envs = do
  case parentEnv env of
    Just par -> _combineParentEnvs par (env : envs)
    Nothing -> (env : envs)

_combineOuterEnvs :: Env -> [Env] -> IO [Env]
_combineOuterEnvs env envs = do
  found <- _envInEnvs env envs
  case found of
    False -> do
        _outer <- case (outerEnv env) of
                    Just outer -> _combineOuterEnvs outer []
                    Nothing -> return []
        _par <- case (parentEnv env) of
                    Just par -> _combineOuterEnvs par []
                    Nothing -> return []
        return $ (env : envs) ++ _par ++ _outer 
    True -> return envs 

_envInEnvs :: Env -> [Env] -> IO Bool
_envInEnvs env (e : es) = do
--  return False
-- TODO:
  envStr <- printEnv env
  eStr <- printEnv e
  env' <- liftIO $ readIORef $ bindings env
  e' <- liftIO $ readIORef $ bindings e
--  -- TODO: comparison must be more advanced
--  if (((length $ Data.Map.assocs env') > 10) && (length $ Data.Map.assocs env') == (length $ Data.Map.assocs e'))
--  cmp <- isMemoryEquivalent env' e'
--  case cmp of
--     True -> return True
--     _ -> _envInEnvs env es 
  if (bindings env) == (bindings e)
     then return (trace ("Eq: \n 1> " ++ envStr ++ "\n 2> " ++ eStr ++ "\nDONE") True)
     else _envInEnvs env es
_envInEnvs _ [] = return False


-- Private version of the above that is called recursively from within this module.
setNamespacedVarByAddress 
    :: Env      -- ^ Environment 
    -> String   -- ^ Namespace
    -> Integer  -- ^ Memory address
    -> LispVal  -- ^ Value
    -> IOThrowsError LispVal   -- ^ Value
setNamespacedVarByAddress envRef namespace mloc value = do
    envRefs <- liftIO $ _combineEnvs envRef
-- TODO: try this, the lengths are unacceptable with 'make test'
-- we need to do better...
    _ <- lift $ update $ (trace ("len = " ++ (show $ length envRefs)) envRefs)
--    _ <- lift $ update envRefs
    return value
 where 
  -- Check for updates in the list of env's
  update :: [Env] -> IO ()
  update (e : envs) = do

--    testing <- printEnv e
--    env <- liftIO $ readIORef $ bindings (trace ("=> " ++ testing) e)

    env <- liftIO $ readIORef $ bindings e
    _ <- setLoc $ Data.Map.assocs env
    update envs
  update [] = return ()

  -- Change IO references at mloc's location
  setLoc :: [((String, String), IORef LispVal)] -> IO ()
  setLoc [] = return ()
  setLoc (v@((vnamespace, vname), a) : vs) 
   | vnamespace == namespace = do
     -- Check var in namespace, and change if at requested mem location
     var <- liftIO $ readIORef a
     if checkAddress var mloc
        then do
          liftIO $ writeIORef a value
--          liftIO $ writeIORef a (trace ("setLoc: " ++ vname ++ "[" ++ (show value) ++ "]") value)
          -- keep checking
          setLoc vs
        else setLoc vs
   | otherwise = setLoc vs

-- |Bind a variable in the given namespace
defineNamespacedVar
    :: Env      -- ^ Environment 
    -> String   -- ^ Namespace
    -> String   -- ^ Variable
    -> LispVal  -- ^ Value
    -> IOThrowsError LispVal   -- ^ Value
defineNamespacedVar envRef
                    namespace
                    var value = do
  alreadyDefined <- liftIO $ isNamespacedBound envRef namespace var
  if alreadyDefined
    then setNamespacedVar envRef namespace var value >> return value
    else liftIO $ do
       valueRef <- newIORef value
       env <- readIORef $ bindings envRef
       writeIORef (bindings envRef) (Data.Map.insert (namespace, var) valueRef env) --  (((namespace, var), valueRef) : env)
       return value
