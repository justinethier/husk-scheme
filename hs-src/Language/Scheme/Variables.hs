{-# LANGUAGE CPP #-}

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
    , recPrintEnv
    , recExportsFromEnv 
    , exportsFromEnv 
    , copyEnv
    , extendEnv
    , importEnv
    , topmostEnv
    , nullEnvWithParent
    , findNamespacedEnv
    , macroNamespace
    , varNamespace 
    -- * Getters
    , getVar
    , getVar'
    , getNamespacedVar 
    , getNamespacedVar' 
    , getNamespacedRef 
    -- * Setters
    , defineVar
    , defineNamespacedVar
    , setVar
    , setNamespacedVar
    , updateObject 
    , updateNamespacedObject 
    -- * Predicates
    , isBound
    , isRecBound
    , isNamespacedRecBound 
    -- * Pointers
    , derefPtr
--    , derefPtrs
    , recDerefPtrs
    , safeRecDerefPtrs
    , recDerefToFnc
    ) where
import Language.Scheme.Types
import Control.Monad.Error
import Data.Array
import Data.IORef
import qualified Data.Map
-- import Debug.Trace

-- |Internal namespace for macros
macroNamespace :: Char
macroNamespace = 'm'

-- |Internal namespace for variables
varNamespace :: Char
varNamespace = 'v'

-- Experimental code:
-- From: http://rafaelbarreto.com/2011/08/21/comparing-objects-by-memory-location-in-haskell/
--
-- import Foreign
-- isMemoryEquivalent :: a -> a -> IO Bool
-- isMemoryEquivalent obj1 obj2 = do
--   obj1Ptr <- newStablePtr obj1
--   obj2Ptr <- newStablePtr obj2
--   let result = obj1Ptr == obj2Ptr
--   freeStablePtr obj1Ptr
--   freeStablePtr obj2Ptr
--   return result
-- 
-- -- Using above, search an env for a variable definition, but stop if the upperEnv is
-- -- reached before the variable
-- isNamespacedRecBoundWUpper :: Env -> Env -> String -> String -> IO Bool
-- isNamespacedRecBoundWUpper upperEnvRef envRef namespace var = do 
--   areEnvsEqual <- liftIO $ isMemoryEquivalent upperEnvRef envRef
--   if areEnvsEqual
--      then return False
--      else do
--          found <- liftIO $ isNamespacedBound envRef namespace var
--          if found
--             then return True 
--             else case parentEnv envRef of
--                       (Just par) -> isNamespacedRecBoundWUpper upperEnvRef par namespace var
--                       Nothing -> return False -- Var never found
--

-- |Create a variable's name in an environment using given arguments
getVarName :: Char -> String -> String
getVarName namespace name = (namespace : ('_' : name))

-- |Show the contents of an environment
printEnv :: Env         -- ^Environment
         -> IO String   -- ^Contents of the env as a string
printEnv env = do
  binds <- liftIO $ readIORef $ bindings env
  l <- mapM showVar $ Data.Map.toList binds 
  return $ unlines l
 where 
  showVar (name, val) = do
    v <- liftIO $ readIORef val
    return $ "[" ++ name ++ "]" ++ ": " ++ show v

-- |Recursively print an environment to string
recPrintEnv :: Env -> IO String
recPrintEnv env = do
  envStr <- liftIO $ printEnv env

  case parentEnv env of
    Just par -> do
        parEnvStr <- liftIO $ recPrintEnv par
        return $ envStr ++ "\n" ++ parEnvStr
    Nothing -> return envStr

-- |Recursively find all exports from the given environment
recExportsFromEnv :: Env -> IO [LispVal]
recExportsFromEnv env = do
  xs <- exportsFromEnv env

  case parentEnv env of
    Just par -> do
        pxs <- liftIO $ recExportsFromEnv par
        return $ xs ++ pxs
    Nothing -> return xs

-- |Return a list of symbols exported from an environment
exportsFromEnv :: Env 
               -> IO [LispVal]
exportsFromEnv env = do
  binds <- liftIO $ readIORef $ bindings env
  return $ getExports [] $ fst $ unzip $ Data.Map.toList binds 
 where 
  getExports acc (('m':'_':b) : bs) = getExports (Atom b:acc) bs
  getExports acc (('v':'_':b) : bs) = getExports (Atom b:acc) bs
  getExports acc (_ : bs) = getExports acc bs
  getExports acc [] = acc

-- |Create a deep copy of an environment
copyEnv :: Env      -- ^ Source environment
        -> IO Env   -- ^ A copy of the source environment
copyEnv env = do
  ptrs <- liftIO $ readIORef $ pointers env
  ptrList <- newIORef ptrs

  binds <- liftIO $ readIORef $ bindings env
  bindingListT <- mapM addBinding $ Data.Map.toList binds 
  bindingList <- newIORef $ Data.Map.fromList bindingListT
  return $ Environment (parentEnv env) bindingList ptrList
 where addBinding (name, val) = do 
         x <- liftIO $ readIORef val
         ref <- newIORef x
         return (name, ref)

-- |Perform a deep copy of an environment's contents into
--  another environment.
--
--  The destination environment is modified!
--
importEnv 
  :: Env -- ^ Destination environment
  -> Env -- ^ Source environment
  -> IO Env
importEnv dEnv sEnv = do
  sPtrs <- liftIO $ readIORef $ pointers sEnv
  dPtrs <- liftIO $ readIORef $ pointers dEnv
  writeIORef (pointers dEnv) $ Data.Map.union sPtrs dPtrs

  sBinds <- liftIO $ readIORef $ bindings sEnv
  dBinds <- liftIO $ readIORef $ bindings dEnv
  writeIORef (bindings dEnv)  $ Data.Map.union sBinds dBinds

  case parentEnv sEnv of
    Just ps -> importEnv dEnv ps 
    Nothing -> return dEnv 

-- |Extend given environment by binding a series of values to a new environment.
extendEnv :: Env -- ^ Environment 
          -> [((Char, String), LispVal)] -- ^ Extensions to the environment
          -> IO Env -- ^ Extended environment
extendEnv envRef abindings = do 
  bindinglistT <- (mapM addBinding abindings) -- >>= newIORef
  bindinglist <- newIORef $ Data.Map.fromList bindinglistT
  nullPointers <- newIORef $ Data.Map.fromList []
  return $ Environment (Just envRef) bindinglist nullPointers
 where addBinding ((namespace, name), val) = do ref <- newIORef val
                                                return (getVarName namespace name, ref)

-- |Find the top-most environment
topmostEnv :: Env -> IO Env
topmostEnv envRef = do
    case parentEnv envRef of
        Just p -> topmostEnv p
        Nothing -> return envRef

-- |Create a null environment with the given environment as its parent.
nullEnvWithParent :: Env -> IO Env 
nullEnvWithParent p = do
  Environment _ binds ptrs <- nullEnv
  return $ Environment (Just p) binds ptrs

-- |Recursively search environments to find one that contains the given variable.
findNamespacedEnv 
    :: Env      -- ^Environment to begin the search; 
                --  parent env's will be searched as well.
    -> Char     -- ^Namespace
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
    -> Char     -- ^ Namespace
    -> String   -- ^ Variable
    -> IO Bool  -- ^ True if the variable is bound
isNamespacedBound envRef namespace var = 
    (readIORef $ bindings envRef) >>= return . Data.Map.member (getVarName namespace var)

-- |Determine if a variable is bound in a given namespace
--  or a parent of the given environment.
isNamespacedRecBound 
    :: Env      -- ^ Environment
    -> Char     -- ^ Namespace
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

-- |Retrieve the value of a variable defined in the default namespace,
--  or Nothing if it is not defined
getVar' :: Env       -- ^ Environment
        -> String    -- ^ Variable
        -> IOThrowsError (Maybe LispVal) -- ^ Contents of the variable
getVar' envRef var = getNamespacedVar' envRef varNamespace var

-- |Retrieve an ioRef defined in a given namespace
getNamespacedRef :: Env     -- ^ Environment
                 -> Char    -- ^ Namespace
                 -> String  -- ^ Variable
                 -> IOThrowsError (IORef LispVal)
getNamespacedRef envRef
                 namespace
                 var = do
  let fnc io = return io
  v <- getNamespacedObj' envRef namespace var fnc
  case v of
    Just a -> return a
    Nothing -> (throwError $ UnboundVar "Getting an unbound variable" var)

-- |Retrieve the value of a variable defined in a given namespace
getNamespacedVar :: Env     -- ^ Environment
                 -> Char    -- ^ Namespace
                 -> String  -- ^ Variable
                 -> IOThrowsError LispVal -- ^ Contents of the variable
getNamespacedVar envRef
                 namespace
                 var = do
  v <- getNamespacedVar' envRef namespace var
  case v of
    Just a -> return a
    Nothing -> (throwError $ UnboundVar "Getting an unbound variable" var)

-- |Retrieve the value of a variable defined in a given namespace,
--  or Nothing if it is not defined
getNamespacedVar' :: Env     -- ^ Environment
                 -> Char    -- ^ Namespace
                 -> String  -- ^ Variable
                 -> IOThrowsError (Maybe LispVal) -- ^ Contents of the variable, if found
getNamespacedVar' envRef
                 namespace
                 var = do 
    getNamespacedObj' envRef namespace var fnc
  where fnc io = readIORef io

getNamespacedObj' :: Env     -- ^ Environment
                 -> Char    -- ^ Namespace
                 -> String  -- ^ Variable
                 -> (IORef LispVal -> IO a)
                 -> IOThrowsError (Maybe a) -- ^ Contents of the variable, if found
getNamespacedObj' envRef
                 namespace
                 var 
                 unpackFnc = do 
    binds <- liftIO $ readIORef $ bindings envRef
    case Data.Map.lookup (getVarName namespace var) binds of
      (Just a) -> do
          v <- liftIO $ unpackFnc a
          return $ Just v
      Nothing -> case parentEnv envRef of
                   (Just par) -> getNamespacedObj' par namespace var unpackFnc
                   Nothing -> return Nothing

-- |Set a variable in the default namespace
setVar
    :: Env      -- ^ Environment
    -> String   -- ^ Variable
    -> LispVal  -- ^ Value
    -> IOThrowsError LispVal -- ^ Value
setVar envRef var value = setNamespacedVar envRef varNamespace var value

-- |Set a variable in a given namespace
setNamespacedVar 
    :: Env      -- ^ Environment 
    -> Char     -- ^ Namespace
    -> String   -- ^ Variable
    -> LispVal  -- ^ Value
    -> IOThrowsError LispVal   -- ^ Value
setNamespacedVar envRef
                 namespace
                 var value = do 
  -- Issue #98 - Need to detect circular references
  --
  -- TODO:
  -- Note this implementation is rather simplistic since
  -- it does not take environments into account. The same
  -- variable name could refer to 2 different variables in
  -- different environments.
  case value of
    Pointer p _ -> do
      if p == var 
          then return value
          else next
    _ -> next

  where 
    next = do
      _ <- updatePointers envRef namespace var 
      _setNamespacedVar envRef namespace var value

-- |An internal function that does the actual setting of a 
--  variable, without all the extra code that keeps pointers
--  in sync when a variable is re-binded
--
--  Note this function still binds reverse pointers
--  for purposes of book-keeping.
_setNamespacedVar 
    :: Env      -- ^ Environment 
    -> Char     -- ^ Namespace
    -> String   -- ^ Variable
    -> LispVal  -- ^ Value
    -> IOThrowsError LispVal   -- ^ Value
_setNamespacedVar envRef
                 namespace
                 var value = do 
  -- Set the variable to its new value
  valueToStore <- getValueToStore namespace var envRef value
  _setNamespacedVarDirect envRef namespace var valueToStore

-- |Do the actual "set" operation, with NO pointer operations.
--  Only call this if you know what you are doing!
_setNamespacedVarDirect
    :: Env      -- ^ Environment 
    -> Char     -- ^ Namespace
    -> String   -- ^ Variable
    -> LispVal  -- ^ Value
    -> IOThrowsError LispVal   -- ^ Value
_setNamespacedVarDirect envRef
                 namespace
                 var valueToStore = do 
  env <- liftIO $ readIORef $ bindings envRef
  case Data.Map.lookup (getVarName namespace var) env of
    (Just a) -> do
      liftIO $ writeIORef a valueToStore
      return valueToStore
    Nothing -> case parentEnv envRef of
      (Just par) -> _setNamespacedVarDirect par namespace var valueToStore
      Nothing -> throwError $ UnboundVar "Setting an unbound variable: " var

-- |This helper function is used to keep pointers in sync when
--  a variable is bound to a different value.
updatePointers :: Env -> Char -> String -> IOThrowsError LispVal
updatePointers envRef namespace var = do
  ptrs <- liftIO $ readIORef $ pointers envRef
  case Data.Map.lookup (getVarName namespace var) ptrs of
    (Just valIORef) -> do
      val <- liftIO $ readIORef valIORef
      case val of 
        -- If var has any pointers, then we need to 
        -- assign the first pointer to the old value
        -- of x, and the rest need to be updated to 
        -- point to that first var

        -- This is the first pointer to (the old) var
        (Pointer pVar pEnv : ps) -> do
          -- Since var is now fresh, reset its pointers list
          liftIO $ writeIORef valIORef []

          -- The first pointer now becomes the old var,
          -- so its pointers list should become ps
          _ <- movePointers pEnv namespace pVar ps

          -- Each ps needs to be updated to point to pVar
          -- instead of var
          _ <- pointToNewVar pEnv namespace pVar ps

          -- Set first pointer to existing value of var
          existingValue <- getNamespacedVar envRef namespace var
          _setNamespacedVar pEnv namespace pVar existingValue

        -- No pointers, so nothing to do
        [] -> return $ Nil ""
        _ -> throwError $ InternalError
               "non-pointer value found in updatePointers"
    Nothing -> return $ Nil ""
 where
  -- |Move the given pointers (ptr) to the list of
  --  pointers for variable (var)
  movePointers :: Env -> Char -> String -> [LispVal] -> IOThrowsError LispVal
  movePointers envRef' namespace' var' ptrs = do
    env <- liftIO $ readIORef $ pointers envRef'
    case Data.Map.lookup (getVarName namespace' var') env of
      Just ps' -> do
        -- Append ptrs to existing list of pointers to var
        ps <- liftIO $ readIORef ps'
        liftIO $ writeIORef ps' $ ps ++ ptrs
        return $ Nil ""
      Nothing -> do
        -- var does not have any pointers; create new list
        valueRef <- liftIO $ newIORef ptrs
        liftIO $ writeIORef (pointers envRef') (Data.Map.insert (getVarName namespace var') valueRef env)
        return $ Nil ""

  -- |Update each pointer's source to point to pVar
  pointToNewVar pEnv namespace' pVar' (Pointer v e : ps) = do
    _ <- _setNamespacedVarDirect e namespace' v (Pointer pVar' pEnv)
    pointToNewVar pEnv namespace' pVar' ps
  pointToNewVar _ _ _ [] = return $ Nil ""
  pointToNewVar _ _ _ _ = throwError $ InternalError "pointToNewVar"

-- |A wrapper for updateNamespaceObject that uses the variable namespace.
updateObject :: Env -> String -> LispVal -> IOThrowsError LispVal
updateObject env var value = 
  updateNamespacedObject env varNamespace var value

-- |This function updates the object that "var" refers to. If "var" is
--  a pointer, that means this function will update that pointer (or the last
--  pointer in the chain) to point to the given "value" object. If "var"
--  is not a pointer, the result is the same as a setVar (but without updating
--  any pointer references, see below).
--
--  Note this function only updates the object, it does not
--  update any associated pointers. So it should probably only be
--  used internally by husk, unless you really know what you are
--  doing!
updateNamespacedObject :: Env -> Char -> String -> LispVal -> IOThrowsError LispVal
updateNamespacedObject env namespace var value = do
  varContents <- getNamespacedVar env namespace var
  obj <- findPointerTo varContents
  case obj of
    Pointer pVar pEnv -> do
      _setNamespacedVar pEnv namespace pVar value
    _ -> _setNamespacedVar env namespace var value

-- |Bind a variable in the default namespace
defineVar
    :: Env      -- ^ Environment
    -> String   -- ^ Variable
    -> LispVal  -- ^ Value
    -> IOThrowsError LispVal -- ^ Value
defineVar envRef var value = defineNamespacedVar envRef varNamespace var value

-- |Bind a variable in the given namespace
defineNamespacedVar
    :: Env      -- ^ Environment 
    -> Char     -- ^ Namespace
    -> String   -- ^ Variable
    -> LispVal  -- ^ Value
    -> IOThrowsError LispVal   -- ^ Value
defineNamespacedVar envRef
                    namespace
                    var value = do
  alreadyDefined <- liftIO $ isNamespacedBound envRef namespace var
  if alreadyDefined
    then setNamespacedVar envRef namespace var value >> return value
    else do
      --
      -- Future optimization:
      -- don't change anything if (define) is to existing pointer
      -- (IE, it does not really change anything)
      --


      -- If we are assigning to a pointer, we need a reverse lookup to 
      -- note that the pointer "value" points to "var"
      -- 
      -- So run through this logic to figure out what exactly to store,
      -- both for bindings and for rev-lookup pointers
      valueToStore <- getValueToStore namespace var envRef value
      liftIO $ do
        -- Write new value binding
        valueRef <- newIORef valueToStore
        env <- readIORef $ bindings envRef
        writeIORef (bindings envRef) (Data.Map.insert (getVarName namespace var) valueRef env)
        return valueToStore

-- |An internal helper function to get the value to save to an env
--  based on the value passed to the define/set function. Normally this
--  is straightforward, but there is book-keeping involved if a
--  pointer is passed, depending on if the pointer resolves to an object.
getValueToStore :: Char -> String -> Env -> LispVal -> IOThrowsError LispVal
getValueToStore namespace var env (Pointer p pEnv) = do
  addReversePointer namespace p pEnv namespace var env
getValueToStore _ _ _ value = return value

-- |Accept input for a pointer (ptrVar) and a variable that the pointer is going
--  to be assigned to. If that variable is an object then we setup a reverse lookup
--  for future book-keeping. Otherwise, we just look it up and return it directly, 
--  no booking-keeping required.
addReversePointer :: Char -> String -> Env -> Char -> String -> Env -> IOThrowsError LispVal
addReversePointer namespace var envRef ptrNamespace ptrVar ptrEnvRef = do
   env <- liftIO $ readIORef $ bindings envRef
   case Data.Map.lookup (getVarName namespace var) env of
     (Just a) -> do
       v <- liftIO $ readIORef a
       if isObject v
          then do
            -- Store a reverse pointer for book keeping
            ptrs <- liftIO $ readIORef $ pointers envRef
            
            -- Lookup ptr for var
            case Data.Map.lookup (getVarName namespace var) ptrs of
              -- Append another reverse ptr to this var
              -- FUTURE: make sure ptr is not already there, 
              --         before adding it to the list again?
              (Just valueRef) -> liftIO $ do
                value <- readIORef valueRef
                writeIORef valueRef (value ++ [Pointer ptrVar ptrEnvRef])
                return $ Pointer var envRef 

              -- No mapping, add the first reverse pointer
              Nothing -> liftIO $ do
                valueRef <- newIORef [Pointer ptrVar ptrEnvRef]
                writeIORef (pointers envRef) (Data.Map.insert (getVarName namespace var) valueRef ptrs)
                return $ Pointer var envRef -- Return non-reverse ptr to caller
          else return v -- Not an object, return value directly
     Nothing -> case parentEnv envRef of
       (Just par) -> addReversePointer namespace var par ptrNamespace ptrVar ptrEnvRef
       Nothing -> throwError $ UnboundVar "Getting an unbound variable: " var

-- |Return a value with a pointer dereferenced, if necessary
derefPtr :: LispVal -> IOThrowsError LispVal
-- Try dereferencing again if a ptr is found
--
-- Not sure if this is the best solution; it would be 
-- nice if we did not have to worry about multiple levels
-- of ptrs, especially since I believe husk only needs to 
-- have one level. but for now we will go with this to
-- move forward.
--
derefPtr (Pointer p env) = do
    result <- getVar env p
    derefPtr result
derefPtr v = return v

-- -- |Return the given list of values, but if any of the
-- --  original values is a pointer it will be dereferenced
-- derefPtrs :: [LispVal] -> IOThrowsError LispVal
-- derefPtrs lvs = mapM (liftThrows $ derefPtr) lvs

-- |Recursively process the given data structure, dereferencing
--  any pointers found along the way. 
-- 
--  This could potentially be expensive on large data structures 
--  since it must walk the entire object.
recDerefPtrs :: LispVal -> IOThrowsError LispVal
recDerefPtrs v = safeRecDerefPtrs [] v

-- |Attempt to dereference pointers safely, without being caught in a cycle
safeRecDerefPtrs :: [LispVal] -> LispVal -> IOThrowsError LispVal
#ifdef UsePointers
safeRecDerefPtrs ps (List l) = do
    result <- mapM (safeRecDerefPtrs ps) l
    return $ List result
safeRecDerefPtrs ps (DottedList ls l) = do
    ds <- mapM (safeRecDerefPtrs ps) ls
    d <- safeRecDerefPtrs ps l
    return $ DottedList ds d
safeRecDerefPtrs ps (Vector v) = do
   let vs = elems v
   ds <- mapM (safeRecDerefPtrs ps) vs
   return $ Vector $ listArray (0, length vs - 1) ds
safeRecDerefPtrs ps (HashTable ht) = do
    ks <- mapM (safeRecDerefPtrs ps)$ map (\ (k, _) -> k) $ Data.Map.toList ht
    vs <- mapM (safeRecDerefPtrs ps)$ map (\ (_, v) -> v) $ Data.Map.toList ht
    return $ HashTable $ Data.Map.fromList $ zip ks vs
#endif
safeRecDerefPtrs ps ptr@(Pointer p env) = do
    case containsPtr ps ptr of
        True -> return ptr -- Avoid cycle
        _ -> do
            result <- getVar env p
            safeRecDerefPtrs (ptr : ps) result 
safeRecDerefPtrs _ v = return v

containsPtr :: [LispVal] -> LispVal -> Bool
containsPtr ((Pointer pa ea):ps) p@(Pointer pb eb) = do
    let found = (pa == pb) && ((bindings ea) == (bindings eb))
    if found
       then True
       else containsPtr ps p
containsPtr _ _ = False

-- |A helper to recursively dereference all pointers and
--  pass results to a function
recDerefToFnc :: ([LispVal] -> ThrowsError LispVal) -> [LispVal] 
            -> IOThrowsError LispVal
recDerefToFnc fnc lvs = do
    List result <- recDerefPtrs $ List lvs 
    liftThrows $ fnc result

-- |A predicate to determine if the given lisp value 
--  is an "object" that can be pointed to.
isObject :: LispVal -> Bool
isObject (List _) = True
isObject (DottedList _ _) = True
isObject (String _) = True
isObject (Vector _) = True
isObject (HashTable _) = True
isObject (ByteVector _) = True
isObject (Pointer _ _) = True
isObject _ = False

-- |Same as dereferencing a pointer, except we want the
--  last pointer to an object (if there is one) instead
--  of the object itself
findPointerTo :: LispVal -> IOThrowsError LispVal
findPointerTo ptr@(Pointer p env) = do
    result <- getVar env p
    case result of
      (Pointer _ _) -> findPointerTo result
      _ -> return ptr
findPointerTo v = return v

