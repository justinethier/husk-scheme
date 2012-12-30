{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{- |
Module      : Language.Scheme.Types
Copyright   : Justin Ethier
Licence     : MIT (see LICENSE in the distribution)

Maintainer  : github.com/justinethier
Stability   : experimental
Portability : portable

This module contains top-level data type definitions, environments, error types, and associated functions.

-}

module Language.Scheme.Types
    ( Env (..)
    , nullEnv 
    , LispError (..)
    , ThrowsError 
    , trapError
    , extractValue 
    , IOThrowsError 
    , liftThrows 
    , runIOThrowsREPL 
    , runIOThrows 
    , LispVal (
          Atom
        , List
        , DottedList
        , Vector
        , ByteVector
        , HashTable
        , Number
        , Float
        , Complex
        , Rational
        , String
        , Char
        , Bool
        , PrimitiveFunc
        , Func
             , params
             , vararg
             , body
             , closure
        , HFunc
             , hparams
             , hvararg
             , hbody
             , hclosure
        , IOFunc
        , EvalFunc
        , Pointer
             , pointerVar
             , pointerEnv
        , Opaque
        , Port
        , Continuation
             , contClosure
             , currentCont
             , nextCont
             , extraReturnArgs
             , dynamicWind
        , Syntax
             , synClosure
             , synRenameClosure
             , synDefinedInMacro
             , synIdentifiers
             , synRules
        , SyntaxExplicitRenaming
        , LispEnv
        , EOF
        , Nil)
    , toOpaque
    , fromOpaque
    , DeferredCode (..)
    , DynamicWinders (..)
    , makeNullContinuation 
    , makeCPS 
    , makeCPSWArgs 
    , eqv 
    , eqvList
    , eqVal 
    , makeFunc
    , makeNormalFunc
    , makeVarargs
    , makeHFunc
    , makeNormalHFunc
    , makeHVarargs
    )
 where
import Control.Monad.Error
import Data.Complex
import Data.Array
import qualified Data.ByteString as BS
import Data.Dynamic
import Data.IORef
import qualified Data.Map
-- import Data.Maybe
import Data.Ratio
import Data.Word
import System.IO
import Text.ParserCombinators.Parsec hiding (spaces)

-- Environment management

-- |A Scheme environment containing variable bindings of form @(namespaceName, variableName), variableValue@
data Env = Environment {
        parentEnv :: (Maybe Env), 
        bindings :: (IORef (Data.Map.Map String (IORef LispVal))),
        pointers :: (IORef (Data.Map.Map String (IORef [LispVal])))
    }

-- |An empty environment
nullEnv :: IO Env
nullEnv = do 
    nullBindings <- newIORef $ Data.Map.fromList []
    nullPointers <- newIORef $ Data.Map.fromList []
    return $ Environment Nothing nullBindings nullPointers

-- |Types of errors that may occur when evaluating Scheme code
data LispError = NumArgs Integer [LispVal] -- ^Invalid number of function arguments
  | TypeMismatch String LispVal -- ^Type error
  | Parser ParseError -- ^Parsing error
  | BadSpecialForm String LispVal -- ^Invalid special (built-in) form
  | NotFunction String String
  | UnboundVar String String
  | DivideByZero -- ^Divide by Zero error
  | NotImplemented String
  | InternalError String {- ^An internal error within husk; in theory user (Scheme) code
                         should never allow one of these errors to be triggered. -}
  | Default String -- ^Default error

-- |Create a textual description for a 'LispError'
showError :: LispError -> String
showError (NumArgs expected found) = "Expected " ++ show expected
                                  ++ " args; found values " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected
                                  ++ ", found " ++ show found
showError (Parser parseErr) = "Parse error at " ++ ": " ++ show parseErr
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func) = message ++ ": " ++ show func
showError (UnboundVar message varname) = message ++ ": " ++ varname
showError (DivideByZero) = "Division by zero"
showError (NotImplemented message) = "Not implemented: " ++ message
showError (InternalError message) = "An internal error occurred: " ++ message
showError (Default message) = "Error: " ++ message

instance Show LispError where show = showError
instance Error LispError where
  noMsg = Default "An error has occurred"
  strMsg = Default

type ThrowsError = Either LispError

trapError :: -- forall (m :: * -> *) e.
            (MonadError e m, Show e) =>
             m String -> m String
trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val
extractValue (Left _) = error "Unexpected error in extractValue; "

type IOThrowsError = ErrorT LispError IO

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

-- |Execute an IO action and return result or an error message.
--  This is intended for use by a REPL, where a result is always
--  needed regardless of type.
runIOThrowsREPL :: IOThrowsError String -> IO String
runIOThrowsREPL action = runErrorT (trapError action) >>= return . extractValue

-- |Execute an IO action and return error or Nothing if no error was thrown.
runIOThrows :: IOThrowsError String -> IO (Maybe String)
runIOThrows action = do
    runState <- runErrorT action
    case runState of
        Left err -> return $ Just (show err)
        Right _ -> return $ Nothing

-- |Scheme data types
data LispVal = Atom String
 -- ^Symbol
 | List [LispVal]
 -- ^List
 | DottedList [LispVal] LispVal
 -- ^Pair
 | Vector (Array Int LispVal)
 -- ^Vector
 | ByteVector BS.ByteString
 -- ^ByteVector from R7RS
 | HashTable (Data.Map.Map LispVal LispVal)
 {- ^Hash table.
 Technically this could be a derived data type instead of being built-in to the
 interpreter. And perhaps in the future it will be. But for now, a hash table
 is too important of a data type to not be included. -}
 --
 -- Map is technically the wrong structure to use for a hash table since it is based on a binary tree and hence operations tend to be O(log n) instead of O(1). However, according to <http://www.opensubscriber.com/message/haskell-cafe@haskell.org/10779624.html> Map has good performance characteristics compared to the alternatives. So it stays for the moment...
 --
 | Number Integer {- FUTURE: rename this to "Integer" (or "WholeNumber" or something else more meaningful)
 Integer -}
 | Float Double {- FUTURE: rename this "Real" instead of "Float"...
 Floating point -}
 | Complex (Complex Double)
 -- ^Complex number
 | Rational Rational
 -- ^Rational number
 | String String
 -- ^String
 | Char Char
 -- ^Character
 | Bool Bool
 -- ^Boolean
 | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
 -- ^Primitive function
 | Func {params :: [String],
         vararg :: (Maybe String),
         body :: [LispVal],
         closure :: Env
        }
 -- ^Function
 | HFunc {hparams :: [String],
          hvararg :: (Maybe String),
          hbody :: (Env -> LispVal -> LispVal -> Maybe [LispVal] -> IOThrowsError LispVal),
          hclosure :: Env
        }
 -- ^Function formed from a Haskell function
 | IOFunc ([LispVal] -> IOThrowsError LispVal)
 -- ^Primitive function within the IO monad
 | EvalFunc ([LispVal] -> IOThrowsError LispVal)
 {- ^Function within the IO monad with access to
 the current environment and continuation. -}
 | Pointer { pointerVar :: String
            ,pointerEnv :: Env } 
 -- ^Pointer to an environment variable.
 | Opaque Dynamic
 -- ^Opaque Haskell value.
 | Port Handle
 -- ^I/O port
 | Continuation {  contClosure :: Env                   -- Environment of the continuation
                 , currentCont :: (Maybe DeferredCode)  -- Code of current continuation
                 , nextCont :: (Maybe LispVal)          -- Code to resume after body of cont
                 , extraReturnArgs :: (Maybe [LispVal]) -- Extra return arguments, to support (values) and (call-with-values)
                        , dynamicWind :: (Maybe [DynamicWinders]) -- Functions injected by (dynamic-wind)
                }
 -- ^Continuation
 | Syntax { synClosure :: Maybe Env       -- ^ Code env in effect at definition time, if applicable
          , synRenameClosure :: Maybe Env -- ^ Renames (from macro hygiene) in effect at def time;
                                          --   only applicable if this macro defined inside another macro.
          , synDefinedInMacro :: Bool
          , synIdentifiers :: [LispVal]   -- ^ Literal identifiers from syntax-rules 
          , synRules :: [LispVal]         -- ^ Rules from syntax-rules
   } -- ^ Type to hold a syntax object that is created by a macro definition.
     --   Syntax objects are not used like regular types in that they are not
     --   passed around within variables. In other words, you cannot use set! to
     --   assign a variable to a syntax object. But they are used during function
     --   application. In any case, it is convenient to define the type here 
     --   because syntax objects are stored in the same environments and 
     --   manipulated by the same functions as regular variables.
 | SyntaxExplicitRenaming LispVal
   -- ^ Syntax for an explicit-renaming macro
 | LispEnv Env
 | EOF
 | Nil String
 -- ^Internal use only; do not use this type directly.

-- |Convert a Haskell value to an opaque Lisp value.
toOpaque :: Typeable a => a -> LispVal
toOpaque = Opaque . toDyn

-- |Convert an opaque Lisp value back into a Haskell value of the appropriate
--  type, or produce a TypeMismatch error.
fromOpaque :: forall a. Typeable a => LispVal -> ThrowsError a
-- fromOpaque (Opaque o) | isJust $ fromDynamic o = fromJust $ fromDynamic o
-- fromOpaque badArg = throwError $ TypeMismatch (show $ toOpaque (undefined :: a)) badArg

-- Old version that used ViewPatterns
fromOpaque (Opaque (fromDynamic -> Just v)) = return v
fromOpaque badArg = throwError $ TypeMismatch (show $ toOpaque (undefined :: a)) badArg

-- |Container to hold code that is passed to a continuation for deferred execution
data DeferredCode =
    SchemeBody [LispVal] | -- ^A block of Scheme code
    HaskellBody {
       contFunction :: (Env -> LispVal -> LispVal -> Maybe [LispVal] -> IOThrowsError LispVal)
     , contFunctionArgs :: (Maybe [LispVal]) -- Arguments to the higher-order function
    } -- ^A Haskell function

-- |Container to store information from a dynamic-wind
data DynamicWinders = DynamicWinders {
    before :: LispVal -- ^Function to execute when resuming continuation within extent of dynamic-wind
  , after :: LispVal -- ^Function to execute when leaving extent of dynamic-wind
}

showDWVal :: DynamicWinders -> String
showDWVal (DynamicWinders b a) = "(" ++ (show b) ++ " . " ++ (show a) ++ ")"

instance Show DynamicWinders where show = showDWVal

-- Make an "empty" continuation that does not contain any code
makeNullContinuation :: Env -> LispVal
makeNullContinuation env = Continuation env Nothing Nothing Nothing Nothing

-- Make a continuation that takes a higher-order function (written in Haskell)
makeCPS :: Env -> LispVal -> (Env -> LispVal -> LispVal -> Maybe [LispVal] -> IOThrowsError LispVal) -> LispVal
makeCPS env cont@(Continuation _ _ _ _ dynWind) cps = Continuation env (Just (HaskellBody cps Nothing)) (Just cont) Nothing dynWind
makeCPS env cont cps = Continuation env (Just (HaskellBody cps Nothing)) (Just cont) Nothing Nothing -- This overload just here for completeness; it should never be used

-- Make a continuation that stores a higher-order function and arguments to that function
makeCPSWArgs :: Env -> LispVal -> (Env -> LispVal -> LispVal -> Maybe [LispVal] -> IOThrowsError LispVal) -> [LispVal] -> LispVal
makeCPSWArgs env cont@(Continuation _ _ _ _ dynWind) cps args = Continuation env (Just (HaskellBody cps (Just args))) (Just cont) Nothing dynWind
makeCPSWArgs env cont cps args = Continuation env (Just (HaskellBody cps (Just args))) (Just cont) Nothing Nothing -- This overload just here for completeness; it should never be used

instance Ord LispVal where
  compare (Bool a) (Bool b) = compare a b
  compare (Number a) (Number b) = compare a b
  compare (Rational a) (Rational b) = compare a b
  compare (Float a) (Float b) = compare a b
  compare (String a) (String b) = compare a b
  compare (Char a) (Char b) = compare a b
  compare (Atom a) (Atom b) = compare a b
{- compare (DottedList xs x) (DottedList xs x) = compare a b
Vector
HashTable
List
Func
Others? -}
  compare a b = compare (show a) (show b) -- Hack (??): sort alphabetically when types differ or have no handlers

-- |Compare two 'LispVal' instances
eqv :: [LispVal] -> ThrowsError LispVal
eqv [(Bool arg1), (Bool arg2)] = return $ Bool $ arg1 == arg2
eqv [(Number arg1), (Number arg2)] = return $ Bool $ arg1 == arg2
eqv [(Complex arg1), (Complex arg2)] = return $ Bool $ arg1 == arg2
eqv [(Rational arg1), (Rational arg2)] = return $ Bool $ arg1 == arg2
eqv [(Float arg1), (Float arg2)] = return $ Bool $ arg1 == arg2
eqv [(String arg1), (String arg2)] = return $ Bool $ arg1 == arg2
eqv [(Char arg1), (Char arg2)] = return $ Bool $ arg1 == arg2
eqv [(Atom arg1), (Atom arg2)] = return $ Bool $ arg1 == arg2
eqv [(DottedList xs x), (DottedList ys y)] = eqv [List $ xs ++ [x], List $ ys ++ [y]]
eqv [(Vector arg1), (Vector arg2)] = eqv [List $ (elems arg1), List $ (elems arg2)]
eqv [(ByteVector a), (ByteVector b)] = return $ Bool $ a == b
eqv [(HashTable arg1), (HashTable arg2)] =
  eqv [List $ (map (\ (x, y) -> List [x, y]) $ Data.Map.toAscList arg1),
       List $ (map (\ (x, y) -> List [x, y]) $ Data.Map.toAscList arg2)]
--
-- This comparison function may be too simplistic. Basically we check to see if
-- functions have the same calling interface. If they do, then we compare the 
-- function bodies for equality.
--
--FUTURE:
--
-- The real solution for this and many of the other comparison functions is to
-- assign memory locations to data. Then we can just compare memory locations
-- in cases such as this one. But that is a much larger change.
eqv [x@(Func _ _ xBody _), y@(Func _ _ yBody _)] = do
  if (show x) /= (show y)
     then return $ Bool False
     else eqvList eqv [List xBody, List yBody] 
eqv [x@(HFunc _ _ _ _), y@(Func _ _ _ _)] = do
  if (show x) /= (show y)
     then return $ Bool False
     else return $ Bool True -- TODO: compare high-order functions... eqvList eqv [List xBody, List yBody] 
--
eqv [x@(PrimitiveFunc _), y@(PrimitiveFunc _)] = return $ Bool $ (show x) == (show y)
eqv [x@(IOFunc _), y@(IOFunc _)] = return $ Bool $ (show x) == (show y)
eqv [x@(EvalFunc _), y@(EvalFunc _)] = return $ Bool $ (show x) == (show y)
-- FUTURE: comparison of two continuations
eqv [l1@(List _), l2@(List _)] = eqvList eqv [l1, l2]
eqv [_, _] = return $ Bool False
eqv badArgList = throwError $ NumArgs 2 badArgList

-- |Compare two lists of haskell values, using the given comparison function
eqvList :: ([LispVal] -> ThrowsError LispVal) -> [LispVal] -> ThrowsError LispVal
eqvList eqvFunc [(List arg1), (List arg2)] = return $ Bool $ (length arg1 == length arg2) &&
                                                    (all eqvPair $ zip arg1 arg2)
    where eqvPair (x1, x2) = case eqvFunc [x1, x2] of
                               Left _ -> False
                               Right (Bool val) -> val
                               _ -> False -- OK?
eqvList _ _ = throwError $ Default "Unexpected error in eqvList"

eqVal :: LispVal -> LispVal -> Bool
eqVal a b = do
  let result = eqv [a, b]
  case result of
    Left _ -> False
    Right (Bool val) -> val
    _ -> False -- Is this OK?

instance Eq LispVal where
  x == y = eqVal x y

-- |Create a textual description of a 'LispVal'
showVal :: LispVal -> String
showVal (Nil _) = ""
showVal (EOF) = "#!EOF"
showVal (LispEnv _) = "<env>"
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Char chr) = [chr]
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Complex contents) = (show $ realPart contents) ++ "+" ++ (show $ imagPart contents) ++ "i"
showVal (Rational contents) = (show (numerator contents)) ++ "/" ++ (show (denominator contents))
showVal (Float contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (Vector contents) = "#(" ++ (unwordsList $ Data.Array.elems contents) ++ ")"
showVal (ByteVector contents) = "#u8(" ++ unwords (map show (BS.unpack contents)) ++ ")"
showVal (HashTable _) = "<hash-table>"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList h t) = "(" ++ unwordsList h ++ " . " ++ showVal t ++ ")"
showVal (PrimitiveFunc _) = "<primitive>"
showVal (Continuation _ _ _ _ _) = "<continuation>"
showVal (Syntax _ _ _ _ _) = "<syntax>"
showVal (SyntaxExplicitRenaming _) = "<er-macro-transformer syntax>"
showVal (Func {params = args, vararg = varargs, body = _, closure = _}) =
  "(lambda (" ++ unwords (map show args) ++
    (case varargs of
      Nothing -> ""
      Just arg -> " . " ++ arg) ++ ") ...)"
showVal (HFunc {hparams = args, hvararg = varargs, hbody = _, hclosure = _}) =
  "(lambda (" ++ unwords (map show args) ++
    (case varargs of
      Nothing -> ""
      Just arg -> " . " ++ arg) ++ ") ...)"
showVal (Port _) = "<IO port>"
showVal (IOFunc _) = "<IO primitive>"
showVal (EvalFunc _) = "<procedure>"
showVal (Pointer p _) = "<ptr " ++ p ++ ">"
showVal (Opaque d) = "<Haskell " ++ show (dynTypeRep d) ++ ">"

-- |Convert a list of Lisp objects into a space-separated string
unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

-- |Allow conversion of lispval instances to strings
instance Show LispVal where show = showVal


-- Functions required by the interpreter --

-- |Create a scheme function
makeFunc :: -- forall (m :: * -> *).
            (Monad m) =>
            Maybe String -> Env -> [LispVal] -> [LispVal] -> m LispVal
makeFunc varargs env fparams fbody = return $ Func (map showVal fparams) varargs fbody env

-- |Create a normal scheme function
makeNormalFunc :: (Monad m) => Env
               -> [LispVal]
               -> [LispVal]
               -> m LispVal
makeNormalFunc = makeFunc Nothing

-- |Create a scheme function that can receive any number of arguments
makeVarargs :: (Monad m) => LispVal -> Env
                        -> [LispVal]
                        -> [LispVal]
                        -> m LispVal
makeVarargs = makeFunc . Just . showVal

-- Functions required by a compiled program --

-- |Create a haskell function
makeHFunc ::
            (Monad m) =>
            Maybe String 
         -> Env 
         -> [String] 
         -> (Env -> LispVal -> LispVal -> Maybe [LispVal] -> IOThrowsError LispVal) 
--         -> String 
         -> m LispVal
makeHFunc varargs env fparams fbody = return $ HFunc fparams varargs fbody env --(map showVal fparams) varargs fbody env
-- |Create a normal haskell function
makeNormalHFunc :: (Monad m) =>
                  Env
               -> [String]
               -> (Env -> LispVal -> LispVal -> Maybe [LispVal] -> IOThrowsError LispVal)
               -> m LispVal
makeNormalHFunc = makeHFunc Nothing

-- |Create a haskell function that can receive any number of arguments
makeHVarargs :: (Monad m) => LispVal 
                        -> Env
                        -> [String]
                        -> (Env -> LispVal -> LispVal -> Maybe [LispVal] -> IOThrowsError LispVal)
                        -> m LispVal
makeHVarargs = makeHFunc . Just . showVal
