{- |
Module      : Language.Scheme.Types
Copyright   : Justin Ethier
Licence     : MIT (see LICENSE in the distribution)

Maintainer  : github.com/justinethier
Stability   : experimental
Portability : portable

husk scheme interpreter

A lightweight dialect of R5RS scheme.

This module contains top-level data type definitions and their associated functions, including:
 - Scheme data types
 - Scheme errors

-}

module Language.Scheme.Types where
import Complex
import Control.Monad.Error
import Data.Array
import Data.IORef
import qualified Data.Map
import IO hiding (try)
import Ratio
import Text.ParserCombinators.Parsec hiding (spaces)

-- Environment management

-- |A Scheme environment containing variable bindings of form @(namespaceName, variableName), variableValue@
data Env = Environment {parentEnv :: (Maybe Env), bindings :: (IORef [((String, String), IORef LispVal)])} -- lookup via: (namespace, variable)

-- |An empty environment
nullEnv :: IO Env
nullEnv = do nullBindings <- newIORef []
             return $ Environment Nothing nullBindings

-- Internal namespace for macros
macroNamespace :: [Char]
macroNamespace = "m"

-- Internal namespace for variables
varNamespace :: [Char]
varNamespace = "v"

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

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = runErrorT (trapError action) >>= return . extractValue

-- |Scheme data types
data LispVal = Atom String
 -- ^Symbol
 | List [LispVal]
 -- ^List
 | DottedList [LispVal] LispVal
 -- ^Pair
 | Vector (Array Int LispVal)
 -- ^Vector
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
 | IOFunc ([LispVal] -> IOThrowsError LispVal)
 -- ^Primitive function within the IO monad
 | EvalFunc ([LispVal] -> IOThrowsError LispVal)
 {- ^Function within the IO monad with access to
 the current environment and continuation. -}
 | Port Handle
 -- ^I/O port
 | Continuation { closure :: Env                       -- Environment of the continuation
                 , currentCont :: (Maybe DeferredCode)  -- Code of current continuation
                 , nextCont :: (Maybe LispVal)       -- Code to resume after body of cont
                 , extraReturnArgs :: (Maybe [LispVal]) -- Extra return arguments, to support (values) and (call-with-values)
                        , dynamicWind :: (Maybe [DynamicWinders]) -- Functions injected by (dynamic-wind)
                }
 -- ^Continuation
 | Syntax { closure :: Env
          , identifiers :: [LispVal]
          , pattern :: [LispVal]
          , template :: [LispVal] -- TODO: use a syntax-rules type to hold a single pattern/transform pair?

   } -- ^ Type to hold a syntax object that is created by a macro definition.
     --   Syntax objects are not used like regular types in that they are not
     --   passed around within variables. In other words, you cannot use set! to
     --   assign a variable to a syntax object. But they are used during function
     --   application. In any case, it is convenient to define the type here 
     --   because syntax objects are stored in the same environments and 
     --   manipulated by the same functions as regular variables.
 | SyntaxResult { synExpanded :: LispVal -- Expanded syntax
                , synMatched :: Bool  -- True unless an nary (0-or-more match) pattern variable is not matched
   } -- ^Internal use only
 | EOF
 | Nil String
 -- ^Internal use only; do not use this type directly.

-- |A helper function to create a syntax result
normalSyntaxResult lisp = SyntaxResult lisp True
getSyntaxResult (SyntaxResult r _) = r
--getSyntaxResult _ = throwError $ Default "Unexpected error in getSyntaxResult"

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
eqv [(HashTable arg1), (HashTable arg2)] =
  eqv [List $ (map (\ (x, y) -> List [x, y]) $ Data.Map.toAscList arg1),
       List $ (map (\ (x, y) -> List [x, y]) $ Data.Map.toAscList arg2)]
eqv [l1@(List _), l2@(List _)] = eqvList eqv [l1, l2]
eqv [_, _] = return $ Bool False
eqv badArgList = throwError $ NumArgs 2 badArgList

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
showVal (SyntaxResult result noMatch) = "{" ++ show result ++ ", " ++ show noMatch ++ "}"
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
showVal (HashTable _) = "<hash-table>"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList h t) = "(" ++ unwordsList h ++ " . " ++ showVal t ++ ")"
showVal (PrimitiveFunc _) = "<primitive>"
showVal (Continuation _ _ _ _ _) = "<continuation>"
showVal (Func {params = args, vararg = varargs, body = _, closure = _}) =
  "(lambda (" ++ unwords (map show args) ++
    (case varargs of
      Nothing -> ""
      Just arg -> " . " ++ arg) ++ ") ...)"
showVal (Port _) = "<IO port>"
showVal (IOFunc _) = "<IO primitive>"
showVal (EvalFunc _) = "<procedure>"

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

-- |Allow conversion of lispval instances to strings
instance Show LispVal where show = showVal
