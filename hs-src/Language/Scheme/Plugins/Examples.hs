-- This is just a test file, but the idea is that this would be a plugin
-- file that could be loaded at runtime by the husk interpreter.
--
-- This is just a concept right now, but is where we want to head with the FFI
module Language.Scheme.Plugins.Examples (test, test2) where

import Language.Scheme.Types
import Control.Monad.Error

test :: [LispVal] -> ThrowsError LispVal -- TODO: future - IOThrowsError, but would require externalizing continueEval
test [String s] = return $ String "Test from a function" 
test [Number n] = return $ Number (n + 1)
test _ = throwError $ Default "unknown args passed to test method"

test2 :: [LispVal] -> ThrowsError LispVal
test2 [String s] = return $ String "TEST2 - Test from a function" 
test2 [Number n] = return $ Number (n + 2)
test2 _ = throwError $ Default "TEST2 - unknown args passed to test method"
