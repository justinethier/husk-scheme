-- This is just a test file, but the idea is that this would be a plugin
-- file that could be loaded at runtime by the husk interpreter.
--
-- This is just a concept right now, but is where we want to head with the FFI
module Test (Test.test) where

import Language.Scheme.Types
import Control.Monad.Error

test :: [LispVal] -> ThrowsError LispVal -- TODO: future - IOThrowsError, but would require externalizing continueEval
test [String s] = return $ String "Test from a function" --return $ String "test" --throwError $ Default "test method"
test [Number n] = return $ Number (n + 1)
test _ = throwError $ Default "unknown args passed to test method"
