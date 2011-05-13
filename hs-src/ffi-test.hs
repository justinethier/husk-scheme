-- This is just a test file, but the idea is that this would be a plugin
-- file that could be loaded at runtime by the husk interpreter.
--
-- This is just a concept right now, but is where we want to head with the FFI
module Test where

import Language.Scheme.Types
import Control.Monad.Error

test :: [LispVal] -> IOThrowsError LispVal
test _ = return $ String "test" --throwError $ Default "test method"
