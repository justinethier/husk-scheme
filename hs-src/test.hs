-- a test program to demonstrate a potential compilation target
--
-- The corresponding scheme code is:
--
-- (write (+ 1 2))
--
-- Compile with:
--
-- ghc -cpp -Wall --make -package ghc -fglasgow-exts -o test hs-src/test.hs hs-src/Language/Scheme/Primitives.hs hs-src/Language/Scheme/Parser.hs hs-src/Language/Scheme/Numerical.hs hs-src/Language/Scheme/Core.hs hs-src/Language/Scheme/Macro.hs hs-src/Language/Scheme/FFI.hs hs-src/Language/Scheme/Macro/Matches.hs
--

module Main where
--import Paths_husk_scheme
import Language.Scheme.Core      -- Scheme Interpreter
import Language.Scheme.Numerical
import Language.Scheme.Primitives
import Language.Scheme.Types     -- Scheme data types
--import Language.Scheme.Variables -- Scheme variable operations
import Control.Monad.Error
import System.IO
--import System.Environment
--import System.Console.Haskeline

main :: IO String --()
main = do
  env <- primitiveBindings
  (runIOThrows $ liftM show $ run env (makeNullContinuation env) (Nil "") Nothing) -- >>= putStr ""  -- TODO: args  - see shell.hs

-- Compiled equivalent of:
-- (write (+ 1 2))
-- TODO: not really sure how to break this up into CPS, but I believe that is required in
-- order for continuations to work properly. Is there a need for a continueEval function,
-- or does the compiler setup everything in CPS style? If the compiler handles it, how does
-- the generated code work with dynamic code that is injected by (load) or via a REPL??
-- or maybe that code is separate so it does not matter??? - no, this cannot be the case because
-- a lambda defined in the compiled code could be passed a continuation from the dynamic
-- code, right?
{-
 - huski would handle this as follows:
 - - detect function application of (write)
 - - use prepareApply
 -   - detect function application of (+)
 -   - use prepareApply
 -     - eval 1
 -     - eval 2
 -   - use apply to call (+)
 - - use apply to call (write)
 - -}
run :: Env -> LispVal -> LispVal -> Maybe [LispVal] -> IOThrowsError LispVal
run env cont _ _ = do -- Missing params are result and args
-- Old code (as generated today)
--  result <- liftThrows $ numAdd [Number 1, Number 2]
--   writeProc (\ port obj -> hPrint port obj) [result]
 x1 <- -- TODO: lookup write from env
 continueEval env (makeCPS env cont f1) x1

-- then call into similar code that processes +
f1 env cont value _ = do
 x1 <- lookup +
 continueEval env (makeCPS env cont f2) x1

-- then call into code to get 1
f2 env cont value _ = do
 x1 <- Number 1
 continueEval env (makeCPSWArgs env cont f3 [x1]) value

f3 env cont value args = do
 x1 <- Number 2
 continueEval env (makeCPSWArgs env cont f4 (args ++ [x1])) value

-- TODO: then apply function corresponding to +
f4 env cont value args = do
-- TODO: need to apply args to func, and get value
-- then need to pass that to a continuation to a function f5 for (write)
--  x1 <- apply (makeCPSWArgs env cont f5) value args
--  continueEval env (makeCPSWArgs env cont x1

-- TODO: then apply function corresponding to write

{- TODO: this code is obsolete
f2 :: Env -> LispVal -> LispVal -> Maybe [LispVal] -> IOThrowsError LispVal
f2 env cont vals = do
  writeProc (\ port obj -> hPrint port obj) vals

-}
