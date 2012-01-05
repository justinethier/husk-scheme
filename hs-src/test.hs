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
run, f1, f2, f3, f4, f5 :: Env -> LispVal -> LispVal -> Maybe [LispVal] -> IOThrowsError LispVal
run env cont _ _ = do -- Missing params are result and args
-- Old code (as generated today)
--  result <- liftThrows $ numAdd [Number 1, Number 2]
--   writeProc (\ port obj -> hPrint port obj) [result]
 x1 <- getVar env "write" 
 continueEval env (makeCPS env cont f1) x1

-- then call into similar code that processes +
f1 env cont value _ = do
TODO: value (the write function) is now lost - ooops... how to handle? should a new continuation be created?
 x1 <- getVar env "+" 
 continueEval env (makeCPS env cont f2) x1

-- then call into code to get 1
f2 env cont value _ = do
 x1 <- Number 1
 continueEval env (makeCPSWArgs env cont f3 [x1]) value

f3 env cont value (Just args) = do
 x1 <- Number 2
 continueEval env (makeCPSWArgs env cont f4 (args ++ [x1])) value

-- TODO: then apply function corresponding to +
f4 env cont value (Just args) = do
  apply (makeCPSWArgs env cont f5) value args
-- TODO: need to apply args to func, and get value
-- then need to pass that to a continuation to a function f5 for (write)
--  x1 <- apply (makeCPSWArgs env cont f5) value args
--  continueEval env (makeCPSWArgs env cont x1



-- Stubs to attempt to deal with above
apply :: LispVal -> LispVal -> [LispVal] -> IOThrowsError LispVal
apply cont (PrimitiveFunc func) args = do
  result <- liftThrows $ func args
  case cont of
    Continuation cEnv _ _ _ _ -> continueEval cEnv cont result
    _ -> return result

continueEval :: Env -> LispVal -> LispVal -> IOThrowsError LispVal

{- Passing a higher-order function as the continuation; just evaluate it. This is
 - done to enable an 'eval' function to be broken up into multiple sub-functions,
 - so that any of the sub-functions can be passed around as a continuation. 
 -
 - Carry extra args from the current continuation into the next, to support (call-with-values)
 -}
continueEval _
            (Continuation cEnv (Just (HaskellBody func funcArgs))
                               (Just (Continuation cce cnc ccc _ cdynwind))
                                xargs _) -- rather sloppy, should refactor code so this is not necessary
             val = func cEnv (Continuation cce cnc ccc xargs cdynwind) val funcArgs
{-
 - No higher order function, so:
 -
 - If there is Scheme code to evaluate in the function body, we continue to evaluate it.
 -
 - Otherwise, if all code in the function has been executed, we 'unwind' to an outer
 - continuation (if there is one), or we just return the result. Yes technically with
 - CPS you are supposed to keep calling into functions and never return, but in this case
 - when the computation is complete, you have to return something. 
 -}
continueEval _ (Continuation cEnv (Just (SchemeBody cBody)) (Just cCont) extraArgs dynWind) val = do
--    case (trace ("cBody = " ++ show cBody) cBody) of
    case cBody of
        [] -> do
          case cCont of
            Continuation nEnv ncCont nnCont _ nDynWind ->
              -- Pass extra args along if last expression of a function, to support (call-with-values)
              continueEval nEnv (Continuation nEnv ncCont nnCont extraArgs nDynWind) val
            _ -> return (val)
        [lv] -> meval cEnv (Continuation cEnv (Just (SchemeBody [])) (Just cCont) Nothing dynWind) lv
        (lv : lvs) -> meval cEnv (Continuation cEnv (Just (SchemeBody lvs)) (Just cCont) Nothing dynWind) lv

-- No current continuation, but a next cont is available; call into it
continueEval _ (Continuation cEnv Nothing (Just cCont) _ _) val = continueEval cEnv cCont val

-- There is no continuation code, just return value
continueEval _ (Continuation _ Nothing Nothing _ _) val = return val
continueEval _ _ _ = throwError $ Default "Internal error in continueEval"
