-- a test program to demonstrate a potential compilation target
--
-- ghc -cpp -Wall --make -package ghc -fglasgow-exts -o test hs-src/test.hs hs-src/Language/Scheme/Primitives.hs hs-src/Language/Scheme/Parser.hs hs-src/Language/Scheme/Numerical.hs
--

module Main where
--import Paths_husk_scheme
--import Language.Scheme.Core      -- Scheme Interpreter
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
  (runIOThrows $ liftM show $ run) -- >>= putStr ""  -- TODO: args  - see shell.hs

run :: IOThrowsError LispVal
run = do
--TODO: how to handle throws, how to handle multiple expressions (hint: we need to use CPS)...
  result <- liftThrows $ numAdd [Number 1, Number 2]
  writeProc (\ port obj -> hPrint port obj) [result]
{-main = do args <- getArgs
          if null args then do showBanner
                               runRepl
                       else runOne $ args -}
