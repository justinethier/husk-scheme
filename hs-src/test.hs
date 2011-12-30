-- a test program to demonstrate a potential compilation target
--
-- ghc -cpp -Wall --make -package ghc -fglasgow-exts -o test hs-src/test.hs hs-src/Language/Scheme/Primitives.hs hs-src/Language/Scheme/Parser.hs hs-src/Language/Scheme/Numerical.hs
--

module Main where
--import Paths_husk_scheme
--import Language.Scheme.Core      -- Scheme Interpreter
import Language.Scheme.Primitives
import Language.Scheme.Types     -- Scheme data types
--import Language.Scheme.Variables -- Scheme variable operations
import Control.Monad.Error
import System.IO
--import System.Environment
--import System.Console.Haskeline

main :: IO LispVal --()
main = do
--TODO: how to handle throws
  writeProc (\ port obj -> hPrint port obj) [Number 1]
{-main = do args <- getArgs
          if null args then do showBanner
                               runRepl
                       else runOne $ args -}
