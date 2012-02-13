-- Based on code from: http://www.haskell.org/haskellwiki/GHC/As_a_library
--A.hs
--invoke: ghci -package ghc A.hs
 
import GHC
import Outputable
 
import GHC.Paths ( libdir )
--GHC.Paths is available via cabal install ghc-paths
 
import DynFlags
targetFile = "B.hs"
 
main = do
   res <- example
   putStrLn $ showSDoc ( ppr res )
 
example = 
    defaultErrorHandler defaultLogAction $ do
      runGhc (Just libdir) $ do
        dflags <- getSessionDynFlags
        let dflags' = foldl xopt_set dflags
                            [Opt_Cpp, Opt_ImplicitPrelude, Opt_MagicHash]
        setSessionDynFlags dflags'
        target <- guessTarget targetFile Nothing
        setTargets [target]
        load LoadAllTargets
        modSum <- getModSummary $ mkModuleName "B"
        p <- parseModule modSum
        t <- typecheckModule p
        d <- desugarModule t
        l <- loadModule d
        n <- getNamesInScope
        c <- return $ coreModule d
 
        g <- getModuleGraph
        mapM showModule g     
        return $ (parsedSource d,"/n-----/n",  typecheckedSource d)
