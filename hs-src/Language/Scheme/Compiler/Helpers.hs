{-

TODO:

makeNormalFunc
-}

module Language.Scheme.Compiler.Helpers where
import Language.Scheme.Types

--makeNormalFunc :: Env -> [LispVal] -> String -> IOThrowsError LispVal 
makeHFunc ::
--            (Monad m) =>
            Maybe String 
         -> Env 
         -> [String] 
         -> (Env -> LispVal -> LispVal -> Maybe [LispVal] -> IOThrowsError LispVal) 
         -> String 
         -> LispVal --m LispVal
makeHFunc varargs env fparams fbody = return $ HFunc fparams varargs fbody env --(map showVal fparams) varargs fbody env
makeNormalHFunc :: -- (Monad m) =>
                  Env
               -> [String]
               -> (Env -> LispVal -> LispVal -> Maybe [LispVal] -> IOThrowsError LispVal)
               -> String
               -> LispVal --m LispVal
makeNormalHFunc = makeHFunc Nothing
{- TODO:
makeHVarargs :: (Monad m) => LispVal -> Env
                        -> [LispVal]
                        -> [LispVal]
                        -> m LispVal
makeHVarargs = makeFunc . Just . showVal
-}
