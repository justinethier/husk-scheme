-- Testing integration with Text.JSON

module Language.Scheme.Plugins.Json where 

import Control.Monad.Error
import Data.Ratio
import Text.JSON
import Text.JSON.Generic
import Language.Scheme.Types

-- ideas from http://therning.org/magnus/archives/719
instance JSON LispVal where
  showJSON (List []) = JSNull
  showJSON (String s) = JSString $ toJSString s
  showJSON (Bool b) = JSBool b
  showJSON (Number n) = JSRational True $ fromInteger n 
  showJSON (Float n) = JSRational False $ toRational n 
  showJSON (List l) = showJSONs l
  -- TODO: JSObject

  readJSON (JSNull) = return $ List []
  readJSON (JSString str) = return $ String $ fromJSString str
  readJSON (JSBool b) = return $ Bool b
  readJSON (JSRational True num) = return $ Number $ numerator num
  readJSON (JSRational False num) = return $ Float $ fromRational num
  readJSON (JSArray a) = do
    result <- mapM readJSON a
    return $ List $ result
  -- TODO: JSObject

-- |Wrapper for Text.JSON.decode
jsDecode :: [LispVal] -> IOThrowsError LispVal
jsDecode [String json] = do
    let r = decode json :: Result LispVal
    case r of
        Ok result -> return result
        Error msg -> throwError $ Default $ "JSON Parse Error: " ++ msg

-- TODO:
-- jsEncode :: [LispVal] -> IOThrowsError LispVal
-- jsEncode [val] = do

_test :: IO ()
_test = do
    _testDecodeEncode "\"test\""
    _testDecodeEncode "true"
    _testDecodeEncode "null"
    _testDecodeEncode "1"
    _testDecodeEncode "1.5"
    _testDecodeEncode "[1.1, 2, 3, 1.5]"

_testDecodeEncode :: String -> IO ()
_testDecodeEncode str = do
    let x = decode  str :: Result LispVal
    case x of
        Ok x -> putStrLn $ encode x
        Error msg -> putStrLn $ "An error occurred: " ++ msg

