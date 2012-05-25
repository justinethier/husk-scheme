-- Testing integration with Text.JSON

module Language.Scheme.Plugins.Json where 

import Data.Ratio
import Text.JSON
import Text.JSON.Generic
import Language.Scheme.Types

-- ideas from http://therning.org/magnus/archives/719
instance JSON LispVal where
  showJSON (List []) = JSNull
  showJSON (String s) = JSString $ toJSString s
  showJSON (Bool b) = JSBool b
--  showJSON (Number n) = JSRational False $ fromInteger n 
  showJSON (Rational n) = JSRational True n -- TODO: is this ok??
--  showJSON (Float n) = JSRational True $ toRational n 
  showJSON (List l) = showJSONs l

  readJSON (JSNull) = return $ List []
  readJSON (JSString str) = return $ String $ fromJSString str
  readJSON (JSBool b) = return $ Bool b
  readJSON (JSRational isFloat num) = do
    return $ Rational num -- TODO: need to take isFloat bool into account
                          -- and create instances of int, float, or rational
  readJSON (JSArray a) = do
    result <- mapM readJSON a
    return $ List $ result
  -- JSObject

_test :: IO ()
_test = do
    _testDecodeEncode "\"test\""
    _testDecodeEncode "true"
    _testDecodeEncode "null"
    _testDecodeEncode "1"
    _testDecodeEncode "1.5"
    _testDecodeEncode "[1, 2, 3, 1.5]"

_testDecodeEncode :: String -> IO ()
_testDecodeEncode str = do
    let x = decode  str :: Result LispVal
    case x of
        Ok x -> putStrLn $ encode x
        Error msg -> putStrLn $ "An error occurred: " ++ msg

