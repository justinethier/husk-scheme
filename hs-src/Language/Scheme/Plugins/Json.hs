-- Testing integration with Text.JSON

import Text.JSON
import Text.JSON.Generic
import Language.Scheme.Types

-- ideas from http://therning.org/magnus/archives/719
instance JSON LispVal where
  showJSON (String s) = JSString $ toJSString s

  readJSON (JSString str) = return $ String $ fromJSString str

test :: IO ()
test = do
    let x = decode  "\"true\"" :: Result LispVal
    case x of
        Ok x -> putStrLn $ encode x
        Error msg -> putStrLn $ "An error occurred: " ++ msg
