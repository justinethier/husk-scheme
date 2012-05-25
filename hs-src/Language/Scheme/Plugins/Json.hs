-- Testing integration with Text.JSON

import Text.JSON
import Text.JSON.Generic
import Language.Scheme.Types

-- ideas from http://therning.org/magnus/archives/719
instance JSON LispVal where
  showJSON (String s) = JSString $ toJSString s

  readJSON (JSString str) = return $ String $ fromJSString str

test :: IO ()
test = putStrLn $ encode $ toJSON $ String "test"
