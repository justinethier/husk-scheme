-- Testing integration with Text.JSON

import Text.JSON
import Text.JSON.Generic
import Language.Scheme.Types

-- ideas from http://therning.org/magnus/archives/719
instance JSON LispVal where
  showJSON (String s) = toJSString s

--  readJSON (JSString str) = String str

-- putStrLn $ encode $ toJSON $ String "test"
