module Errors where
import Types

showError :: String -> String
showError msg = error ("\x1b[31m"++msg++"\x1b[37m")