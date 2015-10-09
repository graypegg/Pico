module Data.DefaultCfg where

defaultCfg :: String
defaultCfg = unlines ["interactive {",
                      "\tprompt       = \"> \"",
                      "\tlaunch_text  = off",
                      "\tlist_modules = on",
                      "}"]