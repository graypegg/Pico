module Data.DefaultCfg where

defaultCfg :: String
defaultCfg = unlines ["interactive {",
                      "\tprompt       = \"> \"",
                      "\tlaunch_text  = on",
                      "\tlist_modules = on",
                      "}",
                      "interpreter {",
                      "\tcycle_limit  = off",
                      "\tmax_cycles   = 150",
                      "}"]