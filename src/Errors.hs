module Errors (showError) where
import Types

-- | Shows a nice looking error message
showError :: String -> String
showError msg = ("\x1b[37;41m"++(getDashes True (msg'!!0))++"Pico Error"++(getDashes False (msg'!!0))++"\x1b[0m\n\x1b[31;47m"++(msg'!!0)++"\x1b[0m\n"++(init (unlines (drop 1 msg')))++"\x1b[0m")
				where msg' = lines msg

-- | Sets the right amount of dashes for the error header
getDashes :: Bool -> String -> String
getDashes True str = let dashes = (ceiling ((fromIntegral ((length str)-10))/(fromIntegral 2)))::Int in
				     take dashes (repeat '-')
getDashes False str = let dashes = (floor ((fromIntegral ((length str)-10))/(fromIntegral 2)))::Int in
				      take dashes (repeat '-')