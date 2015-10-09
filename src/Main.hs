import Types
import Process
import FileIO
import Helpers (getConfigValue)
import Data.Maybe (fromJust)
import Data.LaunchText
import System.Environment (getArgs)

tape = Tape {
	bytes = [(Cell 0) | x <- [0..]],
	cursor = 0
}

main = do
	filePath <- getArgs
	if (filePath == [])
		then do
   			cfgLaunchText <- getConfigValue "interactive.launch_text"
   			if (fromJust cfgLaunchText)
				then putStrLn $ launchText
				else putStr ""
			prg <- interactive [] []
			putStrLn $ runProgram (Program prg 0 Running []) tape (-1)
		else do
			prg <- loadFile (filePath!!0)
			putStrLn $ runProgram prg tape (-1)