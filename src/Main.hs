import Types
import Process (runProgram)
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
			cfgCyclesBool <- getConfigValue "interpreter.cycle_limit"
			if ((fromJust cfgCyclesBool) == False)
				then putStrLn $ runProgram (Program prg 0 [] Running []) tape (-1)
				else do 
					    cfgCyclesMax <- getConfigValue "interpreter.max_cycles"
					    putStrLn $ runProgram (Program prg 0 [] Running []) tape (fromJust cfgCyclesMax)
		else do
			prg <- loadFile (filePath!!0)
			cfgCyclesBool <- getConfigValue "interpreter.cycle_limit"
			if ((fromJust cfgCyclesBool) == False)
				then putStrLn $ runProgram prg tape (-1)
				else do 
					    cfgCyclesMax <- getConfigValue "interpreter.max_cycles"
					    putStrLn $ runProgram prg tape (fromJust cfgCyclesMax)