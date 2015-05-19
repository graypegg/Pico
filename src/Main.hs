import Types
import Process
import FileIO
import Paths_Pico (version)
import Data.Version (showVersion)
import System.Environment (getArgs)

tape = Tape {
	bytes = [(Cell 0) | x <- [0..]],
	cursor = 0
}

main = do
	filePath <- getArgs
	if (filePath == [])
		then do
			putStrLn $ "Pico version "++ (showVersion version) ++"\nInteractive Mode, Run program with HALT command"
			prg <- interactive [] []
			putStrLn $ runProgram (Program prg 0 Running []) tape (-1)
		else do
			prg <- loadFile (filePath!!0)
			putStrLn $ runProgram prg tape (-1)