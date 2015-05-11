import Types
import Process
import FileIO
import System.Environment (getArgs)

tape = Tape {
	bytes = [(Cell 0) | x <- [0..]],
	cursor = 0
}

main = do
	filePath <- getArgs
	prg <- loadFile (filePath!!0)
	putStrLn $ runProgram prg tape (-1)