import Types
import Process
import FileIO
import System.Environment

prg = Program {
	code = [
		"+8",
		"[/0]~Loop1",
		"@>",
		"+4",
		"[/0]~Loop2",
		"@>",
		"+2",
		"@>",
		"+3",
		"@>",
		"+3",
		"@>",
		"+1",
		"@<",
		"@<",
		"@<",
		"@<",
		"-1",
		"[END]~Loop2",
		"@>",
		"+1",
		"@>",
		"+1",
		"@>",
		"-1",
		"@>",
		"@>",
		"+1",
		"[/0]~Loop3",
		"@<",
		"[END]~Loop3",
		"@<",
		"-1",
		"[END]~Loop1",
		"@>",
		"@>",
		"!ASCII",
		"@>",
		"-3",
		"!ASCII",
		"+7",
		"!ASCII",
		"!ASCII",
		"+3",
		"!ASCII",
		"@>",
		"@>",
		"!ASCII",
		"@<",
		"-1",
		"!ASCII",
		"@<",
		"!ASCII",
		"+3",
		"!ASCII",
		"+6",
		"!ASCII",
		"-8",
		"!ASCII",
		"@>",
		"@>",
		"+1",
		"!ASCII",
		"@>",
		"+2",
		"!ASCII",
		"HALT"
	],
	pointer = 0,
	state = Running
}

tape = Tape {
	bytes = [(Cell 0) | x <- [0..]],
	cursor = 0
}

main = do
	filePath <- getArgs
	prg <- loadFile (filePath!!0)
	putStrLn $ runProgram prg tape (-1)