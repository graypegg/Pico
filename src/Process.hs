module Process (runProgram) where
import Types
import Helpers
import Data.Char (chr, intToDigit)
import Numeric (showHex, showIntAtBase)

getTapeAfter :: String -> [Function] -> Tape -> Tape
getTapeAfter arg f t = runProgramTape (Program ((function ((filter (\x -> (identifier x) == arg) f)!!0))++["HALT"]) 0 Running []) t

runProgramTape :: Program -> Tape -> Tape
runProgramTape (Program c p Running f) (Tape b cur) = ( runProgramTape
															(programIns (c!!p) (Program c p Running f) (Tape b cur))
															(tapeIns (c!!p) (Program c p Running f) (Tape b cur))
													  )
runProgramTape (Program _ _ Halted _) t             = t

getStringAfter :: String -> [Function] -> Tape -> String
getStringAfter arg f t = runProgram (Program ((function ((filter (\x -> (identifier x) == arg) f)!!0))++["HALT"]) 0 Running []) t (-1)

runProgram :: Program -> Tape -> Int -> String
runProgram _ _ 0 = "\n\n-------------------------------------\nERROR! Maxium number of cycles used!\n| Suggestion: Increase maximum cycles\n"
runProgram (Program c p Running f) (Tape b cur) (-1) = ( printIns (c!!p) (Program c p Running f) (Tape b cur) ) ++
													( runProgram 
															(programIns (c!!p) (Program c p Running f) (Tape b cur))
															(tapeIns (c!!p) (Program c p Running f) (Tape b cur))
															(-1)
													)
runProgram (Program c p Running f) (Tape b cur) i = ( printIns (c!!p) (Program c p Running f) (Tape b cur) ) ++
													( runProgram 
															(programIns (c!!p) (Program c p Running f) (Tape b cur))
															(tapeIns (c!!p) (Program c p Running f) (Tape b cur))
															(i-1)
													)
runProgram (Program _ _ Halted _) _ _ = ""

printIns :: String -> Program -> Tape -> String
printIns ('!':arg) _ (Tape b cur)
	| arg == "INT"                     = show (value (b!!cur))
	| arg == "HEX"                     = showHex (value (b!!cur)) ""
	| arg == "BIN"                     = showIntAtBase 2 intToDigit (value (b!!cur)) ""
	| arg == "ASCII"                   = [ (chr (value (b!!cur))) ]
	| arg == "NEWLINE"                 = "\n"
	| otherwise                        = ""
printIns ('"':arg) _ _                 = init arg
printIns ('$':arg) (Program _ _ _ f) t = getStringAfter arg f t
printIns _ _ _                         = ""

programIns :: String -> Program -> Tape -> Program
programIns "HALT" (Program c p Running f) t    = (Program c p Halted f)
programIns ('[':arg) (Program c p Running f) t = compareBrackets arg c p t f
programIns ('%':arg) (Program c p Running f) t = createFunction arg c p f
programIns ('^':arg) (Program c p Running f) _ = readFunction arg c p
programIns _ (Program c p Running f) _         = (Program c (p+1) Running f)

tapeIns :: String -> Program -> Tape -> Tape
tapeIns ('+':arg) _ (Tape b cur)      = Tape (replaceNth cur (newVal) b) cur
								        where newVal = Cell $ (value (b!!cur)) + (read arg)
tapeIns ('-':arg) _ (Tape b cur)      = Tape (replaceNth cur (newVal) b) cur
								        where newVal = Cell $ (value (b!!cur)) - (read arg)
tapeIns ('=':arg) _ (Tape b cur)      = Tape (replaceNth cur (newVal) b) cur
								        where newVal = Cell $ read arg
tapeIns ('$':arg) (Program _ _ _ f) t = getTapeAfter arg f t
tapeIns ('@':arg) _ (Tape b cur) 
	| arg == ">" = Tape b (cur+1)
	| arg == "<" = Tape b (cur-1)
tapeIns _ _ t = t