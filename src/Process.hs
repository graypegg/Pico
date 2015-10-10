module Process (runProgram) where
import Types
import LibPico
import Helpers
import Data.Char (chr, intToDigit)
import Numeric (showHex, showIntAtBase)
import Data.List.Utils (startswith)
import Errors
import Data.Char (ord)
import System.Console.Readline

-- Core Functions --

runProgram :: Program -> Tape -> Int -> String
runProgram _ _ 0 = showError "Maxium number of cycles used!\nSuggestion: Increase maximum cycles"
runProgram _ (TapeError msg) _ = showError msg
runProgram (ProgramError msg) _ _ = showError msg
runProgram (Program c p is Running f) (Tape b cur) (-1) = ( printIns (c!!p) (Program c p is Running f) (Tape b cur) ) ++
													   ( runProgram 
															   (programIns (c!!p) (Program c p is Running f) (Tape b cur))
															   (tapeIns (c!!p) (Program c p is Running f) (Tape b cur))
															   (-1)
													   )
runProgram (Program c p is Running f) (Tape b cur) i    = ( printIns (c!!p) (Program c p is Running f) (Tape b cur) ) ++
													   ( runProgram 
															   (programIns (c!!p) (Program c p is Running f) (Tape b cur))
															   (tapeIns (c!!p) (Program c p is Running f) (Tape b cur))
															   (i-1)
													   )
runProgram (Program _ _ _ Halted _) _ _ 				 = ""

-- Function Processing --

getTapeAfter :: String -> [Function] -> Tape -> Tape
getTapeAfter arg f t = runProgramTape (Program ((function ((filter (\x -> (identifier x) == arg) f)!!0))++["HALT"]) 0 [] Running f) t

runProgramTape :: Program -> Tape -> Tape
runProgramTape (Program c p is Running f) (Tape b cur) = ( runProgramTape
															(programIns (c!!p) (Program c p is Running f) (Tape b cur))
															(tapeIns (c!!p) (Program c p is Running f) (Tape b cur))
													  )
runProgramTape (Program _ _ _ Halted _) t             = t

getStringAfter :: String -> [Function] -> Tape -> String
getStringAfter arg f t = runProgram (Program ((function ((filter (\x -> (identifier x) == arg) f)!!0))++["HALT"]) 0 [] Running f) t (-1)

-- Instruction Processing --

printIns :: String -> Program -> Tape -> String
printIns ('!':arg) _ (Tape b cur)
	| arg == "INT"                     = show (value (b!!cur))
	| arg == "HEX"                     = showHex (value (b!!cur)) ""
	| arg == "BIN"                     = showIntAtBase 2 intToDigit (value (b!!cur)) ""
	| arg == "ASCII"                   = [ (chr (value (b!!cur))) ]
	| arg == "NEWLINE"                 = "\n"
	| startswith "TAPE" arg            = showTape b arg cur
	| otherwise                        = showError $ "Unknown output type\nReferring to: \""++arg++"\""
printIns ('"':arg) _ _                 = init arg
printIns ('$':arg) (Program _ _ _ _ f) t = if (functionLoaded f arg)
											 then getStringAfter arg f t
											 else ""
printIns _ _ _                         = ""

programIns :: String -> Program -> Tape -> Program
programIns "HALT" (Program c p is Running f) t    = (Program c p is Halted f)
programIns ('[':arg) (Program c p is Running f) t = compareBrackets arg c p is t f
programIns ('%':arg) (Program c p is Running f) t = createFunction arg c p is f
programIns ('^':arg) (Program c p is Running f) _ = readFunction arg c p is
programIns ('?':arg) (Program c p is Running f) _ = getInput arg c p is f
programIns _ (Program c p is Running f) _         = (Program c (p+1) is Running f)

getInput :: String -> [String] -> Int -> [Int] -> [Function] -> Program
getInput arg c p is f = do
						chars <- readline ""
						return $ Program c p (concat ( map (\x -> ord x) chars ) is) Running f


tapeIns :: String -> Program -> Tape -> Tape
tapeIns ('+':arg) _ (Tape b cur)      = if (isInteger arg)
											then do let newVal = Cell $ (value (b!!cur)) + (read arg) in
													Tape (replaceNth cur (newVal) b) cur
											else TapeError $ "Unknown operator after \"+\"\nReferring to: \""++arg++"\""
tapeIns ('-':arg) _ (Tape b cur)      = if (isInteger arg)
											then do let newVal = Cell $ (value (b!!cur)) - (read arg) in
													Tape (replaceNth cur (newVal) b) cur
											else TapeError $ "Unknown operator after \"-\"\nReferring to: \""++arg++"\""
tapeIns ('=':arg) _ (Tape b cur)      = if (isInteger arg)
											then do let newVal = Cell $ read arg in
													Tape (replaceNth cur (newVal) b) cur
											else TapeError $ "Unknown operator after \"=\"\nReferring to: \""++arg++"\""
tapeIns ('$':arg) (Program _ _ _ _ f) t = if (functionLoaded f arg)
											then getTapeAfter arg f t
											else TapeError $ "Unknown function\nReferring to: \""++arg++"\""++(functionCorrect arg)
tapeIns ('@':arg) _ (Tape b cur) 
	| arg == ">" = Tape b (cur+1)
	| arg == "<" = Tape b (cur-1)
	| otherwise  = multipleMove arg b cur
tapeIns _ _ t = t