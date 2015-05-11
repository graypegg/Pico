module Process (runProgram) where
import Types
import Data.Char (chr)
import Data.List.Split (splitOn)
import Data.List (elemIndex, isSuffixOf)
import Data.Maybe (fromJust)

replaceNth :: Int -> a -> [a] -> [a]
replaceNth n newVal (x:xs)
	| n == 0    = newVal:xs
	| otherwise = x:replaceNth (n-1) newVal xs

number :: String -> Int
number rest = read $ (splitOn "]~" rest)!!0

getLabel :: String -> String
getLabel rest = (splitOn "~" rest)!!1

findStart :: String -> [String] -> Int
findStart label c = fromJust $ elemIndex ([ x | x <- c, (isSuffixOf label x) ]!!0) c

jumpToTop :: String -> Program -> Program
jumpToTop label (Program c p Running) = Program c ((findStart label c)) Running

jumpToEnd :: String -> Program -> Program
jumpToEnd label (Program c p Running) = Program c ((fromJust (elemIndex ("[END]~"++label) c))+1) Running

compareBrackets :: String -> [String] -> Int -> Tape -> Program
compareBrackets ('E':arg) c p t = jumpToTop (getLabel arg) (Program c p Running)
compareBrackets ('/':arg) c p t = ifNotEq arg c p t
compareBrackets ('=':arg) c p t = ifEq arg c p t
compareBrackets ('>':arg) c p t = ifGT arg c p t
compareBrackets ('<':arg) c p t = ifLT arg c p t

ifNotEq :: String -> [String] -> Int -> Tape -> Program
ifNotEq rest c p (Tape b cur)
	| (number rest) /= (value (b!!cur)) = (Program c (p+1) Running)
	| otherwise                         = jumpToEnd (getLabel rest) (Program c p Running)

ifEq :: String -> [String] -> Int -> Tape -> Program
ifEq rest c p (Tape b cur)
	| (number rest) == (value (b!!cur)) = (Program c (p+1) Running)
	| otherwise                         = jumpToEnd (getLabel rest) (Program c p Running)

ifGT :: String -> [String] -> Int -> Tape -> Program
ifGT rest c p (Tape b cur)
	| (number rest) < (value (b!!cur)) = (Program c (p+1) Running)
	| otherwise                         = jumpToEnd (getLabel rest) (Program c p Running)

ifLT :: String -> [String] -> Int -> Tape -> Program
ifLT rest c p (Tape b cur)
	| (number rest) > (value (b!!cur)) = (Program c (p+1) Running)
	| otherwise                         = jumpToEnd (getLabel rest) (Program c p Running)

runProgram :: Program -> Tape -> Int -> String
runProgram _ _ 0 = "\n\n-------------------------------------\nERROR! Maxium number of cycles used!\n| Suggestion: Increase maximum cycles\n"
runProgram (Program c p Running) (Tape b cur) (-1) = ( printIns (c!!p) (Tape b cur) ) ++
												( runProgram 
														(programIns (Program c p Running) (Tape b cur))
														(tapeIns (c!!p) (Tape b cur))
														(-1)
												)
runProgram (Program c p Running) (Tape b cur) i = ( printIns (c!!p) (Tape b cur) ) ++
												( runProgram 
														(programIns (Program c p Running) (Tape b cur))
														(tapeIns (c!!p) (Tape b cur))
														(i-1)
												)
runProgram (Program _ _ Halted) t _ = ""

printIns :: String -> Tape -> String
printIns ('!':arg) (Tape b cur)
	| arg == "INT"   = show (value (b!!cur))
	| arg == "ASCII" = [ (chr (value (b!!cur))) ]
	| otherwise      = "!"
printIns ('"':arg) _ = init arg
printIns _ _         = ""

programIns :: Program -> Tape -> Program
programIns (Program c p Running) t
	| (c!!p) == "HALT"     = (Program c p Halted)
	| (head (c!!p)) == '[' = compareBrackets (tail (c!!p)) c p t
	| otherwise            = (Program c (p+1) Running)

tapeIns :: String -> Tape -> Tape
tapeIns ('+':arg) (Tape b cur) = Tape (replaceNth cur (newVal) b) cur
								 where newVal = Cell $ (value (b!!cur)) + (read arg)
tapeIns ('-':arg) (Tape b cur) = Tape (replaceNth cur (newVal) b) cur
								 where newVal = Cell $ (value (b!!cur)) - (read arg)
tapeIns ('@':arg) (Tape b cur)
	| arg == ">" = Tape b (cur+1)
	| arg == "<" = Tape b (cur-1)
tapeIns _ t = t