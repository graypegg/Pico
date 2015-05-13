module Helpers where
import Types
import LibPico.Math
import Data.List.Split (splitOn)
import Data.List (elemIndex, isSuffixOf)
import Data.Maybe (fromJust)

replaceNth :: Int -> a -> [a] -> [a]
replaceNth n newVal (x:xs)
	| n == 0    = newVal:xs
	| otherwise = x:replaceNth (n-1) newVal xs

takeGroup :: Int -> Int -> [a] -> [a]
takeGroup start stop list = fst (splitAt ((stop+1)-start) (snd (splitAt start list)))

findEnd :: String -> [String] -> Int
findEnd name c = (fromJust (elemIndex ('/':name) c))-1

number :: String -> Int
number rest = read $ (splitOn "]~" rest)!!0

getLabel :: String -> String
getLabel rest = (splitOn "~" rest)!!1

findStart :: String -> [String] -> Int
findStart label c = fromJust $ elemIndex ([ x | x <- c, (isSuffixOf label x) ]!!0) c

jumpToTop :: String -> Program -> Program
jumpToTop label (Program c p Running f) = Program c ((findStart label c)) Running f

jumpToEnd :: String -> Program -> Program
jumpToEnd label (Program c p Running f) = Program c ((fromJust (elemIndex ("[END]~"++label) c))+1) Running f

compareBrackets :: String -> [String] -> Int -> Tape -> [Function] -> Program
compareBrackets ('E':arg) c p t f = jumpToTop (getLabel arg) (Program c p Running f)
compareBrackets ('/':arg) c p t f = ifNotEq arg c p t f
compareBrackets ('=':arg) c p t f = ifEq arg c p t f
compareBrackets ('>':arg) c p t f = ifGT arg c p t f
compareBrackets ('<':arg) c p t f = ifLT arg c p t f

ifNotEq :: String -> [String] -> Int -> Tape -> [Function] -> Program
ifNotEq rest c p (Tape b cur) f
	| (number rest) /= (value (b!!cur)) = (Program c (p+1) Running f)
	| otherwise                         = jumpToEnd (getLabel rest) (Program c p Running f)

ifEq :: String -> [String] -> Int -> Tape -> [Function] -> Program
ifEq rest c p (Tape b cur) f
	| (number rest) == (value (b!!cur)) = (Program c (p+1) Running f)
	| otherwise                         = jumpToEnd (getLabel rest) (Program c p Running f)

ifGT :: String -> [String] -> Int -> Tape -> [Function] -> Program
ifGT rest c p (Tape b cur) f
	| (number rest) < (value (b!!cur)) = (Program c (p+1) Running f)
	| otherwise                        = jumpToEnd (getLabel rest) (Program c p Running f)

ifLT :: String -> [String] -> Int -> Tape -> [Function] -> Program
ifLT rest c p (Tape b cur) f
	| (number rest) > (value (b!!cur)) = (Program c (p+1) Running f)
	| otherwise                        = jumpToEnd (getLabel rest) (Program c p Running f)

createFunction :: String -> [String] -> Int -> [Function] -> Program
createFunction name c p f = Program {
								code = c,
								pointer = (findEnd name c)+2,
								state = Running,
								functions = f'
							}
							where f' = (Function name (takeGroup (p+1) (findEnd name c) c)):f

dropAt :: Int -> [a] -> [a]
dropAt n xs = let (ys,zs) = splitAt n xs in
			  ys ++ (tail zs)

readFunction :: String -> [String] -> Int -> Program
readFunction "libpico.math" c p = Program (libPicoMath++c') 0 Running []
								  where c' = dropAt p c
readFunction _ c p = Program c' 0 Running []
					 where c' = dropAt p c