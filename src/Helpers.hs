module Helpers where
import Types
import LibPico
import Data.List.Split (splitOn)
import Data.List (elemIndex, isSuffixOf)
import Data.Maybe (fromJust)
import Control.Exception (catch)
import Errors
import System.Directory
import Data.DefaultCfg
import Data.Text (pack)
import Data.Maybe
import qualified Data.Configurator as Cfg

-- General Functions --

-- | Checks if `s` is readable as a Integer
isInteger :: String -> Bool
isInteger s = case reads s :: [(Integer, String)] of
    [(_, "")] -> True
    _         -> False

-- | Replaces the `n`th element in the list `x:xs` with `newVal`
replaceNth :: Int -> a -> [a] -> [a]
replaceNth n newVal (x:xs)
    | n == 0    = newVal:xs
    | otherwise = x:replaceNth (n-1) newVal xs

-- | Extracts the number from an "@" move statement
multipleMove :: String -> [Cell] -> Int -> Tape
multipleMove (dir:arg) b cur
    | dir == '>' && (isInteger arg) = let i = read arg::Int in
                                      Tape b (cur+i)
    | dir == '<' && (isInteger arg) = let i = read arg::Int in
                                      Tape b (cur-i)
    | ( dir == '<' || dir == '>' )
      && (not (isInteger arg))      = TapeError $ "Unknown operator after \"@"++(dir:"\"\nReferring to: \""++arg++"\"")
    | otherwise                     = TapeError $ "Unknown operator after \"@\"\nReferring to: \""++(dir:"\"")

-- | Outputs a string repersentation of the tape
showTape :: [Cell] -> String -> String
showTape b arg = if ((isInteger start) && (isInteger stop))
                    then clipShowTape ((read start):: Int) ((read stop):: Int) $ concat $ map (\x -> (show (value x))++"|") (takeGroup ((read start):: Int) ((read stop):: Int) b)
                    else showError $ "Unknown start and end points\nReferring to: \""++(start)++"\" and \""++(stop)++"\""
                 where start = (splitOn "-" ((splitOn "TAPE" arg)!!1))!!0
                       stop = (splitOn "-" ((splitOn "TAPE" arg)!!1))!!1

-- | Throws error or formats tape by clipping off the end
clipShowTape :: Int -> Int -> String -> String
clipShowTape start _ []     = showError $ "Unknown starting point after !TAPE\nReferring to: \""++(show start)++"\""
clipShowTape start stop str = (show start)++">  "++(init str)++"  <"++(show stop)

-- Function Importing/Creation --

-- | Imports a function into a program
createFunction :: String -> [String] -> Int -> [Function] -> Program
createFunction name c p f = Program {
                                code = c,
                                pointer = (findEnd name c)+2,
                                state = Running,
                                functions = f'
                            }
                            where f' = (Function name (takeGroup (p+1) (findEnd name c) c)):f

-- | Read a function into memory
readFunction :: String -> [String] -> Int -> Program
readFunction arg c p 
    | (loadLib arg) /= [] = let c' = dropAt p c in
                            Program ((loadLib arg)++c') 0 Running []
    | otherwise           = ProgramError $ "Unknown module\nReferring to: \""++arg++"\""

-- | Determine if a function is loaded/imported
functionLoaded :: [Function] -> String -> Bool
functionLoaded f name = elem name (map identifier f)

-- | Drop a line from code
dropAt :: Int -> [a] -> [a]
dropAt n xs = let (ys,zs) = splitAt n xs in
              ys ++ (tail zs)

-- | Returns a sublist of `list` between `start` and `stop`
takeGroup :: Int -> Int -> [a] -> [a]
takeGroup start stop list = fst (splitAt ((stop+1)-start) (snd (splitAt start list)))

-- | Finds the line number of the end of a function
findEnd :: String -> [String] -> Int
findEnd name c = (fromJust (elemIndex ('/':name) c))-1

-- Loop/Conditional Functions --

-- | Returns the number to compare against in a loop
number :: String -> Int
number rest = read $ (splitOn "]~" rest)!!0

-- | Gets the label of a loop
getLabel :: String -> String
getLabel rest = (splitOn "~" rest)!!1

-- | Finds the start of a loop inside `c` using the label, `label`
findStart :: String -> [String] -> Int
findStart label c = fromJust $ elemIndex ([ x | x <- c, (isSuffixOf label x) ]!!0) c

-- | Jump program pointer to top of loop
jumpToTop :: String -> Program -> Program
jumpToTop label (Program c p Running f) = Program c ((findStart label c)) Running f

-- | Jump program pointer to bottom of loop
jumpToEnd :: String -> Program -> Program
jumpToEnd label (Program c p Running f) = Program c ((fromJust (elemIndex ("[END]~"++label) c))+1) Running f

-- | Sets the condition mode for a loop
compareBrackets :: String -> [String] -> Int -> Tape -> [Function] -> Program
compareBrackets ('E':arg) c p t f = jumpToTop (getLabel arg) (Program c p Running f)
compareBrackets ('/':arg) c p t f = ifNotEq arg c p t f
compareBrackets ('=':arg) c p t f = ifEq arg c p t f
compareBrackets ('>':arg) c p t f = ifGT arg c p t f
compareBrackets ('<':arg) c p t f = ifLT arg c p t f

-- | Sets loop into !EQ mode
ifNotEq :: String -> [String] -> Int -> Tape -> [Function] -> Program
ifNotEq rest c p (Tape b cur) f
    | (number rest) /= (value (b!!cur)) = (Program c (p+1) Running f)
    | otherwise                         = jumpToEnd (getLabel rest) (Program c p Running f)

-- | Sets loop into EQ mode
ifEq :: String -> [String] -> Int -> Tape -> [Function] -> Program
ifEq rest c p (Tape b cur) f
    | (number rest) == (value (b!!cur)) = (Program c (p+1) Running f)
    | otherwise                         = jumpToEnd (getLabel rest) (Program c p Running f)

-- | Sets loop into GT mode
ifGT :: String -> [String] -> Int -> Tape -> [Function] -> Program
ifGT rest c p (Tape b cur) f
    | (number rest) < (value (b!!cur)) = (Program c (p+1) Running f)
    | otherwise                        = jumpToEnd (getLabel rest) (Program c p Running f)

-- | Sets loop into LT mode
ifLT :: String -> [String] -> Int -> Tape -> [Function] -> Program
ifLT rest c p (Tape b cur) f
    | (number rest) > (value (b!!cur)) = (Program c (p+1) Running f)
    | otherwise                        = jumpToEnd (getLabel rest) (Program c p Running f)

-- | Gets the config files value for a certain item, does not have a type declaration due to Haskell weirdness.
getConfigValue item = do home <- getHomeDirectory
                         cfgExist <- doesFileExist (home ++ "/.pico_config")
                         if (cfgExist == False)
                             then do
                                 writeFile (home ++ "/.pico_config") defaultCfg
                                 config <- Cfg.load [ Cfg.Required (home ++ "/.pico_config") ]
                                 output <- Cfg.lookup config (pack item)
                                 return output
                             else do
                                 config <- Cfg.load [ Cfg.Required (home ++ "/.pico_config") ]
                                 output <- Cfg.lookup config (pack item)
                                 return output