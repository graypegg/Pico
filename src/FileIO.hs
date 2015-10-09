module FileIO (loadFile, interactive) where
import Types
import Helpers
import Data.Maybe (fromJust)
import Data.Text (unpack, pack, stripStart)
import Data.Maybe (isNothing)
import System.Directory (doesFileExist)
import System.Console.Readline

-- | Determines if instruction is a comment
isComment :: String -> Bool
isComment ('#':_) = True
isComment ""      = True
isComment _       = False

-- | Removes unnessicery comments from code
sanitise :: [String] -> [String]
sanitise rawData = foldl (\acc x -> if (isComment x) then (acc) else (acc++[x])) [] rawData

-- | Removes tabs from code
removeTabs :: String -> String
removeTabs = filter (/='\t')

-- | Removes inital spaces from code
removeSpaces :: [String] -> [String]
removeSpaces (x:xs) = (unpack $ stripStart (pack x)):(removeSpaces xs)
removeSpaces []     = []

-- | Loads a file into a Program interface
loadFile :: String -> IO (Program)
loadFile path = do
    fileStat <- doesFileExist path
    case fileStat of
        True  -> do
                    rawData <- readFile path
                    return Program {
                        code = removeSpaces (sanitise (lines (removeTabs rawData))),
                        pointer = 0,
                        state = Running,
                        functions = []
                    }
        False -> do return $ ProgramError ("File does not exist\nReferring to: \""++path++"\"")

-- | Starts/manages interactive mode
interactive :: [String] -> [String] -> IO ([String])
interactive prg prompt = do
    cfgPrompt <- getConfigValue "interactive.prompt"
    listModules <- getConfigValue "interactive.list_modules"
    ins <- readline $ (printPrompt prompt) ++ (fromJust cfgPrompt)
    case ins of
        Nothing      -> do
                            putStrLn "\nQuitting Interactive Mode"
                            return ["HALT"]
        Just ""      -> do
                            prg <- interactive prg prompt
                            return prg
        Just "HALT"  -> do return $ reverse ("HALT":prg)
        Just ('^':p) -> do
                            addHistory ('^':p)
                            if (fromJust listModules)
                                then do prg <- interactive (('^':p):prg) (p:prompt)
                                        return prg
                                else do prg <- interactive (('^':p):prg) prompt
                                        return prg
        Just line    -> do
                            addHistory line
                            prg <- interactive (line:prg) prompt
                            return prg

-- | Creates an interactive prompt
printPrompt :: [String] -> String
printPrompt (p:ps) = p ++ " " ++ (printPrompt ps)
printPrompt []     = ""