module FileIO (loadFile) where
import Types
import Data.Text (unpack, pack, stripStart)

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
    rawData <- readFile path
    return Program {
    	code = removeSpaces (sanitise (lines (removeTabs rawData))),
    	pointer = 0,
    	state = Running,
    	functions = []
    }