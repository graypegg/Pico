module FileIO (loadFile) where
import Types
import Data.Text (unpack, pack, stripStart)

isComment :: String -> Bool
isComment ('#':_) = True
isComment ""      = True
isComment _       = False

sanitise :: [String] -> [String]
sanitise rawData = foldl (\acc x -> if (isComment x) then (acc) else (acc++[x])) [] rawData

removeTabs :: String -> String
removeTabs = filter (/='\t')

removeSpaces :: [String] -> [String]
removeSpaces (x:xs) = (unpack $ stripStart (pack x)):(removeSpaces xs)
removeSpaces []     = []

loadFile :: String -> IO (Program)
loadFile path = do
    rawData <- readFile path
    return Program {
    	code = removeSpaces (sanitise (lines (removeTabs rawData))),
    	pointer = 0,
    	state = Running
    }