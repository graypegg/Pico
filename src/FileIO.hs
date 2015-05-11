module FileIO (loadFile) where
import Types

isComment :: String -> Bool
isComment ('#':_) = True
isComment ""      = True
isComment _       = False

sanitise :: [String] -> [String]
sanitise rawData = foldl (\acc x -> if (isComment x) then (acc) else (acc++[x])) [] rawData

loadFile :: String -> IO (Program)
loadFile path = do
    rawData <- readFile path
    return Program {
    	code = (sanitise (lines rawData)),
    	pointer = 0,
    	state = Running
    }