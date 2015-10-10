import Types
import Process (runProgram)
import FileIO (loadFile)
import Errors (showError)
import System.Exit (exitFailure, exitSuccess)
import Data.Maybe (fromJust)

goal = "CC~BBB<AA"
file = "extra/cellman.pco"

tape = Tape {
    bytes = [(Cell 0) | x <- [0..]],
    cursor = 0
}

checkResult :: String -> IO()
checkResult x
    | x == goal = exitSuccess
    | otherwise = do putStrLn $ showError ("Simple Test DID NOT return the goal value!\nReturned: " ++ x ++ "\nInstead of: " ++ goal)
                     exitFailure

main = do prg <- loadFile (file)
          checkResult $ runProgram prg tape (-1)