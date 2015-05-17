module LibPico (loadLib, functionCorrect) where
import LibPico.CellManagement
import LibPico.Math
import Data.List
import Data.String.Utils

fstT :: ([String],[String],String) -> [String]
fstT (x,_,_) = x

sndT :: ([String],[String],String) -> [String]
sndT (_,x,_) = x

trdT :: ([String],[String],String) -> String
trdT (_,_,x) = x

libPicoAll :: [([String],[String],String)]
libPicoAll = [libPicoMath, libPicoCellManagement]

loadLib :: String -> [String]
loadLib x = if ((length (filter (\y -> (trdT y) == x) libPicoAll)) == 1)
				then fstT $ (filter (\y -> (trdT y) == x) libPicoAll)!!0
				else []

isSimilar :: String -> String -> Bool
isSimilar arg what = 2 < (length $ filter (\x -> (length x) > 1) (group . concat $ zipWith (\a b -> a:b:[]) what arg))

similarTo :: String -> String
similarTo arg = concat $ map (\x -> "\t" ++ x ++ "\n") (concat $ [ (\x -> map (\y -> "\""++y++"\" from "++(trdT x)) $ filter (isSimilar arg) (sndT x)) x | x <- libPicoAll ])

functionCorrect :: String -> String
functionCorrect arg
	| (endswith "\n" (similarTo arg)) = "\nDid you mean:\n"++(init $ similarTo arg)
	| otherwise                          = ""