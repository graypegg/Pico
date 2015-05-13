module LibPico (loadLib) where
import LibPico.CellManagement
import LibPico.Math

loadLib :: String -> [String]
loadLib x
	| x == "libpico.math"    = libPicoMath
	| x == "libpico.cellman" = libPicoCellManagement
	| otherwise              = []