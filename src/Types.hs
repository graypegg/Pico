module Types where

data State = Running | Halted deriving (Show)

data Cell = Cell {
	value :: Int
} | NoValue deriving (Show)

data Tape = Tape {
	bytes :: [Cell],
	cursor :: Int
} deriving (Show)

data Program = Program {
	code :: [String],
	pointer :: Int,
	state :: State
}