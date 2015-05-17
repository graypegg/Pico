module Types where

data State = Running | Halted deriving (Show)

data Function = Function {
	identifier :: String,
	function :: [String]
} deriving (Show)

data Cell = Cell {
	value :: Int
} | NoValue deriving (Show)

data Tape = Tape {
	bytes :: [Cell],
	cursor :: Int
} | TapeError String deriving (Show)

data Program = Program {
	code :: [String],
	pointer :: Int,
	state :: State,
	functions :: [Function]
} | ProgramError String deriving (Show)