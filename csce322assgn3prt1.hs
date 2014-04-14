import Prelude
import System.Environment ( getArgs )
import Data.List
import Data.Char
import Helpers

-- The main method that will be used for testing / command line access
main = do
	args <- getArgs
	chessFile <- readFile (head args)
	chss <- readTryMoveFile chessFile
	let
		chess = chss
		in yourMain chess

-- yourMain
yourMain chess =
	printResult $ trymove chess

-- YOUR CODE SHOULD COME AFTER THIS POINT

trymove :: (Int, [Char], [[Char]], ((Char,Int),(Char,Int))) -> (Int, [Char], [[Char]])
trymove (time, captured, board, move)
	| outsideBoard (board, move) = (time - 30, captured, board)
	| invalidMove (board, move) = (time - 20, captured, board)
	| capturedPiece (board, move) = (time - 10, newCaptured, updatedBoard)
	| unnoccupied (board, move) = (time - 1, captured, updatedBoard)
	| otherwise = (time, captured, board)
	where
		updatedBoard = movePiece board move
		newCaptured = addPieceToCaptured captured board move

outsideBoard :: ([[Char]], ((Char, Int), (Char, Int))) -> Bool
outsideBoard (board, ((fromFile, fromRank), (toFile, toRank)))
	| (ord toFile) < (ord 'a') = True
	| (ord toFile) > (ord 'h') = True
  | toRank < 1 = True
	| toRank > 8 = True
	| otherwise = False

invalidMove :: ([[Char]], ((Char, Int), (Char, Int))) -> Bool
invalidMove (board, (from, to))
	| sameColor board from to = True
	| otherwise = False

capturedPiece :: ([[Char]], ((Char, Int), (Char, Int))) -> Bool
capturedPiece (board, (from, to))
	| color (get from board) == "white" = color (get to board) == "black"
	| color (get from board) == "black" = color (get to board) == "white"
	| otherwise = False

unnoccupied :: ([[Char]], ((Char, Int), (Char, Int))) -> Bool
unnoccupied (board, (from, to))
	| get to board == '-' = True
	| otherwise = False

movePiece :: [[Char]] -> ((Char, Int), (Char, Int)) -> [[Char]]
movePiece board (from, to) =
	let
		piece = get from board
		lr = lastRow (color piece) to
		pieceToPlace = place piece lr
	in
		set from '-' (set to pieceToPlace board)

place :: Char -> Bool -> Char
place 'p' True = 'q'
place 'a' True = 'u'
place piece lr = piece

lastRow :: [Char] -> (Char, Int) -> Bool
lastRow color (rank, file)
	| color == "black" = file == 1
	| color == "white" = file == 8

isPawn :: Char -> Bool
isPawn 'p' = True
isPawn 'a' = True
isPawn piece = False

sameColor :: [[Char]] -> (Char, Int) -> (Char, Int) -> Bool
sameColor board from to =
	let
		first = color (get from board)
		second = color (get to board)
	in
		first == second

color :: Char -> [Char]
color piece
	| piece `elem` "ohsuia" = "black"
	| piece `elem` "rgbqkp" = "white"
	| otherwise = "space"

addPieceToCaptured :: [Char] -> [[Char]] -> ((Char, Int), (Char, Int)) -> [Char]
addPieceToCaptured captured board (from, to) =
	let
		capturedPiece = get to board
	in
		captured ++ [capturedPiece]
