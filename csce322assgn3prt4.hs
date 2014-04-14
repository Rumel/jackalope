import Prelude
import System.Environment ( getArgs )
import Data.Char
import Data.List
import Debug.Trace
import Helpers

-- The main method that will be used for testing / command line access
main = do
	args <- getArgs
	chessFile <- readFile (head args)
	chss <- readMoveAllFile chessFile
	let
		chess = chss
		in yourMain chess

-- yourMain
yourMain chess =
	printResult $ moveall chess False

-- YOUR CODE SHOULD COME AFTER THIS POINT

moveall :: (Int, [Char], [[Char]], [((Char,Int),(Char,Int))]) -> Bool -> (Int, [Char], [[Char]])
moveall (time, captured, board, moves) True = restart (time, captured, board)
moveall (time, captured, board, []) _ = (time, captured, board)
moveall (time, captured, board, (x:xs)) False =
	let
		(newTime, newCaptured, newBoard) = makemove (time, captured, board, [x])
		(blackTime, blackCaptured, blackBoard) = blackMove (newTime, newCaptured, newBoard)
		terminate = 'i' `elem` newCaptured
	in
		moveall (blackTime, blackCaptured, blackBoard, xs) terminate

blackMove :: (Int, [Char], [[Char]]) -> (Int, [Char], [[Char]])
blackMove (time, captured, board) =
	let
		pieceLocations = blackPieceLocationFile "abcdefgh" board []
		king = findWhiteKing board
		possibleMoves = cyclePieceLocations pieceLocations board king
		sorted = sortBy sortMoves possibleMoves
		(_, moveStart, moveEnd, _) = head (reverse sorted)
	in
		trymove (time, captured, board, (moveStart, moveEnd))

sortPiece :: (Char, (Char, Int)) -> (Char, (Char, Int)) -> Ordering
sortPiece (piece1, (xA, yA)) (piece2, (xB, yB))
	| (elemIndex xA "iaoshu") > (elemIndex xB "iaoshu") = GT
	| (elemIndex xA "iaoshu") < (elemIndex xB "iaoshu") = LT
	| ord xA < ord xB = GT
	| otherwise = LT

sortMoves :: (Ord a) => (Char, (Char, Int), (Char, Int), a) -> (Char, (Char, Int), (Char, Int), a) -> Ordering
sortMoves (piece1, _, (newXa, newYa), distance1) (piece2, _, (newXb, newYb), distance2)
	| distance1 < distance2 = GT
	| distance1 > distance2 = LT
	| distance1 == distance2 = sortPiece (piece1, (newXa, newYa)) (piece2, (newXb, newYb))

findWhiteKingRank :: Char -> [Int] -> [[Char]] -> [(Char, Int)]
findWhiteKingRank file [] board = []
findWhiteKingRank file (x:xs) board
	| get (file, x) board == 'k' = [(file, x)]
	| otherwise = [] ++ (findWhiteKingRank file xs board)

findWhiteKingFile :: [[Char]] -> [(Char, Int)]
findWhiteKingFile board =
	let
		a = findWhiteKingRank 'a' [1,2,3,4,5,6,7,8] board
		b = findWhiteKingRank 'b' [1,2,3,4,5,6,7,8] board
		c = findWhiteKingRank 'c' [1,2,3,4,5,6,7,8] board
		d = findWhiteKingRank 'd' [1,2,3,4,5,6,7,8] board
		e = findWhiteKingRank 'e' [1,2,3,4,5,6,7,8] board
		f = findWhiteKingRank 'f' [1,2,3,4,5,6,7,8] board
		g = findWhiteKingRank 'g' [1,2,3,4,5,6,7,8] board
		h = findWhiteKingRank 'h' [1,2,3,4,5,6,7,8] board
	in
		a ++ b ++ c ++ d ++ e ++ f ++ g ++ h

findWhiteKing :: [[Char]] -> (Char, Int)
findWhiteKing board = head (findWhiteKingFile board)

isBlackPiece :: Char -> (Char, Int) -> [(Char, Int)]
isBlackPiece piece tile
	| piece `elem` "ohsuia" = [tile]
	| otherwise = []

blackPieceLocationRank :: Char -> [Int] -> [[Char]] -> [(Char, Int)] -> [(Char, Int)]
blackPieceLocationRank file [] board pieces = pieces
blackPieceLocationRank file (x:xs) board pieces =
	let
		piece = isBlackPiece (get (file, x) board) (file, x)
		newPieces = blackPieceLocationRank file xs board pieces
	in
		piece ++ newPieces

blackPieceLocationFile :: [Char] -> [[Char]] -> [(Char, Int)] -> [(Char, Int)]
blackPieceLocationFile [] board pieces = pieces
blackPieceLocationFile (x:xs) board pieces =
	let
		newPieces = blackPieceLocationRank x [1,2,3,4,5,6,7,8] board pieces
		otherPieces = blackPieceLocationFile xs board pieces
	in
		newPieces ++ otherPieces

cyclePieceLocations :: (Floating a) => [(Char, Int)] -> [[Char]] -> (Char, Int) -> [(Char, (Char, Int), (Char, Int), a)]
cyclePieceLocations [] board king = []
cyclePieceLocations (x:xs) board king = (pieceMoves x board king) ++ (cyclePieceLocations xs board king)

pieceMoves :: (Floating a) => (Char, Int) -> [[Char]] -> (Char, Int) -> [(Char, (Char, Int), (Char, Int), a)]
pieceMoves location board king
	| get location board == 'o' = rookMoves location board king
	| get location board == 'h' = knightMoves location board king [(2,1), (2,-1), (-2,1), (-2,-1), (1,-2), (-1,-2), (-1,2), (1,2)]
	| get location board == 's' = bishopMoves location board king
	| get location board == 'u' = queenMoves location board king
	| get location board == 'i' = kingMoves location board king [(1,1), (1,0), (1,-1), (-1,1), (-1,-1), (-1,0), (0,-1), (0,1)]
	| get location board == 'a' = pawnMoves location board king
	| otherwise = []

getDistance :: (Floating a) => (Char, Int) -> (Char, Int) -> a
getDistance (x1, y1) (kingX, kingY) =
	let
		numKingX = ord kingX
		numX1 = ord x1
	in
		sqrt (fromIntegral (((numKingX - numX1) ^ 2) + ((kingY - y1) ^ 2)))

validDirectionMoveCaptured :: (Char, Int) -> [[Char]] -> Bool
validDirectionMoveCaptured (x, y) board
	| (ord x) > (ord 'h') = False
	| (ord x) < (ord 'a') = False
	| y > 8 = False
	| y < 1 = False
	| (get (x, y) board) `elem` "rgbqkp" = True
	| otherwise = False

validDirectionMoveOpen :: (Char, Int) -> [[Char]] -> Bool
validDirectionMoveOpen (x, y) board
	| (ord x) > (ord 'h') = False
	| (ord x) < (ord 'a') = False
	| y > 8 = False
	| y < 1 = False
	| (get (x, y) board) == '-' = True
	| otherwise = False

directionMove :: (Floating a) => (Char, Int) -> (Char, Int) -> (Int, Int) -> [[Char]] -> (Char, Int) -> [(Char, (Char, Int), (Char, Int), a)]
directionMove (startX, startY) (currentX, currentY) (x, y) board king
	| validDirectionMoveCaptured (newX, newY) board
		= [(piece, (startX, startY), (newX, newY), distance)]
	| validDirectionMoveOpen (newX, newY) board
		= [(piece, (startX, startY), (newX, newY), distance)] ++ (directionMove (startX, startY) (newX, newY) (x, y) board king)
	| otherwise = []
	where
		piece = get (startX, startY) board
		newX = chr (ord currentX + x)
		newY = currentY + y
		distance = getDistance (newX, newY) king

rookMoves :: (Floating a) => (Char, Int) -> [[Char]] -> (Char, Int) -> [(Char, (Char, Int), (Char, Int), a)]
rookMoves location board king =
	let
		down = directionMove location location (0,-1) board king
		left = directionMove location location (-1,0) board king
		right = directionMove location location (1,0) board king
		up = directionMove location location (0,1) board king
	in
		down ++ left ++ right ++ up

knightMoves :: (Floating a) => (Char, Int) -> [[Char]] -> (Char, Int) -> [(Int, Int)] -> [(Char, (Char, Int), (Char, Int), a)]
knightMoves location board king [] = []
knightMoves (startX, startY) board king ((x, y):xs)
	| validDirectionMoveCaptured newLocation board = [('h', (startX, startY), newLocation, distance)] ++ otherMoves
	| validDirectionMoveOpen newLocation board = [('h', (startX, startY), newLocation, distance)] ++ otherMoves
	| otherwise = [] ++ otherMoves
	where
		newLocation = ((chr (ord startX + x)), startY + y)
		distance = getDistance newLocation king
		otherMoves = knightMoves (startX, startY) board king xs

bishopMoves :: (Floating a) => (Char, Int) -> [[Char]] -> (Char, Int) -> [(Char, (Char, Int), (Char, Int), a)]
bishopMoves location board king =
	let
		nw = directionMove location location (-1,1) board king
		ne = directionMove location location (1,1) board king
		sw = directionMove location location (-1,-1) board king
		se = directionMove location location (1,-1) board king
	in
		nw ++ ne ++ sw ++ se

queenMoves :: (Floating a) => (Char, Int) -> [[Char]] -> (Char, Int) -> [(Char, (Char, Int), (Char, Int), a)]
queenMoves location board king = (bishopMoves location board king) ++ (rookMoves location board king)

kingMoves :: (Floating a) => (Char, Int) -> [[Char]] -> (Char, Int) -> [(Int, Int)] -> [(Char, (Char, Int), (Char, Int), a)]
kingMoves location board king [] = []
kingMoves (startX, startY) board king ((x, y):xs)
	| validDirectionMoveCaptured newLocation board = [('i', (startX, startY), newLocation, distance)] ++ otherMoves
	| validDirectionMoveOpen newLocation board = [('i', (startX, startY), newLocation, distance)] ++ otherMoves
	| otherwise = [] ++ otherMoves
	where
		newLocation = ((chr (ord startX + x)), startY + y)
		distance = getDistance newLocation king
		otherMoves = kingMoves (startX, startY) board king xs

pawnMoveOpen :: (Floating a) => (Char, Int) -> [[Char]] -> (Char, Int) -> (Int, Int) -> [(Char, (Char, Int), (Char, Int), a)]
pawnMoveOpen (startX, startY) board king (x, y)
	| ((ord startX) + x) > (ord 'h') = []
	| ((ord startX) + x) < (ord 'a') = []
	| startY + y > 8 = []
	| startY + y < 1 = []
	| (get (newX, newY) board) == '-' = [('a', (startX, startY), (newX, newY), distance)]
	| otherwise = []
	where
		newX = chr ((ord startX) + x)
		newY = startY + y
		distance = getDistance (newX, newY) king

pawnMoveCaptured :: (Floating a) => (Char, Int) -> [[Char]] -> (Char, Int) -> (Int, Int) -> [(Char, (Char, Int), (Char, Int), a)]
pawnMoveCaptured (startX, startY) board king (x, y)
	| ((ord startX) + x) > (ord 'h') = []
	| ((ord startX) + x) < (ord 'a') = []
	| startY + y > 8 = []
	| startY + y < 1 = []
	| (get (newX, newY) board) `elem` whitePieces = [('a', (startX, startY), (newX, newY), distance)]
	| otherwise = []
	where
		newX = chr ((ord startX) + x)
		newY = startY + y
		whitePieces = "rgbqkp"
		distance = getDistance (newX, newY) king

pawnMoveTwo :: (Floating a) => (Char, Int) -> [[Char]] -> (Char, Int) -> (Int, Int) -> [(Char, (Char, Int), (Char, Int), a)]
pawnMoveTwo (startX, startY) board king (x, y)
	| (startY == 7) && ((get (newX, newY) board) == '-') = [('a', (startX, startY), (newX, newY), distance)]
	| otherwise = []
	where
		newX = chr ((ord startX) + x)
		newY = startY + y
		distance = getDistance (newX, newY) king

pawnMoves :: (Floating a) => (Char, Int) -> [[Char]] -> (Char, Int) -> [(Char, (Char, Int), (Char, Int), a)]
pawnMoves location board king =
	let
		oneSpace = pawnMoveOpen location board king (0,-1)
		twoSpace = pawnMoveTwo location board king (0,-2)
		captureLeft = pawnMoveCaptured location board king (1,-1)
		captureRight = pawnMoveCaptured location board king (-1,-1)
		moves = oneSpace ++ twoSpace ++ captureLeft ++ captureRight
	in
		moves

-- CODE BROUGHT OVER FROM OTHER FILES

makemove :: (Int, [Char], [[Char]], [((Char,Int),(Char,Int))]) -> (Int, [Char], [[Char]])
makemove (time, captured, board, []) = (time, captured, board)
makemove (time, captured, board, (x:xs))
	| otherwise = makemove (newTime, newCaptured, newBoard, xs)
	where
		(newTime, newCaptured, newBoard) = trymove (time, captured, board, x)

kingKilled :: [Char] -> Bool
kingKilled captured
	| 'i' `elem` captured = True
	| otherwise = False

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

restart :: (Int, [Char], [[Char]]) -> (Int, [Char], [[Char]])
restart (time, captured, board) =
	( 600,
		"",
		[ "ohsuisho",
			"aaaaaaaa",
			"--------",
			"--------",
			"--------",
			"--------",
			"pppppppp",
			"rgbqkbgr"] )
