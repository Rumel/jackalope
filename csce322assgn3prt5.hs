import Prelude
import System.Environment ( getArgs )
import Data.List
import Helpers

-- The main method that will be used for testing / command line access
main = do
	args <- getArgs
	chessFile <- readFile (head args)
	chss <- readCastleFile chessFile
	let
		chess = chss
		in yourMain chess

-- yourMain
yourMain chess =
	printResult $ castle chess

-- YOUR CODE SHOULD COME AFTER THIS POINT

castle :: (Int, [Char], [[Char]], ((Char,Int),(Char,Int))) -> (Int, [Char], [[Char]])
castle (time, captured, board, ((fromFile, fromRank), (toFile, toRank)))
	| toFile == 'g' && toRank == 1 = se
	| toFile == 'c' && toRank == 1 = sw
	| toFile == 'g' && toRank == 8 = ne
	| toFile == 'c' && toRank == 8 = nw
	| otherwise = (time, captured, board)
	where
		nw = (time - 1, captured, (set ('d', 8) 'o' (set ('a', 8) '-' (set ('c', 8) 'i' (set ('e', 8) '-' board)))))
		ne = (time - 1, captured, (set ('f', 8) 'o' (set ('h', 8) '-' (set ('g', 8) 'i' (set ('e', 8) '-' board)))))
		sw = (time - 1, captured, (set ('d', 1) 'r' (set ('a', 1) '-' (set ('c', 1) 'k' (set ('e', 1) '-' board)))))
		se = (time - 1, captured, (set ('f', 1) 'r' (set ('h', 1) '-' (set ('g', 1) 'k' (set ('e', 1) '-' board)))))
