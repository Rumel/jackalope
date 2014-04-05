import Prelude
import System.Environment ( getArgs )
import Data.List
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
trymove (time, captured, board, move) = (time, captured, board)