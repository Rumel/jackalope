import Prelude
import System.Environment ( getArgs )
import Data.List
import Helpers

-- The main method that will be used for testing / command line access
main = do
	args <- getArgs
	chessFile <- readFile (head args)
	chss <- readMakeMoveFile chessFile
	let
		chess = chss
		in yourMain chess

-- yourMain
yourMain chess =
	printResult $ makemove chess

-- YOUR CODE SHOULD COME AFTER THIS POINT

makemove :: (Int, [Char], [[Char]], [((Char,Int),(Char,Int))]) -> (Int, [Char], [[Char]])
makemove (time, captured, board, moves) = (time, captured, board)