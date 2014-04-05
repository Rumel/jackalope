import Prelude
import System.Environment ( getArgs )
import Data.List
import Helpers

-- The main method that will be used for testing / command line access
main = do
	args <- getArgs
	chessFile <- readFile (head args)
	chss <- readRestartFile chessFile
	let
		chess = chss
		in yourMain chess

-- yourMain
yourMain chess =
	printResult $ restart chess

-- YOUR CODE SHOULD COME AFTER THIS POINT

restart :: (Int, [Char], [[Char]]) -> (Int, [Char], [[Char]])
restart (time, captured, board) = (time, captured, board)