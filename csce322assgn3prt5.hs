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
castle (time, captured, board, move) = (time, captured, board)