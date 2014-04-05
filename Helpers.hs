module Helpers
( printBoard
, readTryMoveFile
, readRestartFile
, readMakeMoveFile
, readMoveAllFile
, readCastleFile
, printResult
) where

printBoard :: [[Char]] -> IO ()
printBoard [] 	       = print ""
printBoard (row:rows)  = do
	      	       	 print row
			 printBoard rows

readTryMoveFile :: String -> IO (Int, [Char], [[Char]], ((Char,Int),(Char,Int)))
readTryMoveFile = readIO

readRestartFile :: String -> IO (Int, [Char], [[Char]])
readRestartFile = readIO

readMakeMoveFile :: String -> IO (Int, [Char], [[Char]], [((Char,Int),(Char,Int))])
readMakeMoveFile = readIO

readMoveAllFile :: String -> IO (Int, [Char], [[Char]], [((Char,Int),(Char,Int))])
readMoveAllFile = readIO

readCastleFile :: String -> IO (Int, [Char], [[Char]], ((Char,Int),(Char,Int)))
readCastleFile = readIO

printResult :: (Int, [Char], [[Char]] ) -> IO ()
printResult (time,captured,board) = do
	    			    print "Time Remaining"
				    print time
				    print "Captured Pieces"
				    print captured
				    print "Board"
				    printBoard board