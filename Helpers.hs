module Helpers
( printBoard
, readTryMoveFile
, readRestartFile
, readMakeMoveFile
, readMoveAllFile
, readCastleFile
, printResult
, get
, set
) where

import Data.Char

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

get :: (Char,Int) -> [[Char]] -> Char
get (file,8) (row:rows) = getRow file row
get (file,rank) (row:rows) = get (file,rank+1) rows

getRow :: Char -> [Char] -> Char
getRow 'a' (col:cols) = col
getRow file (col:cols) = getRow (chr ((ord file)-1)) cols

set :: (Char,Int) -> Char -> [[Char]] -> [[Char]]
set (file,8) p (row:rows) = (setRow file p row):rows
set (file,rank) p (row:rows) = row:(set (file,rank+1) p rows)

setRow :: Char -> Char -> [Char] -> [Char]
setRow 'a' p (col:cols) = p:cols
setRow file p (col:cols) = col:(setRow (chr ((ord file)-1)) p cols)
