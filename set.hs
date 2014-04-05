import Data.Char

set :: (Char,Int) -> Char -> [[Char]] -> [[Char]]
set (file,8) p (row:rows) = (setRow file p row):rows
set (file,rank) p (row:rows) = row:(set (file,rank+1) p rows)

setRow :: Char -> Char -> [Char] -> [Char]
setRow 'a' p (col:cols) = p:cols
setRow file p (col:cols) = col:(setRow (chr ((ord file)-1)) p cols)
