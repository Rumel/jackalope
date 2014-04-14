import Data.Char

get :: (Char,Int) -> [[Char]] -> Char
get (file,8) (row:rows) = getRow file row
get (file,rank) (row:rows) = get (file,rank+1) rows

getRow :: Char -> [Char] -> Char
getRow 'a' (col:cols) = col
getRow file (col:cols) = getRow (chr ((ord file)-1)) cols

board = [ "ohsuisho",
          "aaaaaaaa",
          "--------",
          "--------",
          "--------",
          "--------",
          "pppppppp",
          "rgbqkbgr"]
