module Lib
    ( elementAt, myLength, myReverse
    ) where

elementAt :: [a] -> Int -> a
elementAt [] x = error "Can't find value in an empty list"
elementAt [x] 1 = x
elementAt (x:_) 1 = x
elementAt [x] y = error "Not enough elements"
elementAt (x:xs) y = elementAt xs (y - 1)

myLength :: [a] -> Int
myLength [] = 0
myLength [x] = 1
myLength (x:xs) = 1 + myLength xs

myReverse :: [a] -> [a]
myReverse [] = []
myReverse [x] = [x]
myReverse (x:xs) = myReverse xs ++ [x]