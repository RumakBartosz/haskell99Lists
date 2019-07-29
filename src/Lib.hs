module Lib
    ( elementAt
    ) where

elementAt :: [a] -> Int -> a
elementAt [] x = error "Can't find value in an empty list"
elementAt [x] 1 = x
elementAt (x:_) 1 = x
elementAt [x] y = error "Not enough elements"
elementAt (x:xs) y = elementAt xs (y - 1)
