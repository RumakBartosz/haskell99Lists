module Lib
    ( elementAt, myLength, myReverse, isPalindrome, compress, flatten, NestedList (Elem, List), pack
    ) where

import Data.List

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

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome [] = True
isPalindrome [x] = True
isPalindrome (x:xs)
  | x == last xs = isPalindrome (init xs)
  | otherwise = False

compress :: (Eq a) => [a] -> [a]
compress [] = []
compress [x] = [x]
compress (x:xs)
  | x == head xs = compress xs
  | otherwise = x : compress xs

data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten (List []) = []
flatten (Elem x) = [x]
flatten (List (x:xs)) = flatten x ++ flatten (List xs)

pack :: String -> [String]
pack [] = []
pack [x] = [[x]]
pack (x:xs) = (x : takeWhile (==x) xs) : pack (dropWhile (==x) xs)
