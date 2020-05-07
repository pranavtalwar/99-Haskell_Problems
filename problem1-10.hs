import Data.List
import Prelude

-- problem 1
myLast :: [a] -> a
myLast [] = error "empty list"
myLast (x:[]) = x
myLast (x:xs) = myLast xs

-- problem 2
myButLast :: [a] -> a
myButLast [] = error "not enough elements"
myButLast (x:[]) = error "not enough elements"
myButLast (x:y:[]) = x
myButLast (x:y:ys) = myButLast (y:ys)

--problem 3
elementAt :: [a] -> Int -> a
elementAt _ 0 = error "index starts at 1"
elementAt [] _ = error "index too large"
elementAt (x:xs) 1 = x
elementAt (x:xs) num = elementAt xs (num - 1)

-- problem 4
myLength :: [a] -> Int 
myLength [] = 0
myLength (x:xs) = 1  + myLength (xs)

-- problem 5
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse (xs) ++ [x]

--problem 6
isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = myReverse xs == xs

-- problem 7
data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten (List []) = []
flatten (Elem a) = [a]
flatten (List (x:xs)) = (flatten x) ++ (flatten (List xs))

-- problem 8
compress :: Eq a => [a] -> [a]
compress [] = []
compress xs = [x !! 0 | x <- pack xs]

-- problem 9
pack :: Eq a => [a] -> [[a]]
pack [] = []
pack (x:xs) = (takeWhile (==x) (x:xs)) : (pack (drop (length (takeWhile (==x) (x:xs))) (x:xs))) 

-- problem 10
encode :: Eq a => [a] -> [(Int, a)]
encode xs = [(length x, x !! 0) | x <- pack xs]



