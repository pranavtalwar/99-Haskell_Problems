import Data.List
import Prelude

-- problem 11
data NewType a = Single a | Multiple Int a deriving (Show, Eq)

-- using problem 9's solution
pack :: Eq a => [a] -> [[a]]
pack [] = []
pack (x:xs) = (takeWhile (==x) (x:xs)) : (pack (drop (length (takeWhile (==x) (x:xs))) (x:xs))) 

encodeModified :: Eq a => [a] -> [NewType a]
encodeModified xs = [if length x == 1 then Single (x !! 0) else Multiple (length x) (x !! 0) | x <- pack xs]

-- problem 12
getData :: NewType a -> [a]
getData (Single x) = [x]
getData (Multiple num x) = [x | n <- [1..num]]

decodeModified :: Eq a => [NewType a] -> [a]
decodeModified xs = concat [getData x| x <- xs]

-- problem 13
encodeDirect :: Eq a => [a] -> [NewType a]
encodeDirect [] = []
encodeDirect (x:[]) = [Single x]
encodeDirect (x:xs) = if count == 1 then (Single x : encodeDirect xs) else (Multiple count x: (encodeDirect (drop count (x:xs))))
    where count = counter (x:xs) x

counter :: Eq a => [a] -> a -> Int
counter (x:[]) y = if x == y then 1 else 0 
counter (x:xs) y = if x == y then 1 + counter xs y else 0 


--problem 14
dupli :: [a] -> [a]
dupli [] = []
dupli (x:xs) = x : x : dupli xs

-- problem 15
repli :: [a] -> Int -> [a]
repli [] _ = []
repli (x:xs) num = [x | n <- [1..num]] ++ (repli xs num)

-- problem 16
dropEvery :: [a] -> Int -> [a]
dropEvery xs num =  if num > length xs then error "invalid arguments" else result
    where result = [x | (x,xi)<- zip xs [1..(length xs)], xi `mod` num /= 0]

-- problem 17
split :: [a] -> Int -> ([a], [a])
split xs num = if num > length xs then error "invalid argument" else result
    where result = (take num xs, drop num xs)

-- problem 18
slice :: [a] -> Int -> Int -> [a]
slice xs start end = [x | (x,xi) <- zip xs [1..length xs], xi >= start && xi <= end]

-- problem 19
rotate :: [a] -> Int -> [a]
rotate xs num = if num > length xs then error "invalid argument" else
                    if num > 0 then (drop num xs) ++ (take num xs) else (drop (length xs - num) xs) ++ (take (length xs - num) xs)

--problem 20
removeAt :: Int -> [a] -> (a, [a])
removeAt num xs = if num > length xs then error "invalid argument" else (xs !! (num - 1), [x | (x, xi) <- zip xs [1..length xs], xi /= num])