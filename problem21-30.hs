-- problem 21
insertAt :: a -> [a] -> Int -> [a]
insertAt y xs pos = (take (pos - 1) xs) ++ [y] ++ (drop (pos - 1) xs)

-- problem 22
range :: Int -> Int -> [Int]
range a b = [a..b]

