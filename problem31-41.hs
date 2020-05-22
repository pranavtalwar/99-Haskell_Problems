import Data.List

-- problem 31
isPrime :: Int -> Bool
isPrime num = [x | x <- [1..num], num `mod` x == 0] == [1,num]

-- problem 32
myGCD :: Int -> Int -> Int 
myGCD a b = if a > b then maximum [x | x <- [1.. abs b], (abs a `mod` x == 0) && (abs b `mod` x == 0)] else  maximum [x | x <- [1..abs a], (abs a `mod` x == 0) && (abs b `mod` x == 0)]

-- problem 33
coprime :: Int -> Int -> Bool
coprime a b = (myGCD a b )== 1

-- problem 34
totient :: Int -> Int
totient 1 = 1
totient num = length [x | x <- [1..(num - 1)], coprime num x]

-- problem 35
multiplicity :: Int -> Int -> Int 
multiplicity a b = if a `mod` b == 0 then 1 + multiplicity (a `div` b) b else 0

primeFactors :: Int -> [Int]
primeFactors num = concat [replicate (multiplicity num x) x| x <- [2..num], isPrime x && num `mod` x == 0]

-- problem 36
prime_factors_mult :: Int -> [(Int, Int)]
prime_factors_mult num = [(factor !! 0, length factor) | factor <- group (primeFactors num)]

-- problem 37
func :: (Int, Int) -> Int
func (x,y) = ((x-1)*(x^(y-1)))

totient_formula :: Int -> Int
totient_formula num = foldr (*) 1 $ map func $ prime_factors_mult $ num

-- problem 38
-- 37 is more efficient than 34 as it is faster

-- problem 39
primesR :: Int -> Int -> [Int]
primesR a b = [x | x <- [a..b], isPrime x]

-- problem 40
goldbach :: Int -> (Int, Int)
goldbach num = [(x, num - x)  | x <-[2..num], isPrime x && isPrime (num - x)] !! 0

-- problem 41
goldbachList :: Int -> Int -> [(Int, Int)]
goldbachList a b = [goldbach x | x <- [a..b], even x]

goldbachList' :: Int -> Int -> Int -> [(Int, Int)]
goldbachList' a b range = [goldbach x | x <- [a..b], even x && ((fst $ goldbach x) > range)]