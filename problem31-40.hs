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