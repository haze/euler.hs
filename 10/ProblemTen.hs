{-|

    Problem 10

    The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.
    Find the sum of all the primes below two million.

|-}

aksPrimeHelp i w x
    | i ^ 2 <= x = (mod x i /= 0) && aksPrimeHelp (i + 6) (6 - w) x
    | otherwise = True

aksPrime n
    | n == 2 = True
    | n == 3 = True
    | mod n 2 == 0 = False
    | mod n 3 == 0 = False
    | otherwise = aksPrimeHelp 5 2 n

isPrime :: Integer -> Bool
isPrime n = null [x | x <- [2 .. max], mod n x == 0]
    where max = floor $ sqrt $ fromInteger n

primes :: [Integer] -> Integer -> Integer -> [Integer]
primes x until n = [x | x <- [2 .. until], isPrime x]

primes' :: Integer -> [Integer]
primes' n = primes [2, 3] n 4

eulerTen :: Integer
eulerTen = sum $ primes' (2000000 - 1)
