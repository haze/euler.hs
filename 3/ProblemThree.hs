
isPrime :: Integer -> Bool
isPrime n = null [x | x <- [2 .. max], mod n x == 0]
    where max = floor $ sqrt $ fromInteger n

factors :: Integer -> [Integer]
factors n = 1 : [x | x <- [ 2 .. n ], mod n x == 0]

primeFactors :: Integer -> [Integer]
primeFactors n = filter isPrime $ tail $ factors n

main :: IO ()
main = print $ maximum $ primeFactors 600851475143
