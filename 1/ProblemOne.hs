{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

multiplesOf :: Integer -> [Integer]
multiplesOf x
    | x == 0    = [ ] -- or an infnite list of 0's?
    | otherwise = [ z * x | z <- [ 1.. ] ]

multiplesOf' :: Integer -> Integer -> [Integer]
multiplesOf' x y
    | x == 0 = multiplesOf y
    | y == 0 = multiplesOf x
    | otherwise = [ z | z <- [ a.. ], (mod z x == 0) || (mod z y == 0)   ]
    where a = min x y

until' :: Integer -> [Integer] -> [Integer]
until' x = takeWhile ( < x )

eulerOne :: Integer
eulerOne = sum $ until' 1000 $ multiplesOf' 3 5

main :: IO ()
main = print eulerOne
