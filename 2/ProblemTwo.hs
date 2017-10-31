module Main where
import Data.List (unfoldr)

fibonacci :: [Integer]
fibonacci = unfoldr (\x -> Just (last x, z x)) [1, 1]
    where z (x:xs) = x:xs ++ [ sum $ take 2 $ reverse xs]

main :: IO ()
main = print eulerTwo
  where eulerTwo = sum $ filter (\x -> mod x 2 == 0) $ takeWhile (< 4000000) fibonacci
