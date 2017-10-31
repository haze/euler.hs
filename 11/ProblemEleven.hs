{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Maybe     as M
import qualified Data.Text      as T
import qualified Data.Text.Read as R
import qualified Text.Printf    as F

data Direction = DiagUpRight | DiagUpLeft | DiagDownRight | DiagDownLeft | Up | Down | Left | Right deriving (Enum, Show)

getGrid :: IO [[ T.Text ]]
getGrid = do
    contents <- readFile "11_res/grid.txt"
    return $ map (T.splitOn " " . T.pack) $ lines contents

generatePoints :: (Num a, Enum a) => (a, a) -> Direction -> a -> [(a, a)]
generatePoints (f_x, f_y) Up            len = (f_x, f_y) : [(f_x, f_y - y) | y <- [ 1..len ]]
generatePoints (f_x, f_y) Down          len = (f_x, f_y) : [(f_x, f_y + y) | y <- [ 1..len ]]
generatePoints (f_x, f_y) Main.Left     len = (f_x, f_y) : [(f_x - x, f_y) | x <- [ 1..len ]]
generatePoints (f_x, f_y) Main.Right    len = (f_x, f_y) : [(f_x + x, f_y) | x <- [ 1..len ]]
generatePoints (f_x, f_y) DiagUpRight   len = (f_x, f_y) : [(f_x + y, f_y + y) | y <- [ 1..len ]]
generatePoints (f_x, f_y) DiagUpLeft    len = (f_x, f_y) : [(f_x - y, f_y + y) | y <- [ 1..len ]]
generatePoints (f_x, f_y) DiagDownRight len = (f_x, f_y) : [(f_x + x, f_y - x) | x <- [ 1..len ]]
generatePoints (f_x, f_y) DiagDownLeft  len = (f_x, f_y) : [(f_x - x, f_y - x) | x <- [ 1..len ]]

getPoint :: [[T.Text]] -> (Int, Int) -> Maybe T.Text
getPoint grid (x, y)
    | x > gsize || y > gsize || x < 0 || y < 0 = Nothing
    | otherwise = Just $ grid !! y !! x
    where gsize = length grid - 1

pointExists :: [[T.Text]] -> (Int, Int) -> Bool
pointExists grid point = M.isJust $ getPoint grid point

pointsExist :: [[T.Text]] -> [(Int, Int)] -> Bool
pointsExist grid xs = all M.isJust $ map (getPoint grid) xs

flatten :: [[a]] -> [a]
flatten xs = (\z n -> foldr (flip (foldr z)) n xs) (:) []

main :: IO ()
main = do
    grid <- getGrid
    let directions = [DiagUpRight ..]
    let gridSize = length grid - 1
    let allStartPoints = flatten [[ (x, y) | x <- [ 0 .. gridSize ] ] | y <- [ 0 .. gridSize ]]
    let everyCombo = flatten [ [ generatePoints point dir 3 | point <- allStartPoints ] | dir <- directions ]
    let filtered = filter (pointsExist grid) everyCombo
    let actuated = take (length filtered - 2) $ map (map (getUnsafe grid)) filtered
    let parsed = map (product . map parseFromText) actuated
    print $ maximum parsed
    where getUnsafe grid (x, y) = (grid !! y) !! x
          parseFromText text = fst $ either (const $ error "Couldn't parse Decimal") id $ R.decimal text
