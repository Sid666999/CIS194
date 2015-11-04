{-# OPTIONS_GHC -Wall #-}
module HW02 where        
-- Mastermind -----------------------------------------

-- A peg can be one of six colors
data Peg = Red | Green | Blue | Yellow | Orange | Purple
         deriving (Show, Eq, Ord)

-- A code is defined to simply be a list of Pegs
type Code = [Peg]

-- A move is constructed using a Code and two integers; the number of
-- exact matches and the number of regular matches
data Move = Move Code Int Int
          deriving (Show, Eq)

-- List containing all of the different Pegs
colors :: [Peg]
colors = [Red, Green, Blue, Yellow, Orange, Purple]

-- Exercise 1 -----------------------------------------

-- Get the number of exact matches between the actual code and the guess
exactMatches :: Code -> Code -> Int
-- I wanna do this, but zip will cause seg fault when Peg is deriving (Show) under GHC 7.10.2
exactMatches puz ans = length $ filter (\x -> fst x == snd x) (zip puz ans)
-- exactMatches = undefined

-- Exercise 2 -----------------------------------------

-- For each peg in xs, count how many times is occurs in ys
countColors :: Code -> [Int]
countColors ans = map (\x -> length $ filter (\y -> y == x) ans) colors

-- Count number of matches between the actual code and the guess
matches :: Code -> Code -> Int
matches puz ans = foldl (\x y -> if fst y <= snd y then (fst y) + x else (snd y) + x) 0 (filter (\x -> fst x > 0 && snd x > 0) (zip (countColors puz) (countColors ans)))

-- Exercise 3 -----------------------------------------

-- Construct a Move from a guess given the actual code
getMove :: Code -> Code -> Move
getMove puz ans = let correctAns = exactMatches puz ans in
                  Move ans correctAns (matches puz ans - correctAns)

-- Exercise 4 -----------------------------------------

isConsistent :: Move -> Code -> Bool
isConsistent (Move ans exactMatch match) puz
    | exactMatch == exactMatch' && match == match' = True
    | otherwise                                    = False
    where
      Move _ exactMatch' match' = getMove puz ans

-- Exercise 5 -----------------------------------------

filterCodes :: Move -> [Code] -> [Code]
filterCodes move codeList = filter (\x -> isConsistent move x) codeList

-- Exercise 6 -----------------------------------------

allCodes :: Int -> [Code]
allCodes 1 = [[Red], [Green], [Blue], [Yellow], [Orange], [Purple]]
allCodes n = concatMap (\x -> concatMap (\y -> [x : y]) (allCodes (n - 1))) colors

-- Exercise 7 -----------------------------------------

solve :: Code -> [Move]
solve puz = helper [] (allCodes (length puz))
    where helper :: [Move] -> [Code] -> [Move]
          helper acc [] = reverse acc
          helper acc (x : xs) = let move = getMove puz x in
                                move : helper acc (filterCodes move xs)


-- Bonus ----------------------------------------------

fiveGuess :: Code -> [Move]
fiveGuess = undefined
