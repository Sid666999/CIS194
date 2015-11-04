{-# OPTIONS_GHC -Wall #-}
module HW01 where

-- Exercise 1 -----------------------------------------

-- Get the last digit from a number
lastDigit :: Integer -> Integer
lastDigit n = n `mod` 10

-- Drop the last digit from a number
dropLastDigit :: Integer -> Integer
dropLastDigit n = (n - lastDigit n) `div` 10
-- Exercise 2 -----------------------------------------

toRevDigits :: Integer -> [Integer]
toRevDigits 0 = []
toRevDigits n
  | n < 0 = []
  | otherwise = lastDigit n : toRevDigits (dropLastDigit n)

-- Exercise 3 -----------------------------------------

-- Double every second number in a list starting on the left.
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther (x : []) = [x]
doubleEveryOther (x : (y : zs)) = x : (y * 2 : doubleEveryOther zs)

-- Exercise 4 -----------------------------------------

-- Calculate the sum of all the digits in every Integer.
sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x : ys) = sum (toRevDigits x) + sumDigits ys
-- Exercise 5 -----------------------------------------

-- Validate a credit card number using the above functions.
luhn :: Integer -> Bool
luhn n
  | result == 0 = True
  | otherwise   = False
  where
    result = lastDigit (sumDigits (doubleEveryOther (toRevDigits n)))

-- Exercise 6 -----------------------------------------

-- Towers of Hanoi for three pegs
type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 1 src _ dst = [(src, dst)]
hanoi 2 src tmp dst = [(src, tmp), (src, dst), (tmp, dst)]
hanoi n src tmp dst = hanoi (n - 1) src dst tmp ++ ((src, dst) : hanoi (n - 1) tmp src dst)
