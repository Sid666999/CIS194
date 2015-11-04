{-# OPTIONS_GHC -Wall #-}
module HW06 where

import Data.List
import Data.Functor

-- Exercise 1 -----------------------------------------

fib :: Integer -> Integer
fib 0 = 1
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)        

fibs1 :: [Integer]
fibs1 = map fib [0..]

-- Exercise 2 -----------------------------------------

fibs2 :: [Integer]
fibs2 = 1 : 1 : zipWith (+) fibs2 (tail fibs2)

-- Exercise 3 -----------------------------------------

data Stream a = Cons a (Stream a)

-- Show instance prints the first 20 elements followed by ellipsis
instance Show a => Show (Stream a) where
    show s = "[" ++ intercalate ", " (map show $ take 10 $ streamToList s)
             ++ ",..."

streamToList :: Stream a -> [a]
streamToList (Cons x xs) = x : streamToList xs

-- Exercise 4 -----------------------------------------

instance Functor Stream where
    fmap f (Cons x xs) = Cons (f x) (fmap f xs)

-- Exercise 5 -----------------------------------------

sRepeat :: a -> Stream a
sRepeat x = Cons x $ sRepeat x

sIterate :: (a -> a) -> a -> Stream a
sIterate f x = Cons x $ sIterate f (f x)

sInterleave :: Stream a -> Stream a -> Stream a
sInterleave (Cons x xs) y = Cons x (sInterleave y xs) 

sTake :: Int -> Stream a -> [a]
sTake 0 _ = []         
sTake n (Cons x xs) = x : sTake (n - 1) xs

-- Exercise 6 -----------------------------------------

nats :: Stream Integer
nats = sIterate (+ 1) 1
       
ruler :: Stream Integer
ruler = helper 0
    where helper :: Integer -> Stream Integer
          helper n = sInterleave (sRepeat n) (helper (n + 1))

-- Exercise 7 -----------------------------------------

-- | Implementation of C rand
rand :: Int -> Stream Int
rand seed = Cons seed $ rand $ (1103515245 * seed + 12345) `mod` 2147483648

-- Exercise 8 -----------------------------------------

{- Total Memory in use: 221 MB -}
minMaxSlow :: [Int] -> Maybe (Int, Int)
minMaxSlow [] = Nothing   -- no min or max if there are no elements
minMaxSlow xs = Just (minimum xs, maximum xs)

-- Exercise 9 -----------------------------------------

{- Total Memory in use: 1 MB -}
minMax :: [Int] -> Maybe (Int, Int)
minMax [] = Nothing          
minMax (x : xs) = helper xs x x
    where
      helper :: [Int] -> Int -> Int -> Maybe (Int, Int)
      helper [] minValue maxValue = Just (minValue, maxValue) 
      helper (y : ys) minValue maxValue
          | y < minValue = helper ys y maxValue
          | y > maxValue = helper ys minValue y
          | otherwise    = helper ys minValue maxValue
                              

main :: IO ()
main = print $ minMax $ sTake 1000000 $ rand 7666532

-- Exercise 10 ----------------------------------------
data Vector = Vector { x :: Integer, y :: Integer } deriving (Eq, Show)

vdot :: Vector -> Vector -> Integer
vdot v1 v2 = (x v1) * (x v2) + (y v1) * (y v2)
            
data Matrix = Matrix { v1 :: Vector, v2 :: Vector } deriving (Eq, Show)

instance Num Matrix where
    Matrix a1 b1 * Matrix a2 b2 = let a2t = Vector (x a2) (x b2)
                                      b2t = Vector (y a2) (y b2)
                                      c = Vector (vdot a1 a2t) (vdot a1 b2t)
                                      d = Vector (vdot b1 a2t) (vdot b1 b2t)
                                  in
                                    Matrix c d
                                         

fastFib :: Int -> Integer
fastFib n = let fibMatrix = Matrix (Vector 1 1) (Vector 1 0)
            in
              y $ v1 $ fibMatrix ^ n

