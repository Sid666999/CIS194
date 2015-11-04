{-# OPTIONS_GHC -Wall #-}
module HW04 where

import Data.List
    
newtype Poly a = P [a]

-- Exercise 1 -----------------------------------------

x :: Num a => Poly a
x = P [0, 1]

-- Exercise 2 ----------------------------------------

instance (Num a, Eq a) => Eq (Poly a) where
    P [] == P [] = True
    P [] == P _ = False
    P _ == P [] = False
    P l1 == P l2
        | y == z    = P ys == P zs
        | otherwise = False
        where
          (y : ys) = dropWhileEnd (== 0) l1
          (z : zs) = dropWhileEnd (== 0) l2
                                  
-- Exercise 3 -----------------------------------------

instance (Num a, Eq a, Show a) => Show (Poly a) where
    show (P coeffs) = let indeterminates = map mkIndeterminate [0..(length coeffs - 1)]
                              where
                                mkIndeterminate x
                                    | x == 0    = ""
                                    | x == 1    = "x"
                                    | otherwise = "x^" ++ show x
                          mkTerm (coeff, indeterminate) = actualCoeff ++ indeterminate
                              where
                                actualCoeff
                                    | coeff == 1 && indeterminate == "" = "1"
                                    | coeff == 1                        = ""
                                    | coeff == -1 && indeterminate == "" = "-1"
                                    | coeff == -1                       = "-"
                                    | otherwise                         = show coeff
                      in
                        foldl (++) "" $ intersperse " + " $ map mkTerm $ reverse $ filter (\x -> fst x /= 0) (zip coeffs indeterminates)

-- Exercise 4 -----------------------------------------
plus :: Num a => Poly a -> Poly a -> Poly a
plus (P xs) (P []) = P xs
plus (P []) (P xs) = P xs                     
plus (P p1) (P p2) = P (plusHelper p1 p2)
    where
      plusHelper p1 [] = p1
      plusHelper [] p2 = p2
      plusHelper (x : xs) (y : ys) = (x + y) : (plusHelper xs ys)
                                       
-- Exercise 5 -----------------------------------------

times :: Num a => Poly a -> Poly a -> Poly a
times (P _) (P []) = P []
times (P []) (P _) = P []                      
times (P (x : xs)) (P ys) = foldr (+) (P [0]) polyList
    where
      polyList = mkPolyList [] (P (x : xs)) (P ys)
          where
            mkPolyList _ (P []) (P _) = []
            mkPolyList acc (P (p : ps)) (P p2) = P (map (\x -> x * p) (acc ++ p2)) : mkPolyList (0 : acc) (P ps) (P p2)
                  
-- Exercise 6 -----------------------------------------

instance Num a => Num (Poly a) where
    (+) = plus
    (*) = times
    negate (P xs) = P (map (\y -> -1 * y) xs) 
    fromInteger i = P [fromIntegral i]
    -- No meaningful definitions exist
    abs    = undefined
    signum = undefined

-- Exercise 7 -----------------------------------------

applyP :: Num a => Poly a -> a -> a
applyP (P poly) y = foldr (+) 0 (substitute 1 poly y)
    where
      substitute _ [] _ = []
      substitute acc (p : ps) y = (p * acc) : substitute (acc * y) ps y


-- Exercise 8 -----------------------------------------

class Num a => Differentiable a where
    deriv  :: a -> a
    nderiv :: Int -> a -> a
    nderiv n f = nderiv (n - 1) (deriv f)
                 
-- Exercise 9 -----------------------------------------

instance Num a => Differentiable (Poly a) where
    deriv (P poly) = let degreeList = map fromIntegral [0..(length poly - 1)]
                     in
                       P $ map (\y -> fst y * snd y) $ zip (tail poly) (tail degreeList)

