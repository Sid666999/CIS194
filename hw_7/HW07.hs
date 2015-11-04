{-# LANGUAGE MonadComprehensions, RecordWildCards #-}
{-# OPTIONS_GHC -Wall #-}
module HW07 where

import Prelude hiding (mapM)
import Cards

import Control.Monad hiding (mapM, liftM)
import Control.Monad.Random
import Data.Functor
import Data.Monoid
import Data.Vector (Vector, cons, (!), (!?), (//))
import System.Random

import qualified Data.Vector as V


-- Exercise 1 -----------------------------------------

liftM :: Monad m => (a -> b) -> m a -> m b
liftM f ma = do
  a <- ma
  return (f a)
    
swapV :: Int -> Int -> Vector a -> Maybe (Vector a)
swapV i j vec = liftM2 (\vi vj -> vec V.// [(i, vj), (j, vi)]) mi mj
    where
      mi = vec V.!? i
      mj = vec V.!? j

    

-- Exercise 2 -----------------------------------------

mapM :: Monad m => (a -> m b) -> [a] -> m [b]
mapM _ [] = return []
mapM f (x : xs) = do
  b <- (return x) >>= f
  bs <- mapM f xs
  return (b : bs)

getElts :: [Int] -> Vector a -> Maybe [a]
getElts indices vec = mapM (\x -> vec V.!? x) indices
    

-- Exercise 3 -----------------------------------------

type Rnd a = Rand StdGen a

randomElt :: Vector a -> Rnd (Maybe a)
randomElt vec = (getRandomR (0, (V.length vec) - 1)) >>= (return . (vec V.!?))

-- Exercise 4 -----------------------------------------

randomVec :: Random a => Int -> Rnd (Vector a)
randomVec 0 = return V.empty             
randomVec n = V.cons <$> getRandom <*> (randomVec (n - 1))

randomVecR :: Random a => Int -> (a, a) -> Rnd (Vector a)
randomVecR 0 _ = return V.empty
randomVecR n (left, right) = V.cons <$> getRandomR (left, right) <*> (randomVecR (n - 1) (left, right))

-- Exercise 5 -----------------------------------------

shuffle :: Vector a -> Rnd (Vector a)
shuffle vec = helper (return vec) (V.length vec - 1)
    where
      helper rndVec 0 = rndVec 
      helper rndVec i = do
        j <- getRandomR (0, i)
        v <- rndVec     
        helper (return $ v V.// [(i, v V.! j), (j, v V.! i)]) (i - 1)
              
-- Exercise 6 -----------------------------------------

partitionAt :: Ord a => Vector a -> Int -> (Vector a, a, Vector a)
partitionAt vec i = let p = vec V.! i
                        (hd, tl) = V.splitAt i vec
                        (ltVec, geVec) = V.partition (< p) (hd V.++ (V.tail tl))
                    in
                      (ltVec, p, geVec)


-- Exercise 7 -----------------------------------------

-- Quicksort
quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = quicksort [ y | y <- xs, y < x ]
                   <> (x : quicksort [ y | y <- xs, y >= x ])

qsort :: Ord a => Vector a -> Vector a
qsort vec
    | V.null vec == True = V.empty
    | otherwise = qsort [ y | y <- V.tail vec, y < V.head vec ]
                  <> (V.cons (V.head vec) (qsort [ y | y <- V.tail vec, y >= V.head vec ]))

-- Exercise 8 -----------------------------------------

qsortR :: Ord a => Vector a -> Rnd (Vector a)
qsortR vec
    | V.null vec == True = return V.empty
    | otherwise = do
  pivot <- getRandomR (0, V.length vec - 1)
  let (ltVec, p, geVec) = partitionAt vec pivot
  lResult <- qsortR ltVec
  rResult <- qsortR geVec
  return $ lResult V.++ (V.cons p rResult)
         
-- Exercise 9 -----------------------------------------

-- Selection
select :: Ord a => Int -> Vector a -> Rnd (Maybe a)
select i vec
    | V.length vec <= i = return Nothing
    | otherwise = do
  pivot <- getRandomR (0, V.length vec - 1)
  let (ltVec, _, geVec) = partitionAt vec pivot
  let ltVecLen = V.length ltVec
  if ltVecLen == i
  then return $ vec V.!? i
  else if ltVecLen > i
       then select i ltVec
       else select (i - ltVecLen - 1) geVec

-- Exercise 10 ----------------------------------------

allCards :: Deck
allCards = [ Card l s | s <- suits, l <- labels ]

newDeck :: Rnd Deck
newDeck =  shuffle allCards

-- Exercise 11 ----------------------------------------

nextCard :: Deck -> Maybe (Card, Deck)
nextCard deck
    | V.null deck == True = Nothing
    | otherwise = Just (V.head deck, V.tail deck)

-- Exercise 12 ----------------------------------------

getCards :: Int -> Deck -> Maybe ([Card], Deck)
getCards 0 deck = Just ([], deck)            
getCards n deck = do
  (card, remainDeck) <- nextCard deck
  (cards, remainDeck2) <- getCards (n - 1) remainDeck
  return (card : cards, remainDeck2)
                          
-- Exercise 13 ----------------------------------------

data State = State { money :: Int, deck :: Deck }

repl :: State -> IO ()
repl s@State{..} | money <= 0  = putStrLn "You ran out of money!"
                 | V.null deck = deckEmpty
                 | otherwise   = do
  putStrLn $ "You have \ESC[32m$" ++ show money ++ "\ESC[0m"
  putStrLn "Would you like to play (y/n)?"
  cont <- getLine
  if cont == "n"
  then putStrLn $ "You left the casino with \ESC[32m$"
           ++ show money ++ "\ESC[0m"
  else play
    where deckEmpty = putStrLn $ "The deck is empty. You got \ESC[32m$"
                      ++ show money ++ "\ESC[0m"
          play = do
            putStrLn "How much do you want to bet?"
            amt <- read <$> getLine
            if amt < 1 || amt > money
            then play
            else do
              case getCards 2 deck of
                Just ([c1, c2], d) -> do
                  putStrLn $ "You got:\n" ++ show c1
                  putStrLn $ "I got:\n" ++ show c2
                  case () of
                    _ | c1 >  c2  -> repl $ State (money + amt) d
                      | c1 <  c2  -> repl $ State (money - amt) d
                      | otherwise -> war s{deck = d} amt
                _ -> deckEmpty
          war (State m d) amt = do
            putStrLn "War!"
            case getCards 6 d of
              Just ([c11, c21, c12, c22, c13, c23], d') -> do
                putStrLn $ "You got\n" ++ ([c11, c12, c13] >>= show)
                putStrLn $ "I got\n" ++ ([c21, c22, c23] >>= show)
                case () of
                  _ | c13 > c23 -> repl $ State (m + amt) d'
                    | c13 < c23 -> repl $ State (m - amt) d'
                    | otherwise -> war (State m d') amt
              _ -> deckEmpty 

main :: IO ()
main = evalRandIO newDeck >>= repl . State 100
