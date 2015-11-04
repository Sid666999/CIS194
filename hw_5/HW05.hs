{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module HW05 where

import Data.Bits (xor)
import Data.ByteString.Lazy (ByteString)
import Data.List (sortBy)    
import Data.Map.Strict (Map)
import System.Environment (getArgs)

import qualified Data.ByteString.Lazy as BS
import qualified Data.Map.Strict as Map

import Parser

-- Exercise 1 -----------------------------------------

getSecret :: FilePath -> FilePath -> IO ByteString
getSecret origPath modPath = do
  origFile <- BS.readFile origPath
  modFile <- BS.readFile modPath
  return $ BS.pack $ filter (\x -> x /= 0) $ BS.zipWith xor origFile modFile
             
-- Exercise 2 -----------------------------------------

decryptWithKey :: ByteString -> FilePath -> IO ()
decryptWithKey key path = do
  encFile <- BS.readFile (path ++ ".enc")
  BS.writeFile path $ BS.pack $ BS.zipWith xor (BS.cycle key) encFile 

myTest :: FilePath -> FilePath -> FilePath -> IO ()
myTest origPath modPath path = do
  key <- getSecret origPath modPath
  decryptWithKey key path

-- Exercise 3 -----------------------------------------

parseFile :: FromJSON a => FilePath -> IO (Maybe a)
parseFile path = do
  file <- BS.readFile path
  return $ decode file

-- Exercise 4 -----------------------------------------

getBadTs :: FilePath -> FilePath -> IO (Maybe [Transaction])
getBadTs victimPath transPath = do
  victTids <- parseFile victimPath
  trans <- parseFile transPath
  return $ filter (\x -> isBad victTids (tid x)) <$> trans
      where
        isBad :: Maybe [TId] -> TId -> Bool 
        isBad Nothing _ = False
        isBad (Just []) _ = False
        isBad (Just (x : xs)) tid
            | x == tid  = True
            | otherwise = isBad (Just xs) tid
        
-- Exercise 5 -----------------------------------------

getFlow :: [Transaction] -> Map String Integer
getFlow [] = Map.empty
getFlow (x : xs) = Map.insertWith (+) (from x) (-1 * amount x) (Map.insertWith (+) (to x) (amount x) (getFlow xs))        

-- Exercise 6 -----------------------------------------

getCriminal :: Map String Integer -> String
getCriminal flowMap = snd $ Map.findMax $ reverseMap flowMap
    where
      reverseMap :: Map String Integer -> Map Integer String
      reverseMap m = Map.fromList $ map (\x -> (snd x, fst x)) (Map.toList m)
      
-- Exercise 7 -----------------------------------------

undoTs :: Map String Integer -> [TId] -> [Transaction]
undoTs flowMap tids = let payers = sortBy (\x y -> if x > y then LT else GT) $ Map.toList $ Map.filter (> 0) flowMap
                          payees = sortBy (\x y -> if x > y then LT else GT) $ Map.toList $ Map.filter (< 0) flowMap
                      in
                        undoTsHelper payers payees tids []
                            where
                              undoTsHelper :: [(String, Integer)] -> [(String, Integer)] -> [TId] -> [Transaction] -> [Transaction]
                              undoTsHelper [] [] _ acc = acc
                              undoTsHelper [] _ _ acc = acc
                              undoTsHelper _ [] _ acc = acc
                              undoTsHelper _ _ [] acc = acc
                              undoTsHelper (x : xs) (y : ys) (z : zs) acc
                                  | snd x > snd y = undoTsHelper ((fst x, snd x - snd y) : xs) ys zs $ (Transaction {from = fst x, to = fst y, amount = snd y, tid = z}) : acc
                                  | snd x < snd y = undoTsHelper xs ((fst y, snd y - snd x) : ys) zs $ (Transaction {from = fst x, to = fst y, amount = snd x, tid = z}) : acc
                                  | otherwise     = undoTsHelper xs ys zs $ (Transaction {from = fst x, to = fst y, amount = snd y, tid = z}) : acc

-- Exercise 8 -----------------------------------------

writeJSON :: ToJSON a => FilePath -> a -> IO ()
writeJSON path trans = do
  BS.writeFile path (encode trans)

-- Exercise 9 -----------------------------------------

doEverything :: FilePath -> FilePath -> FilePath -> FilePath -> FilePath
             -> FilePath -> IO String
doEverything dog1 dog2 trans vict fids out = do
  key <- getSecret dog1 dog2
  decryptWithKey key vict
  mts <- getBadTs vict trans
  case mts of
    Nothing -> error "No Transactions"
    Just ts -> do
      mids <- parseFile fids
      case mids of
        Nothing  -> error "No ids"
        Just ids -> do
          let flow = getFlow ts       
          writeJSON out (undoTs flow ids)
          return (getCriminal flow)

main :: IO ()
main = do
  args <- getArgs
  crim <- 
    case args of
      dog1:dog2:trans:vict:ids:out:_ ->
          doEverything dog1 dog2 trans vict ids out
      _ -> doEverything "dog-original.jpg"
                        "dog.jpg"
                        "transactions.json"
                        "victims.json"
                        "new-ids.json"
                        "new-transactions.json"
  putStrLn crim

