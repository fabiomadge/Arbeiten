module Main where

import Data.List hiding (insert)
import GSA
import System.Random
import Data.Array.IO
import Control.Monad

match x = replicateM x (shuffle [1..x])

main :: IO ()
main = do
  pm <- match 1000
  pw <- match 1000
  let x = findStableMatching (pm, pw)
  print (head x)

-- https://wiki.haskell.org/Random_shuffle
-- | Randomly shuffle a list
--   /O(N)/
shuffle :: [a] -> IO [a]
shuffle xs = do
        ar <- newArray n xs
        forM [1..n] $ \i -> do
            j <- randomRIO (i,n)
            vi <- readArray ar i
            vj <- readArray ar j
            writeArray ar j vi
            return vj
  where
    n = length xs
    newArray :: Int -> [a] -> IO (IOArray Int a)
    newArray n =  newListArray (1,n)
