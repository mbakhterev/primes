{-# OPTIONS_GHC -O2 -fllvm #-}

module Main where

import System.Environment
import Data.Bits
import Data.Int

-- isqrt :: (Bits a, Num a, Ord a) => a -> a
isqrt :: Int64 -> Int64
isqrt n = go n 0 (q `shiftR` 2)
 where
   q = head $ dropWhile (< n) $ iterate (`shiftL` 2) 1
   go z r 0 = r
   go z r q = let t = z - r - q
              in if t >= 0
                 then go t (r `shiftR` 1 + q) (q `shiftR` 2)
                 else go z (r `shiftR` 1) (q `shiftR` 2)
 
primes :: Int64 -> [Int64]
primes n = ps
  where
    ps = 2 : [ n | n <- [3, 5 .. n], isPrime n ]
    isPrime n = all (\p -> n `rem` p /= 0) $ takeWhile (<= isqrt n) ps

main :: IO ()
main = do
  [cnt] <- getArgs
  print $ length $ primes $ read cnt
