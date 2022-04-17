{-# LANGUAGE FlexibleContexts #-}

module Main where

import System.Environment
import Data.Bits
import Data.Int
import Data.Array.Base
import Data.Array.ST
import Control.Monad
import Control.Monad.ST


isqrt :: Int64 -> Int64
isqrt n = go n 0 (q `shiftR` 2)
 where
   q = head $ dropWhile (< n) $ iterate (`shiftL` 2) 1
   go z r 0 = r
   go z r q = let t = z - r - q
              in if t >= 0
                 then go t (r `shiftR` 1 + q) (q `shiftR` 2)
                 else go z (r `shiftR` 1) (q `shiftR` 2)

type Marks s = STUArray s Int64 Int8

sumMarks :: Marks s -> ST s Int64
sumMarks aM = getNumElements aM >>= \l -> go (fromIntegral l) 0 0
  where go l i s = if i < l
                 then readArray aM i >>= go l (i + 1) . (+ s) . fromIntegral
                 else return s

newMarks :: Int64 -> ST s (Marks s)
newMarks nN = newArray (0, nN - 1) 1

marksCount :: Marks s -> ST s Int64
marksCount aM = getNumElements aM >>= return . fromIntegral

go :: Int64 -> ST s Int64
go nN =
  if nN < 2
  then return 0
  else let nM = (div nN 2) - 1
           idx n = div (n - 3) 2
           sieve aM m = do v <- readArray aM (idx m)
                           if v == 0
                           then return ()
                           else let start = idx (m * m)
                                in mapM_ (\i -> writeArray aM i 0) [start, start + m .. nM - 1]
       in do aS <- newMarks nM
             mapM_ (sieve aS) [3, 5 .. (isqrt nN)]
             sumMarks aS >>= return . (+ 1)

main = getArgs >>= (return . read . head) >>= \nN -> print (runST (go nN))
