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
type Primes s = STUArray s Int64 Int64
type Cursors s = STUArray s Int64 Int64

initMarks :: Int64 -> ST s (Marks s)
initMarks limit =
  if limit < 2
  then error "Limit is too low"
  else do a <- newArray (0, limit) 1
          writeArray a 0 0
          writeArray a 1 0
          return a

marksCapacity :: Marks s -> ST s Int64
marksCapacity aM = getNumElements aM >>= return . fromIntegral

primesCapacity :: Primes s -> ST s Int64
primesCapacity aM = getNumElements aM >>= return . fromIntegral

marksLimit :: Marks s -> Int64 -> ST s Int64
marksLimit aM l = marksCapacity aM >>= \capacity -> return (min l (capacity - 1))

sieve :: Marks s -> Int64 -> Int64 -> Int64 -> ST s Int64
sieve aM limit p cursor = marksLimit aM limit >>= go cursor
  where go c l = if c <= l
                 then writeArray aM c 0 >> go (c + p) l
                 else return (c - l - 1)

nextPrimeOffset :: Marks s -> Int64 -> Int64 -> ST s Int64
nextPrimeOffset aM limit start = marksLimit aM limit >>= go (start + 1 + (start .&. 1))
  where go i l = if i > l
                 then return (l + 1)
                 else do v <- readArray aM i
                         if v == 0
                         then go (i + 2) l
                         else return i;

compactify :: [Int64] -> [Int64] -> Int64 -> ST s (Primes s, Cursors s)
compactify primes cursors n = do pV <- newArray (0, n - 1) 0
                                 cV <- newArray (0, n - 1) 0
                                 go pV cV (n - 1) primes cursors
  where go pV cV i [] [] = return (pV, cV)
        go pV cV i (p:ps) (c:cs) = do writeArray pV i p
                                      writeArray cV i c
                                      go pV cV (i - 1) ps cs

optimusPrimes :: Int64 -> ST s (Marks s, Primes s, Cursors s)
optimusPrimes nN = (if nL < 2 then newArray (0, nL) 0 else initMarks nL) >>= go 2 0 [] []
  where nL = isqrt nN
        go p n ps cs aM = if p <= nL
                          then do np <- nextPrimeOffset aM nL p
                                  c <- sieve aM nL p (p + p)
                                  go np (n + 1) (p:ps) (c:cs) aM
                          else do (primes, cursors) <- compactify ps cs n
                                  return (aM, primes, cursors)

sumMarks :: Marks s -> Int64 -> ST s Int64
sumMarks aM nL = marksLimit aM nL >>= \l -> go l 0 0
  where go l i s = if i <= l
                   then readArray aM i >>= go l (i + 1) . (+ s) . fromIntegral
                   else return s

marksReset :: Marks s -> ST s ()
marksReset aM = getBounds aM >>= mapM_ (\i -> writeArray aM i 1) . range

sieveRecursorCount :: Int64 -> Marks s -> Primes s -> Cursors s -> ST s Int64
sieveRecursorCount nN aM aP aC = marksReset aM >> primesCapacity aP >>= go 0
  where go i l = if i < l
                 then do p <- readArray aP i
                         c <- readArray aC i
                         sieve aM nN p c >>= writeArray aC i
                         go (i + 1) l
                 else sumMarks aM nN;

process :: Int64 -> ST s Int64
process nN = do (aM, aP, aC) <- optimusPrimes nN
                pl <- primesCapacity aP
                if pl == 0
                then return 0
                else let go nL left n = if left > 0
                                        then sieveRecursorCount left aM aP aC >>= go nL (left - nL) . (+ n)
                                        else return n
                      in marksCapacity aM >>= \nL -> go nL (nN - nL) pl

main = getArgs >>= (return . read . head) >>= \nN -> print (runST (process nN))
