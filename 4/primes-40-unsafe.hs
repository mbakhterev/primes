{-# LANGUAGE FlexibleContexts #-}

module Main where

import System.Environment
import Data.Bits
import Data.Int
import Data.Array.Base
import Data.Array.ST
import Control.Monad
import Control.Monad.ST

isqrt :: Int -> Int
isqrt n = go n 0 (q `shiftR` 2)
 where
   q = head $ dropWhile (< n) $ iterate (`shiftL` 2) 1
   go z r 0 = r
   go z r q = let t = z - r - q
              in if t >= 0
                 then go t (r `shiftR` 1 + q) (q `shiftR` 2)
                 else go z (r `shiftR` 1) (q `shiftR` 2)

type Marks s = STUArray s Int Int8
type Primes s = STUArray s Int Int
type Cursors s = STUArray s Int Int

initMarks :: Int -> ST s (Marks s)
initMarks nN =
  if nN > 1
  then do a <- newArray (0, nN - 1) 1
          unsafeWrite a 0 0
          unsafeWrite a 1 0
          return a
  else error "Limit is too low"


marksCapacity :: Marks s -> ST s Int
marksCapacity aM = getNumElements aM >>= return . fromIntegral

primesCapacity :: Primes s -> ST s Int
primesCapacity aP = getNumElements aP >>= return . fromIntegral

marksLimit :: Marks s -> Int -> ST s Int
marksLimit aM l = marksCapacity aM >>= return . min l

sieve :: Marks s -> Int -> Int -> Int -> ST s Int
sieve aM nN p cursor = marksLimit aM nN >>= go cursor
  where go c l = if c < l
                 then unsafeWrite aM c 0 >> go (c + p) l
                 else return (c - l)

nextPrimeOffset :: Marks s -> Int -> Int -> ST s Int
nextPrimeOffset aM nN start = marksLimit aM nN >>= go (start + 1 + (start .&. 1))
  where go i l = if i < l
                 then do v <- unsafeRead aM i
                         if v == 0
                         then go (i + 2) l
                         else return i
                 else return i

sumMarks :: Marks s -> Int -> ST s Int
sumMarks aM nN = marksLimit aM nN >>= \l -> go l 0 0
  where go l i s = if i < l
                   then unsafeRead aM i >>= go l (i + 1) . (+ s) . fromIntegral
                   else return s

compactify :: [Int] -> [Int] -> Int -> ST s (Primes s, Cursors s)
compactify primes cursors n = do pV <- newArray (0, n - 1) 0
                                 cV <- newArray (0, n - 1) 0
                                 go pV cV (n - 1) primes cursors
  where go pV cV i [] [] = return (pV, cV)
        go pV cV i (p:ps) (c:cs) = do unsafeWrite pV i p
                                      unsafeWrite cV i c
                                      go pV cV (i - 1) ps cs

optimusPrimes :: Int -> ST s (Marks s, Primes s, Cursors s)
optimusPrimes nN = (if nN < 2 then newArray (0, nN) 0 else initMarks nN) >>= go 2 0 [] []
  where go p n ps cs aM = if p < nN
                          then do np <- nextPrimeOffset aM nN p
                                  c <- sieve aM nN p (p + p)
                                  go np (n + 1) (p:ps) (c:cs) aM
                          else do (primes, cursors) <- compactify ps cs n
                                  return (aM, primes, cursors)

marksReset :: Marks s -> ST s ()
marksReset aM = getBounds aM >>= mapM_ (\i -> unsafeWrite aM i 1) . range

sieveRecursorCount :: Int -> Marks s -> Primes s -> Cursors s -> ST s Int
sieveRecursorCount nN aM aP aC = marksReset aM >> primesCapacity aP >>= go 0
  where go i l = if i < l
                 then do p <- unsafeRead aP i
                         c <- unsafeRead aC i
                         sieve aM nN p c >>= unsafeWrite aC i
                         go (i + 1) l
                 else sumMarks aM nN;

process :: Int -> ST s Int
process nN =
  let nL = (isqrt nN) + 1
  in do (aM, aP, aC) <- optimusPrimes nL
        pl <- primesCapacity aP
        if pl == 0
        then return 0
        else let go nL l n = if l > 0
                             then sieveRecursorCount l aM aP aC >>= go nL (l - nL) . (+ n)
                             else return n
             in marksCapacity aM >>= \nL -> go nL ((nN + 1) - nL) pl

main = getArgs >>= (return . read . head) >>= \nN -> print (runST (process nN))
