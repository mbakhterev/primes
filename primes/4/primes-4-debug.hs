{-# LANGUAGE FlexibleContexts #-}

module Main where

import System.Environment
import Data.Bits
import Data.Int
import Data.Array.Base
import Control.Monad.ST
import Data.Array.ST
import Debug.Trace

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
          unsafeWrite a 0 0
          unsafeWrite a 1 0
          return a

marksCapacity :: Marks s -> ST s Int64
marksCapacity aM = getBounds aM >>= \(_, to) -> return (to + 1)

primesCapacity :: Primes s -> ST s Int64
primesCapacity aM = getBounds aM >>= \(_, to) -> return (to + 1)

marksLimit :: Marks s -> Int64 -> ST s Int64
marksLimit aM l = marksCapacity aM >>= \capacity -> return (min l (capacity - 1))

test :: Marks s -> ST s ()
test aM = unsafeWrite aM 0 3

sieve :: Marks s -> Int64 -> Int64 -> Int64 -> ST s Int64
sieve aM limit p cursor =
  let loop c l = if c <= l
                 then unsafeWrite aM (fromIntegral c) 0 >> loop (c + p) l
                 else return (c - l - 1)
  in marksLimit aM limit >>= loop cursor

nextPrimeOffset :: Marks s -> Int64 -> Int64 -> ST s Int64
nextPrimeOffset aM limit start =
  let loop i l = if i > l -- здесь была ошибка loop l i
                 then return (l + 1)
                 else do v <- unsafeRead aM (fromIntegral i)
                         if v == 0 then loop (i + 2) l else return i -- была ошибка loop l (i + 2)
  in marksLimit aM limit >>= loop (start + 1 + (start .&. 1))

compactify :: [Int64] -> [Int64] -> Int64 -> ST s (Primes s, Cursors s)
compactify primes cursors n =
  do pV <- newArray (0, n - 1) 0
     cV <- newArray (0, n - 1) 0
     let loop i [] [] = return (pV, cV)
         loop i (p:ps) (c:cs) = unsafeWrite pV i p >> unsafeWrite cV i c >> loop (i - 1) ps cs;
     loop (fromIntegral (n - 1)) primes cursors

dump :: (Show a) => a -> a
dump a = trace (show a) a

optimusPrimes :: Int64 -> ST s (Marks s, Primes s, Cursors s)
optimusPrimes nN =
  do let nL = isqrt nN;
     aM <- if nL < 2 then newArray (0, nL) 0 else initMarks nL
     let loop p n ps cs = if p <= nL
                          then do np <- nextPrimeOffset aM nL p
                                  c <- sieve aM nL p (p + p)
                                  loop np (n + 1) (p:ps) (c:cs)
                          else do (primes, cursors) <- compactify ps cs n
                                  return (aM, primes, cursors)
                          -- else (compactify (dump ps) (dump cs) n) >>= \(primes, cursors) -> return (aM, primes, cursors);
     loop 2 0 [] []

sumMarks :: Marks s -> Int64 -> ST s Int64
sumMarks aM nL =
  let loop l i s = if i <= l
                   then unsafeRead aM (fromIntegral i) >>= (loop l (i + 1) . (+ s) . fromIntegral)
                   else return s
  in marksLimit aM nL >>= \l -> loop (fromIntegral l) 0 0

marksReset :: Marks s -> ST s ()
marksReset aM =
  let loop i nL = if i < nL
                  then unsafeWrite aM (fromIntegral i) 1 >> loop (i + 1) nL
                  else return ()
  in marksCapacity aM >>= loop 0

sieveRecursorCount :: Int64 -> Marks s -> Primes s -> Cursors s -> ST s Int64
sieveRecursorCount nN aM aP aC =
  do marksReset aM
     l <- primesCapacity aP
     let loop i = if i < l
                  then do p <- unsafeRead aP (fromIntegral i)
                          c <- unsafeRead aC (fromIntegral i)
                          sieve aM nN p c >>= unsafeWrite aC (fromIntegral i) 
                          loop (i + 1)
                  else sumMarks aM nN;
     loop 0

go :: Int64 -> ST s Int64
go nN = do (aM, aP, aC) <- optimusPrimes nN
           pl <- primesCapacity aP
           if pl == 0
           then return 0
           else do nL <- marksCapacity aM
                   let loop left n = if left > 0
                                     then sieveRecursorCount left aM aP aC >>= (loop (left - nL) . (+ n))
                                     else return n;
                   loop (nN - nL) pl

main = do
  nN <- getArgs >>= (return . read . head) :: IO Int64
  -- print (runSTUArray (do { aM <- initMarks nN; sieve aM nN 2 4; return aM } ))
  -- print (runST (do { aM <- initMarks nN; sieve aM nN 2 4; nextPrimeOffset aM nN 5 } ))
  -- print (runSTUArray (optimusPrimes nN >>= \(aM, aP, aC) -> return aP))
  -- print (runSTUArray (optimusPrimes nN >>= \(aM, aP, aC) -> return aC))
  print (runST (go nN))
