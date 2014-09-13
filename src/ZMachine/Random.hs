-- ZMachine/Random.hs :: The Z-machine's random number generator
--------------------------------------------------------------------------
-- This file is part of HAZE, the Haskellish Abominable Z-machine Emulator
-- Copyright (C) 2004, Daniel Janus <nathell@zodiac.mimuw.edu.pl>

module ZMachine.Random 

where 

import System.Random

data ZRandomGen = ZRandom | ZPredictable Int Int StdGen
-- The RNG. It can be in two states: `random' (it then produces random
-- values using the standard global RNG) and `predictable' (when seeded
-- with the same number, it produces the same sequence of random values).

zRandom :: ZRandomGen -> Int -> IO (Int, ZRandomGen)
-- [zRandom gen n] uses the RNG [gen] to produce a random value in the
-- range [1..n], and returns the value along with the new generator.

zRandom ZRandom n = do
  res <- randomRIO (1, n)
  return (res, ZRandom) 

zRandom (ZPredictable state s gen) n = 
  if s < 1000 then
    return (1 + state `mod` n, ZPredictable (1 + state `mod` s) s gen)
  else
    return (res, ZPredictable state s newgen) 
      where (res, newgen) = randomR (1, n) gen

zSeed :: Int -> ZRandomGen
-- Returns a generator seeded with a given number.

zSeed seed = if seed == 0 then ZRandom else ZPredictable 1 seed (mkStdGen seed)

