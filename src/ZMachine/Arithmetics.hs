-- ZMachine/Arithmetics.hs :: Z-Machine arithmetics
--------------------------------------------------------------------------
-- This file is part of HAZE, the Haskellish Abominable Z-machine Emulator
-- Copyright (C) 2004, Daniel Janus <nathell@zodiac.mimuw.edu.pl>
--------------------------------------------------------------------------
-- This module implements the arithmetical operations of the Z-Machine.
-- Most of them work as usual except that they are signed 16-bit
-- (HAZE's internal representation is unsigned), but the `div' and `mod'
-- have slightly different semantic from Haskell (and presumably IEEE).

module ZMachine.Arithmetics (
  zPlus,      -- :: Int -> Int -> Int
  zMinus,     -- :: Int -> Int -> Int
  zTimes,     -- :: Int -> Int -> Int
  zDiv,       -- :: Int -> Int -> Int
  zMod,       -- :: Int -> Int -> Int
  toSigned,   -- :: Int -> Int
  toUnsigned  -- :: Int -> Int
) 

where

zArithmetic :: (Int -> Int -> Int) -> Int -> Int -> Int
-- [zArithmetic fun] returns a function that behaves exactly like [fun],
-- but first converts its arguments to signed 16-bit integers, and
-- the result back to unsigned form.

zArithmetic fun x y = (toSigned x `fun` toSigned y) `mod` 65536

zPlus :: Int -> Int -> Int
-- Signed 16-bit addition.

zPlus = zArithmetic (+)

zMinus :: Int -> Int -> Int
-- Signed 16-bit subtraction.

zMinus = zArithmetic (-)

zTimes :: Int -> Int -> Int
-- Signed 16-bit multiplication.

zTimes = zArithmetic (*)

zDiv :: Int -> Int -> Int
-- Signed 16-bit integer division. Note that (-11)/2 = -5, not -6.

zDiv = zArithmetic 
  (\x y -> if (y > 0 && x >= 0) || (y < 0 && x < 0) then 
             x `div` y 
           else 
             -(-x) `div` y)

zMod :: Int -> Int -> Int
-- Signed 16-bit division remainder. Note that (-13)%5 = -3, not 2.

zMod = zArithmetic 
  (\x y -> if x > 0 then x `mod` (abs y) else -(-x) `mod` (abs y))

toSigned :: Int -> Int
toUnsigned :: Int -> Int
-- Conversion signed <-> unsigned 16-bit values.

toSigned x = if x < 32768 then x else x - 65536

toUnsigned x = if x >= 0 then x else x + 65536

