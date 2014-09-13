-- ZMachine/Core.hs :: The definition of Z-machine 
--------------------------------------------------------------------------
-- This file is part of HAZE, the Haskellish Abominable Z-machine Emulator
-- Copyright (C) 2004, Daniel Janus <nathell@zodiac.mimuw.edu.pl>
--------------------------------------------------------------------------
-- This module defines the state of a Z-Machine as a Haskell type,
-- as well as functions to access its memory.
-- Implementation note: The memory is defined as a FiniteMap, which is
-- too big and too slow. In most other languages I would have used an ordinary
-- integer-indexed array, but here it would mean a much less functional
-- implementation (and this is supposed to be the course in Advanced
-- Functional Programming, isn't it?)

module ZMachine.Core 

where

import Data.Array
import Data.Bits
import Data.FiniteMap
import System.IO

import ZMachine.Random
import ZMachine.Screen
import ZMachine.Stack

-- The definition of a ZMachine.
data ZMachine = ZMachine {
  memory         :: FiniteMap Int Int, -- map of locations to stored values
  zstack         :: Stack,             
  programCounter :: Int,               
  rndGen         :: ZRandomGen, 
  currentRoutine :: (Int, FiniteMap Int Int), -- local variables and count
  zCharTable     :: Array Int Char,
  zScreen        :: ZScreen,
  storyName      :: String,
  debugMsg       :: Maybe String -- for debugging purposes
}

-- This is mainly for debugging...
instance Show ZMachine where
  show m = 
    "stack size = " ++ (show (stackLength (zstack m))) ++ 
    ", stack frame = " ++ (show (currentFrame (zstack m))) ++ 
    ", current routine = (" ++ (show (fst (currentRoutine m))) ++ 
    ", " ++ (show (fmToList (snd (currentRoutine m)))) ++ 
    "), PC = " ++ (show (programCounter m))

debug :: ZMachine -> String -> ZMachine
-- Set a debugging message. Debugging messages are printed to stderr
-- during execution of the story file (see ZMachine.Processor.keepRunning).

debug zMachine str = zMachine{debugMsg = Just str}

getZMachineVersion :: ZMachine -> Int
-- Return the version of this ZMachine.

getZMachineVersion m = getByteValue m 0

getByteValue, getWordValue :: ZMachine -> Int -> Int
-- Return, respectively, the byte and 2-byte word stored in this
-- ZMachine's memory at the given address.

getByteValue m = lookupWithDefaultFM (memory m) 0 

getWordValue m addr = 256 * (getByteValue m addr) + (getByteValue m (addr + 1))

readBytes, readWords :: ZMachine -> Int -> Int -> ([Int], Int)
-- Similar to get*Value, but when called with arguments [zm n a]
-- return the sequence of n bytes or words, respectively, starting
-- at address a, along with the address of first byte/word after the
-- sequence.

readWords zMachine start argc = 
  foldr (\n (acc,a) -> (getWordValue zMachine a : acc, a + 2)) 
        ([], start) 
        [1..argc]

readBytes zMachine start argc = 
  foldr (\n (acc,a) -> (getByteValue zMachine a : acc, a + 1)) 
        ([], start) 
        [1..argc]

setByteValue, setWordValue :: ZMachine -> Int -> Int -> ZMachine
-- When called with arguments [m addr val], set the byte or word at the
-- address addr in memory to val and return the modified ZMachine.

setByteValue m addr val = m{memory = addToFM (memory m) addr val}

setWordValue m addr val =
  setByteValue (setByteValue m addr hi) (addr + 1) lo
  where hi = (val `shiftR` 8) .&. 255
        lo = val .&. 255
 
zPop :: ZMachine -> (Int, ZMachine)
-- Pops a value off the ZMachine's stack and returns it along with
-- the modified ZMachine.

zPop zMachine = 
  let (v, ns) = pop (zstack zMachine) in (v, zMachine {zstack = ns})

zPush :: ZMachine -> Int -> ZMachine
-- Pushes a value on top of a ZMachine's stack and returns the modified ZM.

zPush zMachine val = zMachine{zstack = push val (zstack zMachine)}

globalVarOffset :: ZMachine -> Int 
-- Returns the address of this ZMachine's global variable table.

globalVarOffset zMachine = getWordValue zMachine 0x0c

unpackAddress :: ZMachine -> Int -> Int
-- Converts a packed address to a byte address.

unpackAddress zMachine paddr = 
  if (version >= 1 && version <= 3) then 
    2 * paddr
  else if (version == 4 || version == 5) then
    4 * paddr 
  else
    8 * paddr
  where version = getZMachineVersion zMachine

flags1 :: ZMachine -> Int
-- Returns the value of Flags 1 field in this ZMachine's memory header.

flags1 zMachine = getByteValue zMachine 1

isTimedGame :: ZMachine -> Bool
-- Answer whether we are playing a `timed' game.

isTimedGame zMachine = 
  if (getZMachineVersion zMachine < 3) then False else 
    (flags1 zMachine `testBit` 1)

addZText :: ZMachine -> String -> ZMachine
-- Write a string to this ZMachine's screen; return the modified ZM.

addZText zMachine text =
  zMachine{zScreen = addText (zScreen zMachine) text}

getRoutine :: ZMachine -> Int -> ([Int], Int, Int)
-- [getRoutine m addr] parses the header of routine at the address [addr]
-- and returns (list of initial values of parameters, number of parameters, 
-- address of the routine's first instruction).

getRoutine zMachine addr =
  let ver = getZMachineVersion zMachine
      argc = getByteValue zMachine addr
      zeros = 0:zeros
  in
    if argc > 15 then 
      error "getRoutine" 
    else
      if ver > 4 then
        (take argc zeros, argc, addr + 1)
      else
        let (args, nextAddr) = readWords zMachine (addr + 1) argc
        in (reverse args, argc, nextAddr)

getVariable :: ZMachine -> Int -> (Int, ZMachine)
-- Get the value of a given variable. Depending on the number, it can be
-- a global variable, a local variable of the current routine, or the
-- top of the stack (in which case the ZMachine changes, so return also that).

getVariable zMachine op 
  | op == 0 = zPop zMachine
  | op >= 1 && op <= 15 = 
      (maybe 
        (error "getVariable") 
        id 
        (lookupFM (snd (currentRoutine zMachine)) op), 
      zMachine)
  | otherwise = 
      (getWordValue zMachine (globalVarOffset zMachine + 2 * (op - 16)),
      zMachine)

storeVariable :: Int -> Int -> ZMachine -> ZMachine
-- [storeVariable var val zm] sets a variable with number [var] (see above) 
-- the ZMachine [zm] to [val] and returns the modified ZMachine.

storeVariable var val zMachine 
  | var == 0 = zPush zMachine val 
  | var >= 1 && var <= 15 = 
      let (cnt, cr) = currentRoutine zMachine
          newCR = if var > cnt 
                    then error "storeVariable" 
                    else (cnt, addToFM cr var val)
      in  zMachine{currentRoutine = newCR}
  | otherwise =
      setWordValue zMachine (globalVarOffset zMachine + 2 * (var - 16)) val

