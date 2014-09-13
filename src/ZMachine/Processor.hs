-- ZMachine/Processor.hs :: The execution of a story file
--------------------------------------------------------------------------
-- This file is part of HAZE, the Haskellish Abominable Z-machine Emulator
-- Copyright (C) 2004, Daniel Janus <nathell@zodiac.mimuw.edu.pl>

module ZMachine.Processor (
  createZMachine, -- :: String -> IO ZMachine
  keepRunning     -- :: ZMachine -> IO ZMachine
)

where

import Data.Array
import Data.FiniteMap
import System.IO

import ZMachine.Core
import ZMachine.Random
import ZMachine.Screen
import ZMachine.Stack
import ZMachine.Text
import ZMachine.ZCode.Execution

createZMachine :: String -> IO ZMachine
-- [createZMachine fileName] creates a Z-Machine with memory initialized from 
-- the file [fileName], empty stack and program counter set properly.

createZMachine fileName = do
  h <- openBinaryFile fileName ReadMode
  memImage <- hGetContents h
  let { res = ZMachine
    (myListToFM (map fromEnum memImage)) 
    emptyStack
    0
    ZRandom
    (0, emptyFM)
    (array (0,0) [])
    dummyZScreen
    fileName
    Nothing}
    
  scr <- initZScreen (getZMachineVersion res)
  return (initZMachine (res{zScreen = scr}))

initZMachine :: ZMachine -> ZMachine
-- Stores the initial values of the program counter and Z-character
-- translation table.

initZMachine m = m {
  programCounter = getWordValue m 0x06,
  zCharTable = initZCharTable m
}

initZCharTable :: ZMachine -> Array Int Char
-- Retrieves the Z-character translation table of this ZMachine.

initZCharTable m =
  if version == 1 then ver1ZCharTranslationTable
  else if version >= 2 && version <= 4 then ver2ZCharTranslationTable
  else if offset == 0 then ver2ZCharTranslationTable
  else readTranslationTable m offset
  where version = getZMachineVersion m
        offset = getWordValue m 0x34 

myListToFM :: [a] -> FiniteMap Int a
-- Converts a list to FiniteMap. The resulting FiniteMap will map integer
-- indexes (starting from 0) to elements of the list.

myListToFM list = listToFM pairlist
  where pairlist = zip [0..] list

keepRunning :: ZMachine -> IO ZMachine
-- The heart of a Z-machine: the processor. It retrieves the
-- instruction at the current program counter, advances the PC,
-- executes the instruction, and invokes itself on the new ZMachine.

keepRunning zMachine = do
  let (instruction, newPC) = getInstruction zMachine
--  hPutStrLn stderr $ "ZMachine state: " ++ (show zMachine)
--  hPutStrLn stderr $ "Got instruction: " ++ (show instruction)
--  hPutStrLn stderr $ ""
  hPutStr stderr $ maybe "" (\x -> "DEBUG: "++x++"\n") (debugMsg zMachine) 
  newZMachine <- executeInstruction instruction (zMachine{programCounter=newPC,debugMsg=Nothing})
  keepRunning newZMachine 

