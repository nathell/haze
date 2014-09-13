-- Main.hs :: The `main' function and friends
--------------------------------------------------------------------------
-- This file is part of HAZE, the Haskellish Abominable Z-machine Emulator
-- Copyright (C) 2004, Daniel Janus <nathell@zodiac.mimuw.edu.pl>

module Main (
  main        -- :: IO ()
)

where

import Prelude hiding (catch)
import Control.Exception
import System
import System.IO hiding (GHC.Exception.catch)

import ZMachine.Core
import ZMachine.Processor
import ZMachine.Screen

main :: IO ()
-- Voila!

main = do
  putStrLn "This is HAZE, the Haskellish Abominable Z-machine Emulator"
  args <- getArgs
  case args of
    [game] -> play game
    _      -> usage

usage :: IO ()
-- Print out information about invoking HAZE.

usage = do
  progName <- getProgName
  putStrLn $ "Usage: " ++ progName ++ " <storyfile>"

play :: String -> IO ()
-- Run the given story file.

play fileName = 
  catchJust ioErrors (doPlay fileName)
    (\e -> if isDoesNotExistError e then 
             putStrLn ("Could not open file: " ++ fileName) 
           else 
             ioError e)

doPlay :: String -> IO ()
-- This function does the dirty work for play. It initializes the
-- Z-Machine with the given story file, checks version and fires up
-- the game, intercepting exceptions which may arise. 

doPlay fileName = do
  zm <- createZMachine fileName
  let ver = getZMachineVersion zm
  if ver `elem` [1..3] 
    then 
      let runIt zm = 
            catchJust userErrors (keepRunning zm >> return ()) $
              (\x -> if x == "quit" then return () else 
                     if x == "restart" then runIt zm else
                     ioError (userError x))
      in runIt zm
    else do
      abnormalDoneScreen
      putStrLn "This file is either not a Z-machine executable, or runs on a version of "
      putStrLn "the Z-machine that is not supported by HAZE. Currently, HAZE supports versions"
      putStrLn "1, 2, and 3 only. "
