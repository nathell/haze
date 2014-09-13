-- ZMachine/Stack.hs :: The Z-machine's stack
--------------------------------------------------------------------------
-- This file is part of HAZE, the Haskellish Abominable Z-machine Emulator
-- Copyright (C) 2004, Daniel Janus <nathell@zodiac.mimuw.edu.pl>

module ZMachine.Stack 

where

import Data.FiniteMap

data Stack = Stack {
  stack :: [Int],
  frames :: FiniteMap Int Int,
  stackLength :: Int,
  currentFrame :: Int,
  currentFrameStart :: Int
}
-- The definition of the stack.

push :: Int -> Stack -> Stack
-- Pushes a value onto the stack, returning the new stack.

push val s = s {stack = val:(stack s), stackLength = 1 + stackLength s}

pop :: Stack -> (Int, Stack)
-- Pops a value off the stack.

pop s = if currentFrameStart s == stackLength s then error "pop" else
  (head (stack s), s {stack = tail (stack s), stackLength = (stackLength s) - 1})

markFrame :: Stack -> Stack
-- Mark current position of a stack as a start of a new frame.
-- This means it will be impossible to pop values from outside frame
-- until it is released.

markFrame s = s {
  frames = addToFM (frames s) frame (stackLength s), 
  currentFrame = frame, 
  currentFrameStart = stackLength s
} where frame = currentFrame s + 1

releaseFrame :: Stack -> Stack
-- Throw away all values on the stack within the current frame and
-- remove the latest frame mark.

releaseFrame s = s {
  stack = drop (stackLength s - currentFrameStart s) (stack s),
  stackLength = currentFrameStart s,
  frames = delFromFM (frames s) frame,
  currentFrame = frame - 1,
  currentFrameStart = case lookupFM (frames s) (frame - 1) of 
                        Nothing -> 0
			Just fs -> fs
} where frame = currentFrame s

emptyStack :: Stack
-- Return a fresh, empty stack.

emptyStack = Stack [] (listToFM [(0,0)]) 0 0 0
