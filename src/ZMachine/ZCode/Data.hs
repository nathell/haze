-- ZMachine/ZCode/Data.hs :: Definition of a Z-code instruction
--------------------------------------------------------------------------
-- This file is part of HAZE, the Haskellish Abominable Z-machine Emulator
-- Copyright (C) 2004, Daniel Janus <nathell@zodiac.mimuw.edu.pl>

module ZMachine.ZCode.Data (
  OpCode(..),      -- data OpCode
  Operand(..),     -- data Operand
  Instruction(..), -- data Instruction
  decodeOperands   -- :: ZMachine -> Instruction -> ([Int], ZMachine)
)

where

import ZMachine.Core

data OpCode = TwoArgOpCode Int | OneArgOpCode Int | NoArgOpCode Int | VarArgOpCode Int | ExtOpCode Int deriving (Eq, Show)
data Operand = Large Int | Small Int | Variable Int deriving Show
data Instruction = Instruction {
  opcode    :: OpCode,
  operands  :: [Operand],
  storeVar  :: Maybe Int,
  branchOfs :: Maybe (Bool, Int), 
  text      :: Maybe String
} deriving Show
-- The definition of an instruction.

decodeOperand zMachine (Small op) = (op, zMachine)
decodeOperand zMachine (Large op) = (op, zMachine)
decodeOperand zMachine (Variable op) = getVariable zMachine op

decodeOperands :: ZMachine -> Instruction -> ([Int], ZMachine)
-- Return the operands of an instruction as a list of Integers.

decodeOperands zMachine instruction = 
  let res = foldl (\(acc, m) par -> let (x,y) = decodeOperand m par in (x:acc, y))
            ([], zMachine) (operands instruction)
  in (reverse (fst res), snd res)

