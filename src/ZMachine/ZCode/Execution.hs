-- ZMachine/ZCode/Execution.hs :: Instructions and instruction decoding
--------------------------------------------------------------------------
-- This file is part of HAZE, the Haskellish Abominable Z-machine Emulator
-- Copyright (C) 2004, Daniel Janus <nathell@zodiac.mimuw.edu.pl>

module ZMachine.ZCode.Execution (
  getInstruction,     -- :: ZMachine -> (Instruction, Int)
  executeInstruction, -- :: Instruction -> ZMachine -> IO ZMachine
  lookupOpCode        -- :: OpCode -> (Bool, Bool, ZMachine -> Instruction -> 
                      --               IO ZMachine)
) 

where

import Data.Array
import Data.Bits

import ZMachine.Core
import ZMachine.Text
import ZMachine.ZCode.Data
import ZMachine.ZCode.Impl

data InstructionForm = ShortForm | LongForm | VariableForm | ExtendedForm deriving Eq

getInstruction :: ZMachine -> (Instruction, Int)
-- Return the instruction at the current program counter.

getInstruction zMachine = getInstructionAt zMachine (programCounter zMachine)

getShortOperandType :: Int -> [Int]
-- Convert a type of a short operand to list.

getShortOperandType num = if num == 3 then [] else [num]

getVariableOperandType :: ZMachine -> OpCode -> Int -> ([Int], Int)
-- Read the types of a `variable'-form instruction's operands and
-- return them along with the address of the next byte. 

getVariableOperandType zMachine opCode nextAddr =
  if opCode == VarArgOpCode 12 || opCode == VarArgOpCode 26 then
    ((getByteOperands nextAddr) ++ (getByteOperands (nextAddr + 1)), nextAddr + 2)
  else
    (getByteOperands nextAddr, nextAddr + 1)
  where
    getByteOperands addr = 
      let byte = getByteValue zMachine addr
      in
        (getShortOperandType (byte `shiftR` 6)) ++
        (getShortOperandType ((byte .&. 48) `shiftR` 4)) ++
        (getShortOperandType ((byte .&. 12) `shiftR` 2)) ++
        (getShortOperandType (byte .&. 3))

getOperands :: ZMachine -> [Int] -> Int -> ([Operand], Int)
-- Reads the sequence of operands bearing given types and returns them
-- along with the next address.

getOperands zMachine operandTypes addr =
    let (a, b) = foldl collect ([], addr) operandTypes in (reverse a, b)
    where collect (acc, addr) n | n == 0 = (Large (getWordValue zMachine addr):acc, addr + 2)
                                | n == 1 = (Small (getByteValue zMachine addr):acc, addr + 1)
                                | n == 2 = (Variable (getByteValue zMachine addr):acc, addr + 1)

getStoreVariable :: ZMachine -> OpCode -> Int -> (Maybe Int, Int)
-- Reads the 'store' part of the instruction, if there is one, and returns
-- it along with the next part's address.

getStoreVariable zMachine opCode addr = 
  let (has, _, _) = lookupOpCode opCode 
  in
    if has then 
      (Just (getByteValue zMachine addr), addr + 1) 
    else
      (Nothing, addr)

getBranchAddress :: ZMachine -> OpCode -> Int -> (Maybe (Bool, Int), Int)
-- Reads the 'branch' part of the instruction, if there is one, and returns
-- it along with the next part's address.

getBranchAddress zMachine opCode addr = 
  let (_, has, _) = lookupOpCode opCode
  in
    if has then
      let b1 = getByteValue zMachine addr
          on = b1 .&. 128 == 128
          noNext = b1 .&. 64 == 64
      in
        if noNext then 
          (Just (on, b1 .&. 63), addr + 1)
        else
          let b2   = getByteValue zMachine (addr + 1)
              uofs = (b1 .&. 63) * 256 + b2
              ofs  = if uofs < 8192 then uofs else uofs - 16384
          in
            (Just (on, ofs), addr + 2)
    else
      (Nothing, addr)

getInstructionText :: ZMachine -> OpCode -> Int -> (Maybe String, Int)
-- Reads the 'text' part of the instruction, if there is one, and returns
-- it along with the next part's address.

getInstructionText zMachine opCode addr = 
  if opCode == NoArgOpCode 2 || opCode == NoArgOpCode 3 then
    let (text, nextAddr) = getText zMachine addr in (Just text, nextAddr)
  else
    (Nothing, addr)

getInstructionAt :: ZMachine -> Int -> (Instruction, Int)
-- [getInstructionAt m ofs] retrieves the instruction at the address [ofs]
-- and returns the decoded instruction along with the offset of the new one.

getInstructionAt zMachine ofs = 
  let code = getByteValue zMachine ofs
      form = if code .&. 192 == 192 then VariableForm else
             if code .&. 128 == 128 then ShortForm else
             if (code == 190) && (getZMachineVersion zMachine >= 5) then ExtendedForm else 
             LongForm
      opcodeNumber = case form of
                       ShortForm    -> code .&. 15
                       LongForm     -> code .&. 31      
                       VariableForm -> code .&. 31
                       ExtendedForm -> getByteValue zMachine (ofs + 1)
      nextAddr = if form == ExtendedForm then ofs + 2 else ofs + 1
      argCount = case form of -- 3 means Variable
                   ShortForm    -> if code .&. 48 == 48 then 0 else 1
                   LongForm     -> 2
                   VariableForm -> if code .&. 32 == 32 then 3 else 2
                   ExtendedForm -> 3
      opCode = if form == ExtendedForm then 
                 ExtOpCode opcodeNumber 
               else 
                 case argCount of 
                   0 -> NoArgOpCode opcodeNumber
                   1 -> OneArgOpCode opcodeNumber
                   2 -> TwoArgOpCode opcodeNumber
                   3 -> VarArgOpCode opcodeNumber
      (operandTypes, nextAddr2) = case form of -- 0: long const, 1: short const, 2: variable
                       ShortForm    -> (getShortOperandType ((code .&. 48) `shiftR` 4), nextAddr)
		       LongForm     -> ((if code .&. 64 == 0 then [1] else [2]) ++ 
                                       (if code .&. 32 == 0 then [1] else [2]), nextAddr)
                       VariableForm -> getVariableOperandType zMachine opCode nextAddr
      (operands, nextAddr3) = getOperands zMachine operandTypes nextAddr2 
      (store, nextAddr4) = getStoreVariable zMachine opCode nextAddr3
      (branch, nextAddr5) = getBranchAddress zMachine opCode nextAddr4
      (text, nextAddr6) = getInstructionText zMachine opCode nextAddr5

  in 
    (Instruction opCode operands store branch text, nextAddr6)

executeInstruction :: Instruction -> ZMachine -> IO ZMachine
-- Self-explanatory: executes the instruction.

executeInstruction instruction zMachine = (lookupInstruction instruction) zMachine instruction

lookupInstruction :: Instruction -> (ZMachine -> Instruction -> IO ZMachine)
-- Retrieves the implementation of the given Z-code instruction.

lookupInstruction instruction = third (lookupOpCode (opcode instruction))
  where third (x, y, z) = z

lookupOpCode :: OpCode -> (Bool, Bool, ZMachine -> Instruction -> IO ZMachine)
-- Retrieves this opcode's entry in the proper implementation table.

lookupOpCode c =
  case c of
    VarArgOpCode num -> (varArgInstructions ! num)
    NoArgOpCode num -> (noArgInstructions ! num)
    OneArgOpCode num -> (oneArgInstructions ! num)
    TwoArgOpCode num -> (twoArgInstructions ! num)
    _ -> (False, False, bcError) -- EXT opcodes, version 5+

noArgInstructions, oneArgInstructions, twoArgInstructions, varArgInstructions :: Array Int (Bool, Bool, ZMachine -> Instruction -> IO ZMachine) 
-- The implementation tables. Each entry has the form:
-- (is a `score' opcode?, is a `branch' opcode?, implementation)

noArgInstructions = 
  listArray (0, 15) [
    (False, False, bcRTrue),     -- rtrue
    (False, False, bcRFalse),    -- rfalse
    (False, False, bcPrint),     -- print
    (False, False, bcPrintRet),  -- print_ret
    (False, False, bcNop),       -- nop
    (False, True, bcSave),       -- save
    (False, True, bcRestore),    -- restore
    (False, False, bcRestart),   -- restart
    (False, False, bcRetPopped), -- ret_popped
    (False, False, bcError),     -- pop/catch
    (False, False, bcQuit),      -- quit
    (False, False, bcNewLine),   -- new_line
    (False, False, bcError),     -- show_status
    (False, False, bcError),     -- verify
    (False, False, bcError),     -- extended
    (False, False, bcError)      -- piracy
  ]

oneArgInstructions = 
  listArray (0, 15) [
    (False, True, bcJz),          -- jz
    (True, True, bcGetSibling),   -- get_sibling
    (True, True, bcGetChild),     -- get_child
    (True, False, bcGetParent),   -- get_parent
    (True, False, bcGetPropLen),  -- get_prop_len
    (False, False, bcInc),        -- inc   
    (False, False, bcDec),        -- dec   
    (False, False, bcPrintAddr),  -- print_addr
    (True, False, bcCall),        -- call_1s
    (False, False, bcRemoveObj),  -- remove_obj
    (False, False, bcPrintObj),   -- print_obj
    (False, False, bcRet),        -- ret   
    (False, False, bcJump),       -- jump  
    (False, False, bcPrintPaddr), -- print_paddr
    (True, False, bcLoad),      -- load   
    (False, False, bcError)       -- not/call_1n
  ]

twoArgInstructions =
  listArray (0, 31) [
    (False, False, bcError),               -- N/A
    (False, True, bcJe),                   -- je
    (False, True, bcJl),                   -- jl
    (False, True, bcJg),                   -- jg
    (False, True, bcDecChk),               -- dec_chk
    (False, True, bcIncChk),               -- inc_chk
    (False, True, bcJin),                  -- jin
    (False, True, bcTest),                 -- test
    (True, False, bcOr),                   -- or
    (True, False, bcAnd),                  -- and
    (False, True, bcTestAttr),             -- test_attr
    (False, False, bcSetAttr),             -- set_attr
    (False, False, bcClearAttr),           -- clear_attr
    (False, False, bcStore),               -- store
    (False, False, bcInsertObj),           -- insert_obj
    (True, False, bcLoadw),                -- loadw
    (True, False, bcLoadb),                -- loadb
    (True, False, bcGetProp),              -- get_prop
    (True, False, bcGetPropAddr),          -- get_prop_addr
    (True, False, bcGetNextProp),          -- get_next_prop
    (True, False, bcAdd),                  -- add
    (True, False, bcSub),                  -- sub
    (True, False, bcMul),                  -- mul
    (True, False, bcDiv),                  -- div
    (True, False, bcMod),                  -- mod
    (True, False, bcCall),                 -- call_2s
    (False, False, bcCall),                -- call_2n
    (False, False, bcError),               -- set_colour
    (False, False, bcError),               -- throw
    (False, False, bcError),               -- N/A
    (False, False, bcError),               -- N/A
    (False, False, bcError)                -- N/A
  ];

varArgInstructions = 
  listArray (0, 31) [
    (True, False, bcCall),       -- call/call_vs
    (False, False, bcStorew),    -- storew
    (False, False, bcStoreb),    -- storeb
    (False, False, bcPutProp),   -- put_prop
    (False, False, bcRead),      -- sread/aread
    (False, False, bcPrintChar), -- print_char
    (False, False, bcPrintNum),  -- print_num
    (True, False, bcRandom),     -- random
    (False, False, bcPush),      -- push
    (False, False, bcPull),      -- pull 
    (False, False, bcError),     -- split_window
    (False, False, bcError),     -- set_window
    (True, False, bcCall),       -- call_vs2
    (False, False, bcError),     -- erase_window
    (False, False, bcError),     -- erase_line
    (False, False, bcError),     -- set_cursor
    (False, False, bcError),     -- get_cursor
    (False, False, bcError),     -- set_text_style
    (False, False, bcError),     -- buffer_mode
    (False, False, bcError),     -- output_stream
    (False, False, bcError),     -- input_stream
    (False, False, bcError),     -- sound_effect
    (False, False, bcError),     -- read_char
    (False, False, bcError),     -- scan_table
    (False, False, bcError),     -- not
    (False, False, bcCall),      -- call_vn
    (False, False, bcCall),      -- call_vn2
    (False, False, bcError),     -- tokenise
    (False, False, bcError),     -- encode_text
    (False, False, bcError),     -- copy_table
    (False, False, bcError),     -- print_table
    (False, False, bcError)      -- check_arg_count
  ]

