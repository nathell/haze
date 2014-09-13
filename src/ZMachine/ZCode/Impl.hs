-- ZMachine/ZCode/Impl.hs :: Implementation of Z-code instructions
--------------------------------------------------------------------------
-- This file is part of HAZE, the Haskellish Abominable Z-machine Emulator
-- Copyright (C) 2004, Daniel Janus <nathell@zodiac.mimuw.edu.pl>
--------------------------------------------------------------------------
-- This module contains implementations of Z-machine opcodes. Each of
-- these takes a state of Z-machine and an instruction, decodes the
-- operands of the instruction, then executes it returning a new
-- ZMachine. Most of the implementations are trivial, and so the documentation
-- throughout here is sparse.

module ZMachine.ZCode.Impl ( 
  bcCall, bcRTrue, bcRFalse, bcRet, bcRetPopped, bcAdd, bcSub, bcMul, bcDiv, 
  bcMod, bcAnd, bcOr, bcJe, bcJl, bcJg, bcJz, bcJin, bcTest, bcInc, bcDec, 
  bcDecChk, bcIncChk, bcTestAttr, bcSetAttr, bcClearAttr, bcLoadw, bcLoadb, 
  bcJump, bcLoad, bcStore, bcStoreb, bcStorew, bcPutProp, bcInsertObj, 
  bcRemoveObj, bcPush, bcPull, bcGetParent, bcGetChild, bcGetSibling, 
  bcGetProp, bcGetPropAddr, bcGetPropLen, bcGetNextProp, bcNop, bcSave, 
  bcRestore, bcRestart, bcPrint, bcPrintPaddr, bcPrintNum, bcPrintObj, 
  bcPrintAddr, bcPrintRet, bcPrintChar, bcNewLine, bcRandom, bcRead, bcReturn, 
  bcQuit, bcError 
    -- :: ZMachine -> Instruction -> IO ZMachine
) where 

import Data.Bits
import Data.Char
import Data.FiniteMap

import ZMachine.Core
import ZMachine.Random
import ZMachine.Stack
import ZMachine.Screen
import ZMachine.Text
import ZMachine.Lexical
import ZMachine.Object
import ZMachine.Arithmetics
import ZMachine.ZCode.Data

branch :: ZMachine -> Bool -> (Bool, Int) -> ZMachine

branch zMachine result (on, ofs) =
  if result == on then
    (if ofs == 0 || ofs == 1 then bcReturn zMachine ofs
     else zMachine{programCounter = programCounter zMachine + ofs - 2})
  else
    zMachine

bcNop zMachine instruction = return zMachine

bcError zMachine instruction = 
  let newZM = addZText zMachine $ "[Invalid opcode: " ++ (show instruction) ++ "]\n\n"
  in
    do doneScreen (zScreen newZM) 
       ioError (userError "quit")

f .:. g = g.f

convertToIO :: (ZMachine -> Instruction -> ZMachine) -> (ZMachine -> Instruction -> IO ZMachine)

convertToIO foo = \x y -> return (foo x y)

bcCall        = convertToIO bcFunCall 
bcRTrue       = convertToIO bcFunRTrue
bcRFalse      = convertToIO bcFunRFalse
bcRet         = convertToIO bcFunRet
bcRetPopped   = convertToIO bcFunRetPopped
bcAdd         = convertToIO bcFunAdd
bcSub         = convertToIO bcFunSub
bcMul         = convertToIO bcFunMul
bcDiv         = convertToIO bcFunDiv
bcMod         = convertToIO bcFunMod
bcAnd         = convertToIO bcFunAnd
bcOr          = convertToIO bcFunOr
bcJe          = convertToIO bcFunJe
bcJl          = convertToIO bcFunJl
bcJg          = convertToIO bcFunJg
bcJz          = convertToIO bcFunJz
bcJin         = convertToIO bcFunJin
bcTest        = convertToIO bcFunTest
bcInc         = convertToIO bcFunInc
bcDec         = convertToIO bcFunDec
bcDecChk      = convertToIO bcFunDecChk
bcIncChk      = convertToIO bcFunIncChk
bcTestAttr    = convertToIO bcFunTestAttr
bcSetAttr     = convertToIO bcFunSetAttr
bcClearAttr   = convertToIO bcFunClearAttr
bcLoadw       = convertToIO bcFunLoadw
bcLoadb       = convertToIO bcFunLoadb
bcJump        = convertToIO bcFunJump
bcLoad        = convertToIO bcFunLoad
bcStore       = convertToIO bcFunStore
bcStoreb      = convertToIO bcFunStoreb
bcStorew      = convertToIO bcFunStorew
bcPutProp     = convertToIO bcFunPutProp
bcInsertObj   = convertToIO bcFunInsertObj
bcRemoveObj   = convertToIO bcFunRemoveObj
bcPush        = convertToIO bcFunPush
bcPull        = convertToIO bcFunPull
bcGetParent   = convertToIO bcFunGetParent
bcGetChild    = convertToIO bcFunGetChild
bcGetSibling  = convertToIO bcFunGetSibling
bcGetProp     = convertToIO bcFunGetProp
bcGetPropAddr = convertToIO bcFunGetPropAddr
bcGetPropLen  = convertToIO bcFunGetPropLen
bcGetNextProp = convertToIO bcFunGetNextProp

bcUnimplementedBranch :: String -> ZMachine -> Instruction -> IO ZMachine

bcUnimplementedBranch text zMachine instruction = 
  let newZM = addZText zMachine (text ++ "\n")
      Just ofs = branchOfs instruction
  in 
    return (branch newZM False ofs)

bcSave = bcUnimplementedBranch "[HAZE does not provide `save'. Sorry!]"
bcRestore = bcUnimplementedBranch "[HAZE does not provide `restore'. Sorry!]"

bcRestart :: ZMachine -> Instruction -> IO ZMachine
bcRestart zMachine instruction = ioError (userError "restart")

bcPrint zMachine instruction = 
  case (text instruction) of
    Nothing -> return zMachine
    Just tx -> return $ addZText zMachine tx

bcPrintPaddr zMachine instruction = 
  let ([addr], newZM) = decodeOperands zMachine instruction
      str = fst $ getText newZM (unpackAddress newZM addr)
  in
    return $ addZText newZM str

bcPrintAddr zMachine instruction = 
  let ([addr], newZM) = decodeOperands zMachine instruction
      str = fst $ getText newZM addr
  in
    return $ addZText newZM str

bcPrintChar zMachine instruction =
  let ([ch], newZM) = decodeOperands zMachine instruction
      char = maybe [] (\x -> [x]) $ charZSCIItoASCII (toEnum ch) 
  in 
    return $ addZText newZM char

bcPrintObj zMachine instruction =
  let ([obj], newZM) = decodeOperands zMachine instruction
      str = objectName newZM obj
  in
    return $ addZText newZM str

bcPrintRet zMachine instruction =
  case (text instruction) of
    Nothing -> return zMachine
    Just tx -> let newZM = addZText zMachine (tx ++ "\n")
               in return $ bcReturn newZM 1 

bcPrintNum zMachine instruction =
  let ([num], newZM) = decodeOperands zMachine instruction
  in  
    return $ addZText newZM (show (toSigned num))

bcNewLine zMachine instruction = 
  return $ addZText zMachine "\n"
  
bcQuit zMachine instruction = do
  doneScreen (zScreen zMachine)
  ioError (userError "quit")

bcRandom zMachine instruction = do
  let ([num], newZM) = decodeOperands zMachine instruction
      Just var = storeVar instruction
  if num > 0 
    then do
      (res, newGen) <- zRandom (rndGen newZM) num
      return $ storeVariable var res (newZM{rndGen = newGen})
    else 
      return $ storeVariable var 0 (newZM{rndGen = zSeed (-num)})

storeText zMachine buf text = 
  let ver = getZMachineVersion zMachine
      store (zm, addr) val = (setByteValue zm addr val, addr + 1)
      startPoint = if ver < 5 then (zMachine, buf + 1) else
        store (zMachine, buf + 1) (length text)
      textStored = foldl store startPoint (map ord text)
  in
    fst (if ver < 5 then store textStored 0 else textStored)

zDrawStatusLine zMachine = do
  let (room, _) = getVariable zMachine 16
      (score, _) = getVariable zMachine 17
      (moves, _) = getVariable zMachine 18
      roomName = objectName zMachine room
      timed = isTimedGame zMachine
  drawStatusLine (zScreen zMachine) roomName score moves timed

bcRead zMachine instruction = do
  let (textBuf:parseBuf:_, newZM) = decodeOperands zMachine instruction
      ver = getZMachineVersion newZM
      maxLength = getByteValue newZM textBuf
      realLength = if ver < 5 then maxLength + 1 else maxLength
  if ver < 4 then zDrawStatusLine newZM else return ()
  (text, nzs) <- readLine (zScreen newZM) realLength
  let realText = map toLower text 
      afterStore = storeText newZM textBuf realText
      afterBuf = 
        tokenise afterStore (getDictionaryAddress afterStore) True realText parseBuf 
  return afterBuf{zScreen = nzs}

bcFunRTrue zMachine instruction = bcReturn zMachine 1
bcFunRFalse zMachine instruction = bcReturn zMachine 0

cover [] list = list
cover cs [] = []
cover (c:cs) (x:xs) = c:(cover cs xs)

bcFunRet zMachine instruction =
  let (ops, newZM) = decodeOperands zMachine instruction
  in bcReturn newZM (head ops)

bcFunAdd zMachine instruction =
  let ([a,b], newZM) = decodeOperands zMachine instruction
      Just var = storeVar instruction
  in storeVariable var (a `zPlus` b) newZM

bcFunSub zMachine instruction =
  let ([a,b], newZM) = decodeOperands zMachine instruction
      Just var = storeVar instruction
  in storeVariable var (a `zMinus` b) newZM

bcFunMul zMachine instruction =
  let ([a,b], newZM) = decodeOperands zMachine instruction
      Just var = storeVar instruction
  in storeVariable var (a `zTimes` b) newZM

bcFunDiv zMachine instruction =
  let ([a,b], newZM) = decodeOperands zMachine instruction
      Just var = storeVar instruction
  in if b == 0 then error "bcFunDiv" else storeVariable var (a `zDiv` b) newZM

bcFunMod zMachine instruction =
  let ([a,b], newZM) = decodeOperands zMachine instruction
      Just var = storeVar instruction
  in if b == 0 then error "bcFunMod" else storeVariable var (a `zMod` b) newZM

bcFunOr zMachine instruction = 
  let ([a,b], newZM) = decodeOperands zMachine instruction
      Just var = storeVar instruction
  in storeVariable var (a .|. b) newZM

bcFunAnd zMachine instruction = 
  let ([a,b], newZM) = decodeOperands zMachine instruction
      Just var = storeVar instruction
  in storeVariable var (a .&. b) newZM

bcFunRetPopped zMachine instruction = 
  let (val, newZM) = getVariable zMachine 0
  in bcReturn newZM val

bcFunJl zMachine instruction =
  let ([a,b], newZM) = decodeOperands zMachine instruction
      Just ofs = branchOfs instruction
  in
    branch newZM (toSigned a < toSigned b) ofs

bcFunJe zMachine instruction =
  let (x:xs, newZM) = decodeOperands zMachine instruction
      Just ofs = branchOfs instruction
  in
    branch newZM (x `elem` xs) ofs

bcFunJg zMachine instruction =
  let ([a,b], newZM) = decodeOperands zMachine instruction
      Just ofs = branchOfs instruction
  in
    branch newZM (toSigned a > toSigned b) ofs

bcFunJz zMachine instruction =
  let ([x], newZM) = decodeOperands zMachine instruction
      Just ofs = branchOfs instruction
  in
    branch newZM (x == 0) ofs

bcFunJin zMachine instruction = 
  let ([obj1, obj2], newZM) = decodeOperands zMachine instruction
      Just ofs = branchOfs instruction
      (parent1, _, _) = getFamily newZM obj1
  in
    branch newZM (parent1 == obj2) ofs

bcFunTest zMachine instruction =
  let ([bitmap, flags], newZM) = decodeOperands zMachine instruction
      Just ofs = branchOfs instruction
  in
    branch newZM (bitmap .&. flags == flags) ofs

bcFunDec zMachine instruction =
  let ([var], newZM) = decodeOperands zMachine instruction
      (oldval, newerZM) = getVariable newZM var
  in 
    storeVariable var (oldval `zMinus` 1) newerZM

bcFunInc zMachine instruction =
  let ([var], newZM) = decodeOperands zMachine instruction
      (oldval, newerZM) = getVariable newZM var
  in 
    storeVariable var (oldval `zPlus` 1) newerZM

bcFunDecChk zMachine instruction =
  let ([var, val], newZM) = decodeOperands zMachine instruction
      Just ofs = branchOfs instruction
      (oldval, newerZM) = getVariable newZM var
      newval = oldval `zMinus` 1
  in
    branch
      (storeVariable var newval newerZM)
      (toSigned newval < toSigned val)
      ofs

bcFunIncChk zMachine instruction =
  let ([var, val], newZM) = decodeOperands zMachine instruction
      Just ofs = branchOfs instruction
      (oldval, newerZM) = getVariable newZM var
      newval = oldval `zPlus` 1
  in
    branch
      (storeVariable var newval newerZM)
      (toSigned newval > toSigned val)
      ofs

bcFunTestAttr zMachine instruction =
  let ([obj, attr], newZM) = decodeOperands zMachine instruction
      Just ofs = branchOfs instruction
  in
    branch newZM (testAttribute zMachine obj attr) ofs

bcFunSetAttr zMachine instruction =
  let ([obj, attr], newZM) = decodeOperands zMachine instruction
  in
    setAttribute newZM obj attr

bcFunClearAttr zMachine instruction =
  let ([obj, attr], newZM) = decodeOperands zMachine instruction
  in
    clearAttribute newZM obj attr

bcFunLoadw zMachine instruction = 
  let ([arr, index], newZM) = decodeOperands zMachine instruction
      Just var = storeVar instruction
  in
    storeVariable var (getWordValue newZM (arr + 2 * index)) newZM

bcFunLoadb zMachine instruction =
  let ([arr, index], newZM) = decodeOperands zMachine instruction
      Just var = storeVar instruction
  in
    storeVariable var (getByteValue newZM (arr + index)) newZM

bcFunLoad zMachine instruction =
  let ([lvar], newZM) = decodeOperands zMachine instruction
      (val, newerZM) = getVariable newZM lvar
      Just var = storeVar instruction
  in
    storeVariable var val newerZM

bcFunStorew zMachine instruction =
  let ([arr, index, val], newZM) = decodeOperands zMachine instruction
  in 
    setWordValue newZM (arr + 2 * index) val

bcFunStoreb zMachine instruction =
  let ([arr, index, val], newZM) = decodeOperands zMachine instruction
  in
    setByteValue newZM (arr + index) val

bcFunStore zMachine instruction =
  let ([var, val], newZM) = decodeOperands zMachine instruction
  in
    storeVariable var val newZM

bcFunJump zMachine instruction =
  let ([ofs], newZM) = decodeOperands zMachine instruction
      pc = programCounter newZM
  in
    newZM{programCounter = pc + toSigned ofs - 2}

bcFunPutProp zMachine instruction =
  let ([obj, prop, value], newZM) = decodeOperands zMachine instruction
  in putProperty newZM obj prop value

bcFunGetProp zMachine instruction =
  let ([obj, prop], newZM) = decodeOperands zMachine instruction
      Just var = storeVar instruction
  in storeVariable var (getProperty newZM obj prop) newZM

bcFunGetPropAddr zMachine instruction =
  let ([obj, prop], newZM) = decodeOperands zMachine instruction
      Just var = storeVar instruction
  in storeVariable var (getPropertyAddress newZM obj prop) newZM

bcFunGetNextProp zMachine instruction =
  let ([obj, prop], newZM) = decodeOperands zMachine instruction
      Just var = storeVar instruction
  in storeVariable var (findNextProperty newZM obj prop) newZM

bcFunGetPropLen zMachine instruction =
  let ([addr], newZM) = decodeOperands zMachine instruction
      Just var = storeVar instruction
      ver = getZMachineVersion newZM
      pos = if ver < 4 then 
              addr - 1
            else
              let byte = getByteValue newZM (addr - 1) 
              in 
                if byte .&. 128 == 128 then addr - 2 else addr - 1
      (_, size, _) = decodeProperty newZM pos
  in storeVariable var size newZM

bcFunInsertObj zMachine instruction =
  let ([obj, dest], newZM) = decodeOperands zMachine instruction
  in
    insertObject newZM obj dest 

bcFunRemoveObj zMachine instruction =
  let ([obj], newZM) = decodeOperands zMachine instruction
  in removeObject newZM obj 

bcFunGetParent zMachine instruction =
  let ([obj], newZM) = decodeOperands zMachine instruction
      (parent, _, _) = getFamily newZM obj
      Just var = storeVar instruction 
  in
    storeVariable var parent newZM

bcFunGetChild zMachine instruction =
  let ([obj], newZM) = decodeOperands zMachine instruction
      (_, _, child) = getFamily newZM obj
      Just var = storeVar instruction
      Just ofs = branchOfs instruction
  in
    branch (storeVariable var child newZM) (child /= 0) ofs

bcFunGetSibling zMachine instruction =
  let ([obj], newZM) = decodeOperands zMachine instruction
      (_, sibling, _) = getFamily newZM obj
      Just var = storeVar instruction
      Just ofs = branchOfs instruction
  in
    branch (storeVariable var sibling newZM) (sibling /= 0) ofs

bcFunPush zMachine instruction =
  let ([value], newZM) = decodeOperands zMachine instruction
  in zPush newZM value

bcFunPull zMachine instruction =
  let ([var], newZM) = decodeOperands zMachine instruction
      (top, newerZM) = zPop newZM
  in storeVariable var top newerZM 
    
bcFunCall zMachine instruction =  
  let (addrs, newZM) = decodeOperands zMachine instruction 
      paddr:params = addrs
      addr = unpackAddress newZM paddr 
  in
    if paddr == 0 then -- calling 0 does nothing and returns false
      case storeVar instruction of
        Nothing -> newZM
        Just v -> storeVariable v 0 newZM
    else
      let 
        (preargs, argc, startAddr) = getRoutine newZM addr
	args = listToFM (zip [1..] (cover params preargs))
        whereToStoreResult = 
          case storeVar instruction of
            Nothing -> 256
            Just v -> v
        newStack = markFrame $                  -- the .:. notation makes it easier to read:
          ((push (programCounter zMachine)) .:. -- first push the PC... 
          (push whereToStoreResult) .:.         -- then the variable number where to store result...
          (foldl (\acc el -> (push (snd el)) .:. acc) id (fmToList (snd (currentRoutine zMachine)))) .:.
                                                -- then local variables of the current routine...
          (push (fst (currentRoutine zMachine)))) -- then the number thereof
          (zstack zMachine)                     -- onto this machine's stack
      in
        zMachine {
          programCounter = startAddr,
          zstack = newStack,
          currentRoutine = (argc, args)
        }

bcReturn zMachine val =
  let stack1 = releaseFrame (zstack zMachine)
      (numVars, stack2) = pop stack1
      (vars, stack3) = 
        foldl (\(acc, st) el -> let (x, nst) = pop st in (x:acc, nst)) ([], stack2) [1..numVars]
      (whereToStoreResult, stack4) = pop stack3
      (returnAddr, stack5) = pop stack4
  in
    storeVariable whereToStoreResult val (zMachine {
      programCounter = returnAddr, 
      zstack = stack5, 
      currentRoutine = (numVars, listToFM (zip [1..] (reverse vars)))
    })

