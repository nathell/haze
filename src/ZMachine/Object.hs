-- ZMachine/Object.hs :: Object handling
--------------------------------------------------------------------------
-- This file is part of HAZE, the Haskellish Abominable Z-machine Emulator
-- Copyright (C) 2004, Daniel Janus <nathell@zodiac.mimuw.edu.pl>

module ZMachine.Object (
  putProperty,        -- :: ZMachine -> Int -> Int -> Int -> ZMachine
  getProperty,        -- :: ZMachine -> Int -> Int -> Int
  getPropertyAddress, -- :: ZMachine -> Int -> Int -> Int
  findNextProperty,   -- :: ZMachine -> Int -> Int -> Int
  testAttribute,      -- :: ZMachine -> Int -> Int -> Bool
  setAttribute,       -- :: ZMachine -> Int -> Int -> ZMachine
  clearAttribute,     -- :: ZMachine -> Int -> Int -> ZMachine
  decodeProperty,     -- :: ZMachine -> Int -> (Int, Int, Int)
  getFamily,          -- :: ZMachine -> Int -> (Int, Int, Int)
  insertObject,       -- :: ZMachine -> Int -> Int -> ZMachine
  removeObject,       -- :: ZMachine -> Int -> ZMachine
  objectName          -- :: ZMachine -> Int -> String
) 

where

import Data.Bits

import ZMachine.Core
import ZMachine.Text

objectAddress :: ZMachine -> Int -> Int
-- Return the address of the given object in this ZMachine's object table.

objectAddress zMachine obj =
  let ver = getZMachineVersion zMachine
      (treeOffset, entrySize) = if ver < 4 then (62, 9) else (126, 14)
  in
    (objectTableOffset zMachine + treeOffset + (obj - 1) * entrySize)

objectPropertyAddress :: ZMachine -> Int -> Int
-- Return the address of the given object's property table.

objectPropertyAddress zMachine obj =
  let ver = getZMachineVersion zMachine
      propOffset = if ver < 4 then 7 else 12
  in getWordValue zMachine (objectAddress zMachine obj + propOffset)

putProperty :: ZMachine -> Int -> Int -> Int -> ZMachine
-- [putProperty zm obj prop value] sets the value of the object [obj]'s 
-- property [prop] to [value]. The property must be 1 or 2 bytes wide;
-- if it is larger, the result is as if it were 2 bytes wide. If the
-- object has no such property, throws an error.
-- Returns the modified ZMachine.
      
putProperty zMachine obj prop value =
  let propTableOfs = objectPropertyAddress zMachine obj
      nameLength = getByteValue zMachine propTableOfs
      propStart = propTableOfs + 2 * nameLength + 1
      property = lookupProperty zMachine prop propStart
  in
    case property of
      Nothing -> error "putProperty"
      Just (size, addr) ->
        if size == 1 then
          setByteValue zMachine addr (value .&. 255)
        else 
          setWordValue zMachine addr value

getProperty :: ZMachine -> Int -> Int -> Int
-- [getProperty zm obj prop] returns the 1- or 2-byte value of the object
-- [obj]'s property [prop], or else the default value if the object has
-- no such property.

getProperty zMachine obj prop =
  let propTableOfs = objectPropertyAddress zMachine obj
      nameLength = getByteValue zMachine propTableOfs
      propStart = propTableOfs + 2 * nameLength + 1
      property = lookupProperty zMachine prop propStart
  in
    case property of
      Nothing -> 
        getWordValue zMachine (objectTableOffset zMachine + 2 * (prop - 1))
      Just (size, addr) -> 
        (if size == 1 then getByteValue else getWordValue) zMachine addr

getPropertyAddress :: ZMachine -> Int -> Int -> Int
-- [getPropertyAddress zm obj prop] returns the byte address of the object
-- [obj]'s property [prop] data, or else 0 if the object has no such
-- property.

getPropertyAddress zMachine obj prop = 
  let propTableOfs = objectPropertyAddress zMachine obj
      nameLength = getByteValue zMachine propTableOfs
      propStart = propTableOfs + 2 * nameLength + 1
      property = lookupProperty zMachine prop propStart
  in
    case property of
      Nothing -> 0
      Just (size, addr) -> addr 

findNextProperty :: ZMachine -> Int -> Int -> Int
-- [findNextProperty zm obj prop] returns the next property after [prop]
-- which the object [obj] possesses. If [prop] is zero, returns the first
-- property; if there are no more properties, returns zero.

findNextProperty zMachine obj prop =
  let propTableOfs = objectPropertyAddress zMachine obj
      nameLength = getByteValue zMachine propTableOfs
      propStart = propTableOfs + 2 * nameLength + 1
      realProp = if prop == 0 then 256 else prop
      search addr =
        let (num, size, nextAddr) = decodeProperty zMachine addr
        in if num < realProp then num else search nextAddr
  in 
    search propStart

testAttribute :: ZMachine -> Int -> Int -> Bool
-- [testAttribute zm obj attr] tests whether the object [obj] has attribute
-- [attr].

testAttribute zMachine obj attr =
  let ver = getZMachineVersion zMachine
      objAddr = objectAddress zMachine obj
      attrCount = if ver < 4 then 32 else 48
      byteNum = attr `shiftR` 3
      bit = 7 - attr .&. 7
  in
    getByteValue zMachine (objAddr + byteNum) `testBit` bit

setAttribute, clearAttribute :: ZMachine -> Int -> Int -> ZMachine
-- When called with arguments [obj attr], these functions set or clear
-- the attribute [attr] of the object [obj].

setAttribute = tweakAttribute setBit
clearAttribute = tweakAttribute clearBit

tweakAttribute :: (Int -> Int -> Int) -> ZMachine -> Int -> Int -> ZMachine
-- This function does the dirty work for the two above: it additionally
-- takes the bit operation to perform and does it on the proper byte.

tweakAttribute op zMachine obj attr =
  let ver = getZMachineVersion zMachine
      objAddr = objectAddress zMachine obj
      attrCount = if ver < 4 then 32 else 48
      byteNum = attr `shiftR` 3
      bit = 7 - attr .&. 7
      oldByte = getByteValue zMachine (objAddr + byteNum)
  in
    setByteValue zMachine (objAddr + byteNum) (oldByte `op` bit)

decodeProperty :: ZMachine -> Int -> (Int, Int, Int)
-- Takes a byte address of an object property and returns a triple
-- (number of property, size of property in bytes, address of
-- the first byte of property data).

decodeProperty zMachine addr =
  let ver = getZMachineVersion zMachine
      sizeByte = getByteValue zMachine addr
  in 
    if ver < 4 then
      (sizeByte .&. 31, 1 + sizeByte `shiftR` 5, addr + 1)
    else
      let num = sizeByte .&. 63
          (size, nextAddr) =
            if sizeByte .&. 128 == 128 then
              (getByteValue zMachine (addr + 1) .&. 63, addr + 2)
            else
              (if sizeByte .&. 64 == 64 then 2 else 1, addr + 1)
      in (num, if size == 0 then 64 else size, nextAddr)         

lookupProperty :: ZMachine -> Int -> Int -> Maybe (Int, Int)
-- [lookupProperty zm number startAddr] searches the property table
-- beginning at the address [startAddr] for the property with the given
-- number and if found, returns its size in bytes along with the address
-- of the first byte of the property data.

lookupProperty zMachine number startAddr =
  let ver = getZMachineVersion zMachine 
      doLookup addr = 
        let (thisNum, thisSize, nextAddr) = decodeProperty zMachine addr
        in
          (if thisNum == number then Just (thisSize, addr + 1) else
           if thisNum < number then Nothing else
           doLookup (nextAddr + thisSize))
  in
    doLookup startAddr

objectTableOffset :: ZMachine -> Int 
-- Return the address of this ZMachine's object table.

objectTableOffset zMachine = getWordValue zMachine 0x0a

getFamily :: ZMachine -> Int -> (Int, Int, Int)
-- Return the triple (parent, left child, right sibling) for the
-- given object.

getFamily zMachine obj =
  let ver = getZMachineVersion zMachine
      objAddr = objectAddress zMachine obj
  in 
    if ver < 4 then
      (getByteValue zMachine (objAddr + 4), 
       getByteValue zMachine (objAddr + 5),
       getByteValue zMachine (objAddr + 6))
    else
      (getWordValue zMachine (objAddr + 6),
       getWordValue zMachine (objAddr + 8),
       getWordValue zMachine (objAddr + 10))
             
setFamily :: ZMachine -> Int -> (Int, Int, Int) -> ZMachine
-- Set the family of the given object and return the modified ZMachine.

setFamily zMachine obj (parent, sibling, child) =
  let ver = getZMachineVersion zMachine
      objAddr = objectAddress zMachine obj
      set = if ver < 4 then setByteValue else setWordValue
      (dparent, dsibling, dchild) = if ver < 4 then (4,5,6) else (6,8,10)
  in
    set 
      (set 
        (set 
          zMachine 
          (objAddr + dparent) parent) 
        (objAddr + dsibling) sibling)
      (objAddr + dchild) child

detach :: ZMachine -> Int -> ZMachine
-- Make the parent of a given object no more have it among its children.

detach zMachine obj =
  let (parent, sibling, child) = getFamily zMachine obj
      searchSiblings cand = 
        let (oparent, osibling, ochild) = getFamily zMachine cand
        in 
          if osibling == 0 then zMachine else
          if osibling == obj then setFamily zMachine cand (oparent, sibling, ochild) else
          searchSiblings osibling
  in
    if parent == 0 then zMachine else
      let (grandpa, psibling, pchild) = getFamily zMachine parent
      in
        if pchild == obj then 
          setFamily zMachine parent (grandpa, psibling, sibling)
        else
          searchSiblings pchild

insertObject :: ZMachine -> Int -> Int -> ZMachine
-- [insertObject zm obj1 obj2] moves the object [obj1] to become the new
-- left child of [obj2]. 

insertObject zMachine obj1 obj2 =
  let newZM = detach zMachine obj1
      (parent1, sibling1, child1) = getFamily newZM obj1
      (parent2, sibling2, child2) = getFamily newZM obj2
  in
    setFamily 
      (setFamily newZM obj2 (parent2, sibling2, obj1))
      obj1 (obj2, child2, child1)

removeObject :: ZMachine -> Int -> ZMachine
-- Detach the given object from its parent.

removeObject zMachine obj =
  let newZM = detach zMachine obj
      (parent, sibling, child) = getFamily newZM obj
  in
    setFamily newZM obj (0, sibling, child)

objectName :: ZMachine -> Int -> String
-- Return the short name of the given object.

objectName zMachine obj =
  fst $ getText zMachine (objectPropertyAddress zMachine obj + 1)
