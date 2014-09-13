-- ZMachine/Text.hs :: Text handling
--------------------------------------------------------------------------
-- This file is part of HAZE, the Haskellish Abominable Z-machine Emulator
-- Copyright (C) 2004, Daniel Janus <nathell@zodiac.mimuw.edu.pl>

module ZMachine.Text (
  getText,                   -- :: ZMachine -> Int -> (String, Int)
  ver1ZCharTranslationTable, -- :: Array Int Char
  ver2ZCharTranslationTable, -- :: Array Int Char 
  zEncode,                   -- :: String -> Array Int Char -> Int -> Integer
  charZSCIItoASCII,          -- :: Char -> Char
  readTranslationTable       -- :: ZMachine -> Int -> Array Int Char
)

where

import Data.Array
import Data.Bits
import Data.Char
import Data.List

import ZMachine.Core

getText :: ZMachine -> Int -> (String, Int)
-- [getText zm addr] decodes a sequence of Z-characters stored at the
-- address [addr] and returns the decoded string along with the address
-- of the byte right after the end of the string.

getText zMachine addr = (decodeText zMachine zChars, nextAddr)
  where (zChars, nextAddr) = getZCharacters zMachine addr

getZCharacters :: ZMachine -> Int -> ([Int], Int)
-- Retrieves the sequence of numeric values of Z-characters at the given
-- address and return this sequence with the address of one byte past the end.

getZCharacters zMachine addr =
  let word = getWordValue zMachine addr
      triad = [(word .&. 31744) `shiftR` 10, 
               (word .&. 992) `shiftR` 5, 
               word .&. 31]
      theEnd = (word .&. 32768 == 32768)
  in
    if theEnd then 
      (triad, addr + 2)
    else 
      let (rest, nextAddr) = getZCharacters zMachine (addr + 2) in (triad ++ rest, nextAddr)

getAbbreviation :: ZMachine -> Int -> Int -> [Int]
-- Expands the abbreviation corresponding to the given Z-character pair.

getAbbreviation zMachine x y = 
  fst (getZCharacters zMachine (2 * getWordValue zMachine (offset + 2*(32*(x-1)+y))))
  where offset = getWordValue zMachine 24

decodeText :: ZMachine -> [Int] -> String
-- Transforms the sequence of Z-characters into ASCII string.

decodeText zMachine zChars =
  reverse (decode [] (0, Nothing) Nothing Nothing table zChars)
  where version = getZMachineVersion zMachine        
        table = zCharTable zMachine
        currentAlphabet (alphabet, Nothing) = alphabet
        currentAlphabet (alphabet, Just temp) = temp
        pickChar c alpha tbl = 
          case charZSCIItoASCII (tbl ! (26 * (currentAlphabet alpha) + c - 6)) of
            Nothing -> []
            Just ch -> [ch]
        shiftAlphabet = if version < 3 then complicatedShift else simpleShift
        simpleShift (alphabet, tempAlphabet) c = (alphabet, Just (c - 3))
        complicatedShift (alphabet, tempAlphabet) c =
          if lock then (newAlphabet, Nothing) else (alphabet, Just newAlphabet)
          where lock = c > 3
                newAlphabet = case (alphabet, if c > 3 then c - 2 else c) of
                                (0, 2) -> 1
                                (1, 2) -> 2
                                (2, 2) -> 0
                                (0, 3) -> 2 
                                (1, 3) -> 0
                                (2, 3) -> 1
        decode acc alphabet multiseq abbr tbl [] = acc
        decode acc alpha@(alphabet, tempAlphabet) multiseq abbr tbl (c:cs) =
          case multiseq of
            Just Nothing  -> 
              decode acc alpha (Just (Just c)) Nothing tbl cs
            Just (Just x) -> 
              decode ((toEnum (32 * x + c)):acc) (alphabet, Nothing)
                Nothing Nothing tbl cs
            Nothing       -> 
              case abbr of
                Just ab -> 
                  decode 
                    (decode acc (0, Nothing) Nothing Nothing tbl
                      (getAbbreviation zMachine ab c))
                    (alphabet, Nothing) Nothing Nothing tbl cs
                Nothing ->                  
                  if ((version == 1 || version == 2) && (c >= 2 && c <= 5)) ||
                      (version > 2 && (c == 4 || c == 5)) then 
                    decode acc (shiftAlphabet alpha c) Nothing Nothing tbl cs
                  else if c == 6 && currentAlphabet alpha == 2 then
                    decode acc alpha (Just Nothing) Nothing tbl cs
                  else if c == 7 && currentAlphabet alpha == 2 then
                    decode ('\n':acc) (alphabet, Nothing) Nothing Nothing tbl cs
                  else if c == 0 then
                    decode (' ':acc) (alphabet, Nothing) Nothing Nothing tbl cs
                  else if c == 1 && version == 1 then
                    decode ('\n':acc) (alphabet, Nothing) Nothing Nothing tbl cs
                  else if (version == 2 && c == 1) || (version == 3 &&
                    (c >= 1 && c <= 3)) then
                    decode acc alpha Nothing (Just c) tbl cs
                  else
                    decode ((pickChar c alpha tbl)++acc) (alphabet, Nothing) 
                      Nothing Nothing tbl cs

ver1ZCharTranslationTable :: Array Int Char
ver2ZCharTranslationTable :: Array Int Char
-- The standard translation tables.

ver1ZCharTranslationTable = listArray (0,77) $
  "abcdefghijklmnopqrstuvwxyz" ++ 
  "ABCDEFGHIJKLMNOPQRSTUVWXYZ" ++
  " 0123456789.,!?_#'\"/\\<-:()"

ver2ZCharTranslationTable = listArray (0,77) $
  "abcdefghijklmnopqrstuvwxyz" ++ 
  "ABCDEFGHIJKLMNOPQRSTUVWXYZ" ++
  " \r0123456789.,!?_#'\"/\\-:()"

charZSCIItoASCII :: Char -> Maybe Char
-- Converts a ZSCII character to its ASCII equivalent, if there is one.

charZSCIItoASCII c =
  if c == '\000' then Nothing else
  if c == '\013' then Just '\n' else
  if c >= '\032' && c <= '\126' then Just c else
  if c >= '\155' && c <= '\251' then Just '?' else -- !!! incompatibility with standard
  error $ "charZSCIItoASCII " ++ (show (fromEnum c))

zEncode :: String -> Array Int Char -> Int -> Integer
-- [zEncode str table maxLength] encodes the string [str] to a Z-character
-- compressed form, padding it to length [maxLength] and returns it as 
-- an Integer suitable for searching in a dictionary.

zEncode str table maxLength = 
  let encode str len =
	    if len == 0 then []
	    else case str of
	      []   -> take len (repeat 5)
	      c:cs -> if c >= 'a' && c <= 'z' then 
			(ord c - ord 'a' + 6):(encode cs (len - 1))
		      else if len == 1 then [5]
		      else 
			let ltbl = drop 52 (elems table)
			in case elemIndex c ltbl of
			  Nothing -> encode cs len
			  Just n -> 5:(n+6):(encode cs (len - 2)) 
      packTriads [] = []
      packTriads [a,b,c] = [32768 + a `shiftL` 10 + b `shiftL` 5 + c]
      packTriads (a:b:c:l) = (a `shiftL` 10 + b `shiftL` 5 + c):(packTriads l)
  in 
    foldl (\x y -> 65536 * x + y) 0 $
      map fromIntegral $ packTriads (encode (map toLower str) maxLength)

readTranslationTable :: ZMachine -> Int -> Array Int Char
-- Reads a translation table stored at the given address in this ZMachine's
-- memory.

readTranslationTable m offset =
  listArray (0,77) (map (toEnum.(getByteValue m)) [offset..(offset + 77)])

