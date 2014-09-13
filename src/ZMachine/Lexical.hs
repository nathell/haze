-- ZMachine/Lexical.hs :: Lexical analysis
--------------------------------------------------------------------------
-- This file is part of HAZE, the Haskellish Abominable Z-machine Emulator
-- Copyright (C) 2004, Daniel Janus <nathell@zodiac.mimuw.edu.pl>

module ZMachine.Lexical (
  getDictionaryAddress, -- :: ZMachine -> Int
  tokenise              -- :: ZMachine -> Int -> Bool -> String -> Int -> 
                        --    ZMachine
)

where

import Data.Char

import ZMachine.Core
import ZMachine.Text

getDictionaryWord :: ZMachine -> Int -> Integer
-- [getDictionaryWord zm n] returns the n-th word in this ZMachine's
-- dictionary, encoded as a 32- or 48-bit integer.

getDictionaryWord zMachine addr = 
  if getZMachineVersion zMachine < 4 then
    65536 * (gwv addr) + gwv (addr + 2)
  else
    (65536^2 * gwv addr) + (65536 * gwv (addr + 2)) + gwv (addr + 4)
  where gwv x = fromIntegral (getWordValue zMachine x)

myWords :: String -> String -> [(Int, String)]
-- [myWords str seps] divides the string [str] into words separated by
-- spaces and the separators specified in the list [seps]; spaces 
-- don't count as words but each separator is considered a separate word.
-- Returns the list of pairs (word, offset of the first letter of this word
-- in [str]). Cf. [Data.List.words].

myWords str seps = snd (search 0 str seps) where
  search _ [] _        = (False, [])
  search n (c:cs) seps = if c == ' ' then
                           (False, snd (search (n + 1) cs seps))
                         else if c `elem` seps then
                           (False, (n, [c]):(snd (search (n + 1) cs seps)))
                         else 
                           case search (n + 1) cs seps of
                             (_, []) -> (True, [(n, [c])])
                             (True, (n', c':cs'):t) ->
                               (True, (n, c:c':cs'):t)
                             (False, l) -> (True, (n, [c]):l)

getDictionaryAddress :: ZMachine -> Int 
-- Returns the address of this ZMachine's main dictionary.

getDictionaryAddress zMachine = getWordValue zMachine 0x08

tokenise :: ZMachine -> Int -> Bool -> String -> Int -> ZMachine
-- [tokenise zm dictionary sorted text buffer] looks up each word of [text]
-- in the specified dictionary and stores the result in a buffer
-- located at the address [buffer], as specified in the Z-Machine
-- Standard. The [sorted] parameter specifies whether the dictionary
-- is sorted (and thus whether binary search can be used).

tokenise zMachine dictionary sorted text parseBuffer =
  let ver = getZMachineVersion zMachine
      numInputCodes = getByteValue zMachine dictionary 
      inputBytes = fst $ readBytes zMachine (dictionary + 1) numInputCodes
      inputCodes = map chr inputBytes
      splitText = myWords text inputCodes
      textOfs = if ver < 5 then 1 else 2
      maxWords = getByteValue zMachine parseBuffer
      parse [] zMachine bufOfs n = zMachine
      parse _ zMachine _ 0 = zMachine
      parse ((ofs, word):t) zMachine bufOfs n = parse t
        (setWordValue 
           (setByteValue 
             (setByteValue zMachine (bufOfs + 3) (ofs + textOfs))
             (bufOfs + 2) (length word))
           bufOfs 
           (lookupDictionary zMachine dictionary sorted word))
        (bufOfs + 4)
        (n - 1)
  in
    parse splitText 
      (setByteValue zMachine (parseBuffer + 1) (length splitText))
      (parseBuffer + 2) maxWords

lookupDictionary :: ZMachine -> Int -> Bool -> String -> Int
-- [lookupDictionary zm dictionary sorted word] returns the address of
-- [word] in the given [dictionary], or else 0 if there is no such word
-- in the dictionary. For meaning of [sorted], see above.

lookupDictionary zMachine dictionary sorted word =
  let ver = getZMachineVersion zMachine
      numInputCodes = getByteValue zMachine dictionary 
      nextAddr = dictionary + numInputCodes + 1
      entryLength = getByteValue zMachine nextAddr
      entryCount = getWordValue zMachine (nextAddr + 1)   
      getEntryAddress n = nextAddr + 3 + n * entryLength
      getEntry n = getDictionaryWord zMachine (getEntryAddress n)
      encodedWord = zEncode word (zCharTable zMachine) (if ver < 4 then 6 else 9)
      binarySearch start end = 
        if start > end then 0 else
          let mid = (start + end) `div` 2 
              cand = getEntry mid
          in  if cand == encodedWord then getEntryAddress mid else
              if cand < encodedWord then binarySearch (mid + 1) end else
              binarySearch start (mid - 1)
      linearSearch start end =
        if start > end then 0 else
          let cand = getEntry start 
          in  if cand == encodedWord then getEntryAddress start else 
              linearSearch (start + 1) end
      search = if sorted then binarySearch else linearSearch
  in
    search 0 (entryCount - 1)
