-- ZMachine/Screen.hs :: The Z-machine's screen model
--------------------------------------------------------------------------
-- This file is part of HAZE, the Haskellish Abominable Z-machine Emulator
-- Copyright (C) 2004, Daniel Janus <nathell@zodiac.mimuw.edu.pl>

module ZMachine.Screen (
  ZScreen,            -- data ZScreen 
  dummyZScreen,       -- :: ZScreen
  initZScreen,        -- :: Int -> IO ZScreen
  drawScreen,         -- :: ZScreen -> IO ()
  flushScreen,        -- :: ZScreen -> ZScreen
  addText,            -- :: ZScreen -> String -> ZScreen
  doneScreen,         -- :: ZScreen -> IO ()
  abnormalDoneScreen, -- :: IO ()
  readLine,           -- :: ZScreen -> Int -> IO (String, ZScreen)
  drawStatusLine      -- :: ZScreen -> String -> Int -> Int -> Bool -> IO ()
)

where

import Curses
import Control.Monad
import Data.Bits
import Data.FiniteMap
import Data.List
 
data ZScreen = ZScreen { 
  screenWidth :: Int,
  screenHeight :: Int,
  upperWindow :: ZWindow, 
  lowerWindow :: ZWindow,
  currentWindow :: Int, -- 0 is upper, 1 lower
  currentAttr :: Char,
  defaultAttr :: Char
}
-- This is the screen of a Z-machine. This implementation is capable
-- of modelling screen of all Z-machine versions except 6, even though
-- officially HAZE supports version up to and including 3 only.
-- The screen consists of two windows, upper and lower: the upper one
-- overlaps the lower, and both have width equal to the width of the screen.
-- One of these windows is the currently selected one, and a current
-- and default set of attributes (color pair and text style) is 
-- provided.

data ZWindow = ZWindow {
  windowWidth :: Int,
  windowHeight :: Int,
  windowOffset :: Int,
  xCursor :: Int,
  yCursor :: Int, 
  buffering :: Bool, -- text buffering active?
  buffer :: String,  -- current buffer
  winData :: [String]
}
-- Most of these attributes are self-explanatory, but the `winData' needs
-- description. It is the list of the lines making up the window, given
-- in bottom-up order (that is, the bottom line is the head of the list 
-- and the length is always equal to the height of the window).
-- The lines are plain text, except that if a character \001 appears
-- anywhere then the next character denotes a set of attributes
-- (a color pair in bits 5-0, bold flag in bit 6) to mean `from now on
-- to the end of line or the next attribute mark, this attribute
-- is the current one.' Each line has length equal to the width of the
-- window, padded at the end with spaces if necessary. The screen-handing
-- routines below follow these guidelines.

dummyZScreen :: ZScreen
-- Return a dummy value of type ZScreen. Needed in ZMachine initialization.

dummyZScreen = 
  ZScreen 0 0 win win 0 ' ' ' '
  where win = ZWindow 0 0 0 0 0 True [] []

initZScreen :: Int -> IO ZScreen
-- Takes an integer (supposed to be a version number of a Z-machine)
-- and creates a ZScreen modelling the screen of that Z-machine.

initZScreen ver = do
  initCurses
  startColor
  foldM 
    (\x (n,(b,f)) -> (initPair (Pair n) f b) >> return x)
    () 
    (zip [1..63] (drop 1
      (let l = [black,red,green,yellow,blue,magenta,cyan,white] 
       in [(b, f) | b <- l, f <- l])))
  (sh, sw) <- scrSize
  let ofs = if ver < 4 then 1 else 0
      upperWindow = createWindow 0 sw ofs 0 0 False
      lowerWindow = createWindow (sh - 1) sw ofs 0 (sh - 2) True
  return $ ZScreen sw sh upperWindow lowerWindow 1 '\39' '\39'

getCurrentWindow :: ZScreen -> ZWindow
-- Returns the currently selected window.

getCurrentWindow screen =
  (if currentWindow screen == 1 then lowerWindow else upperWindow) screen

blankLine :: Int -> String
-- Returns a string of that many spaces.

blankLine width = take width (repeat ' ')

createWindow :: Int -> Int -> Int -> Int -> Int -> Bool -> ZWindow
-- Creates a window with the given attributes.

createWindow height width offset initX initY buf =
  let bl = blankLine width
      blankLines = bl:blankLines
      lines = take height blankLines
  in  ZWindow width height offset initX initY buf "" lines

drawScreen :: ZScreen -> IO ()
-- Physically draws the given ZScreen.

drawScreen scr = do
  drawWindow (lowerWindow scr) (defaultAttr scr)
  drawWindow (upperWindow scr) (defaultAttr scr)
  let cw = getCurrentWindow scr
  move (yCursor cw + windowOffset cw) (xCursor cw)
  refresh

flushScreen :: ZScreen -> ZScreen
-- Flushes the buffer of the lower window of this ZScreen.

flushScreen scr = 
  scr{lowerWindow = flushBuffer curr def win}
  where curr = currentAttr scr
        def = defaultAttr scr
        win = lowerWindow scr

drawWindow :: ZWindow -> Char -> IO Int
-- [drawWindow win def] draws the window [win] with the default text attribute
-- [def].

drawWindow win def = 
  let height = windowHeight win
      offset = windowOffset win
  in
    foldM 
      (\x y -> drawLine def x y >> return (x - 1)) 
      (offset + height - 1) 
      (winData win)

drawLine :: Char -> Int -> String -> IO ()
-- [drawLine def row str] draws the line [str] in the [row] using default 
-- text attribute [def].

drawLine def row str = do
  attrSet attr0 (Pair (fromEnum def))
  move row 0
  doAddLine [] str
  where doAddLine acc [] = addStr (reverse acc)
        doAddLine acc [c] = 
          if c == '\001' then addStr (reverse acc) else addStr (reverse (c:acc))
        doAddLine acc (c1:c2:x) =
          if c1 == '\001' then do
            addStr (reverse acc) 
            let c = fromEnum c2
            attrSet (setBold attr0 (c.&.64 == 64)) (Pair (fromEnum c2.&.63))
            doAddLine [] x
          else doAddLine (c1:acc) (c2:x)

addText :: ZScreen -> String -> ZScreen
-- Writes a text to the given ZScreen.

addText scr str =
  if currentWindow scr == 0 then
    scr{upperWindow = addWText (upperWindow scr) curr def str}
  else
    scr{lowerWindow = addWText (lowerWindow scr) curr def str}
  where
    curr = currentAttr scr
    def = defaultAttr scr

addWText :: ZWindow -> Char -> Char -> String -> ZWindow
-- Writes a text to the given ZWindow, using supplied current and
-- default attributes.

addWText win curr def str =
  let lines = groupBy (\x y -> x /= '\n' && y /= '\n') str
  in 
    foldl (addLine curr def) win lines

addLine :: Char -> Char -> ZWindow -> String -> ZWindow
-- Same as [addText], except it adds a single line of text.

addLine curr def win line = 
  if line == "\n" then 
    newLine (flushBuffer curr def win) 
  else 
    realAddLine curr def win line

newLine :: ZWindow -> ZWindow
-- Insert a carriage return to the given ZWindow (possibly scrolling up
-- its content).

newLine win =
  let x = 0
      y = yCursor win + 1
      ysz = windowHeight win
  in 
    if y < ysz then 
      win{xCursor = x, yCursor = y} 
    else 
      (scrollUp win){xCursor = x, yCursor = y - 1}

scrollUp :: ZWindow -> ZWindow
-- Shifts the lines of the given ZWindow upwards making room for a new one,
-- discards the topmost line, adds a new blank line and moves the cursor
-- to the beginning of it.

scrollUp win = 
  win{winData = (blankLine (windowWidth win)):(init oldData)}
  where oldData = winData win

realAddLine :: Char -> Char -> ZWindow -> String -> ZWindow
-- Same as [addLine], except that it adds a line which isn't a newline.

realAddLine curr def win txt =
  if buffering win then
    addBufferedLine curr def win txt
  else
    addUnbufferedLine curr def win txt

addBufferedLine :: Char -> Char -> ZWindow -> String -> ZWindow
-- Add a single line of text (not containing a newline) in a buffered
-- mode.

addBufferedLine curr def win txt =
  let words = groupBy 
                (\x y -> (x == ' ' && y == ' ') || (x /= ' ' && y /= ' '))
                txt
      isWord w = (head w /= ' ')
  in
    foldl 
      (\w s -> (if isWord s then addBuffer else addBSpaces curr def) w s)
      win words

addBuffer :: ZWindow -> String -> ZWindow
-- Appends the given text to the end of the ZWindow's buffer.

addBuffer win str = win{buffer = oldBuffer ++ str}
  where oldBuffer = buffer win

addBSpaces :: Char -> Char -> ZWindow -> String -> ZWindow
-- Writes the contents of a buffer followed by a given string
-- of spaces to the given window.

addBSpaces curr def win str =
  addSpaces curr def
    (flushBuffer curr def win)
    str

flushBuffer :: Char -> Char -> ZWindow -> ZWindow
-- Writes the contents of a window's buffer to that window. 

flushBuffer curr def win = 
  let buf = buffer win
  in
    (addUnbufferedLine curr def
      ((if xCursor win + length buf > windowWidth win then newLine else id) win)
      buf) {buffer = []}

addSpaces :: Char -> Char -> ZWindow -> String -> ZWindow
-- Writes a string of spaces to the given window.

addSpaces curr def win spaces =
  if xCursor win + length spaces > windowWidth win then 
    newLine win
  else 
    addUnbufferedLine curr def win spaces

addUnbufferedLine :: Char -> Char -> ZWindow -> String -> ZWindow
-- Add a single line of text (not containing a newline) in an unbuffered
-- mode.

addUnbufferedLine curr def win [] = win
addUnbufferedLine curr def win txt = 
  let (now, rest) = splitAt (windowWidth win - xCursor win) txt
      afterNow = overwriteText curr def win now
      next = if xCursor afterNow == windowWidth afterNow && rest /= [] then 
        newLine afterNow else afterNow
  in addUnbufferedLine curr def next rest

overwriteText :: Char -> Char -> ZWindow -> String -> ZWindow
-- Overwrites the part of current line at the cursor with a given text
-- and advances the cursor.

overwriteText curr def win txt =
  let lineNum = windowHeight win - 1 - yCursor win
      txtLen = zLength txt
      (beforeLine, line:afterLine) = splitAt lineNum (winData win) 
      (start, rest) = zSplit (xCursor win) line
      (middle, end) = zSplit txtLen rest 
      realTxt = if curr == def then txt else ['\1',curr] ++ txt ++ ['\1',def]
      newLine = start ++ realTxt ++ end
  in
    win {
      winData = beforeLine ++ [newLine] ++ afterLine,
      xCursor = xCursor win + txtLen
    }

zSplit :: Int -> String -> (String, String)
-- Same as [Data.List.split], but considering the change-attribute sequence
-- as having zero length.

zSplit 0 l = ([], l)
zSplit _ [] = ([], [])
zSplit n ([x]) = ([x], [])
zSplit n (h:h1:t) = 
  if h == '\001' then
    let (x, y) = zSplit n t
    in (h:h1:x, y)
  else
    let (x, y) = zSplit (n - 1) (h1:t)
    in (h:x, y)

zLength :: String -> Int
-- Same as [length], but considering the change-attribute sequence
-- as having zero length.

zLength l = calcZLength 0 l
  where calcZLength acc [] = acc
        calcZLength acc "\001" = acc
        calcZLength acc ('\001':_:l) = calcZLength acc l
        calcZLength acc (h:t) = calcZLength (acc + 1) t

doneScreen :: ZScreen -> IO ()
-- Performs clean-up operations on the ZScreen.

doneScreen scr = do
  drawScreen (flushScreen (addText scr "[Press any key to exit.]"))
  getCh
  endWin

abnormalDoneScreen :: IO ()
-- As above, except that it doesn't display the goodbye text nor wait 
-- for keypress.

abnormalDoneScreen = endWin

changeAttr :: ZScreen -> Int -> ZScreen
-- Changes the screen's current attribute.

changeAttr scr attr = scr{currentAttr = toEnum attr}

readLine :: ZScreen -> Int -> IO (String, ZScreen)
-- [readLine scr maxLength] reads a line of text (no more than [maxLength]
-- characters) from the keyboard and returns it along with the modified
-- ZScreen.

readLine scr maxLength = do
  let newScr = flushScreen scr
  drawScreen newScr
  let cw = getCurrentWindow newScr
      x = xCursor cw
      y = yCursor cw + windowOffset cw
  str <- doReadLine (min maxLength (windowWidth cw - x - 1)) (x, y) 0 "" 0
  move y x
  return (str, addText newScr (str ++ "\n"))
  
doReadLine :: Int -> (Int, Int) -> Int -> String -> Int -> IO String
-- This does the dirty work for readLine.

doReadLine maxLength (xs, ys) ofs str len = do
  move ys xs
  addStr str
  addStr (if len < maxLength then " " else "")
  move ys (xs + ofs)
  refresh
  c <- getCh
  case c of
    KeyChar '\n' -> return str
    KeyChar '\r' -> return str
    KeyEnter -> return str
    KeyLeft -> do let nofs = if ofs > 0 then ofs - 1 else ofs
                  doReadLine maxLength (xs, ys) nofs str len
    KeyRight -> do let nofs = if ofs < len then ofs + 1 else ofs
                   doReadLine maxLength (xs, ys) nofs str len
    KeyChar '\8' -> if ofs == 0 then
                      doReadLine maxLength (xs, ys) ofs str len
                    else
                      doReadLine maxLength (xs, ys) (ofs - 1) (removeAt str (ofs - 1)) (len - 1)
    KeyBackspace -> if ofs == 0 then
                      doReadLine maxLength (xs, ys) ofs str len
                    else
                      doReadLine maxLength (xs, ys) (ofs - 1) (removeAt str (ofs - 1)) (len - 1)
    KeyDC -> if ofs == len then
               doReadLine maxLength (xs, ys) ofs str len
             else
               doReadLine maxLength (xs, ys) ofs (removeAt str ofs) (len - 1)
    KeyChar x | x >= '\32' && x <= '\126' -> 
                 if len == maxLength then 
                   doReadLine maxLength (xs, ys) ofs str len
                 else
                   doReadLine maxLength (xs, ys) (ofs + 1) (insertAt str ofs x) (len + 1)
    _ -> doReadLine maxLength (xs, ys) ofs str len

insertAt :: [a] -> Int -> a -> [a]
-- [insertAt l ofs x] inserts element [x] before [ofs]'th element
-- onto the list [l].

insertAt l ofs x =
  let (start, end) = splitAt ofs l in start ++ [x] ++ end

removeAt :: [a] -> Int -> [a]
-- [removeAt l ofs] returns [l] without [ofs]'th element.

removeAt l ofs =
  let (start, end) = splitAt ofs l in start ++ (drop 1 end)

drawStatusLine :: ZScreen -> String -> Int -> Int -> Bool -> IO ()
-- Draws a status line above a given ZScreen.

drawStatusLine scr location score moves timed =
  let w = screenWidth scr
      sm = if timed then
             "  Time: " ++ (show score) ++ ":" ++ (show moves) ++ " "
           else 
             "  Score: " ++ (show score) ++ "  Moves: " ++ (show moves) ++ " "
      len = 1 + length sm
      maxLocLen = w - len
      locLen = length location
      loc = 
        if locLen > maxLocLen then 
          take (maxLocLen - 3) location ++ "..."
        else
          location ++ take (maxLocLen - locLen) (repeat ' ')
      statusLine = " " ++ loc ++ sm
  in 
    do move 0 0
       attrSet (setReverse attr0 True) (Pair 0)
       addStr statusLine
