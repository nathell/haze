-- Taken from the QForeign project
-- Updated to work with GHC 6.2.1 and pdcurses under Win32 by Daniel Janus

module Curses (
    --------------------------------------------------------------------
    
    Window,      -- data Window deriving Eq
    stdScr,      -- :: Window
    initScr,     -- :: IO Window
    cBreak,      -- :: Bool -> IO ()
    raw,         -- :: Bool -> IO ()
    echo,        -- :: Bool -> IO ()
    nl,          -- :: Bool -> IO ()
    intrFlush,   -- :: Bool -> IO ()
    keypad,      -- :: Window -> Bool -> IO ()
    initCurses,  -- :: IO ()
    endWin,      -- :: IO ()
    
    --------------------------------------------------------------------
    
    scrSize,          -- :: IO (Int, Int)
    refresh,          -- :: IO ()
#if HAVE_UNICODE
    setCursesOutConv, -- :: IO (Conv Char Byte) -> IO ()
#endif
    
    --------------------------------------------------------------------
    
    hasColors,      -- :: IO Bool
    startColor,     -- :: IO ()
    Pair(..),       -- newtype Pair = Pair Int deriving (Eq, Ord, Ix)
    colorPairs,     -- :: IO Int
    Color(..),      -- newtype Color = Color Int deriving (Eq, Ord, Ix)
    colors,         -- :: IO Int
    black, red, green, yellow, blue, magenta, cyan, white, -- :: Color
    initPair,       -- :: Pair -> Color -> Color -> IO ()
    pairContent,    -- :: Pair -> IO (Color, Color)
    canChangeColor, -- :: IO Bool
    initColor,      -- :: Color -> (Int, Int, Int) -> IO ()
    colorContent,   -- :: Color -> IO (Int, Int, Int)
    
    --------------------------------------------------------------------
    
    Attr,  -- data Attr deriving Eq
    attr0, -- :: Attr
    
    isAltCharset, isBlink, isBold, isDim, isInvis,
    isProtect, isReverse, isStandout, 
    isUnderline, 
        -- :: Attr -> Bool
    
    setAltCharset, setBlink, setBold, setDim, setInvis,
    setProtect, setReverse, setStandout,
    setUnderline, 
        -- :: Attr -> Bool -> Attr
    
    attrSet, -- :: Attr -> Pair -> IO ()
    
    --------------------------------------------------------------------

    ulCorner, llCorner, urCorner, lrCorner, rTee, lTee, bTee, tTee,
    hLine, vLine, plus, s1, s9, diamond, ckBoard, degree, plMinus,
    bullet, lArrow, rArrow, dArrow, uArrow, board, lantern, block,
    -- The rest of characters is not standard. ncurses says:
    -- "These aren't documented, but a lot of System Vs have them
    -- anyway (you can spot pprryyzz{{||}} in a lot of AT&T terminfo
    -- strings). The ACS_names may not match AT&T's, our source
    -- didn't know them."
    s3, s7, lEqual, gEqual, pi, nEqual, sterling,
        -- :: Char
    
    addStr,        -- :: String -> IO ()
    addStrLn,      -- :: String -> IO ()
    addGraphStr,   -- :: String -> IO ()
    addGraphStrLn, -- :: String -> IO ()
    addLn,         -- :: IO ()
    
    --------------------------------------------------------------------
    
    bkgrndSet,      -- :: Attr -> Pair -> IO ()
    erase,          -- :: IO ()
    clrToEol,       -- :: IO ()
    move,           -- :: Int -> Int -> IO ()
    Visibility(..), -- data Visibility = Invisible | Visible | VeryVisible
    cursSet,        -- :: Visibility -> IO Visibility
    
    --------------------------------------------------------------------
    
    Key(..),
        -- data Key
        --     = KeyChar Char | KeyBreak | KeyDown | KeyUp | KeyLeft
        --     | KeyRight | KeyHome | KeyBackspace | KeyF Int | KeyDL
        --     | KeyIL | KeyDC | KeyIC | KeyEIC | KeyClear | KeyEOS
        --     | KeyEOL | KeySF | KeySR | KeyNPage | KeyPPage | KeySTab
        --     | KeyCTab | KeyCATab | KeyEnter | KeySReset | KeyReset
        --     | KeyPrint | KeyLL | KeyA1 | KeyA3 | KeyB2 | KeyC1
        --     | KeyC3 | KeyBTab | KeyBeg | KeyCancel | KeyClose
        --     | KeyCommand | KeyCopy | KeyCreate | KeyEnd | KeyExit
        --     | KeyFind | KeyHelp | KeyMark | KeyMessage | KeyMove
        --     | KeyNext | KeyOpen | KeyOptions | KeyPrevious | KeyRedo
        --     | KeyReference | KeyRefresh | KeyReplace | KeyRestart
        --     | KeyResume | KeySave | KeySBeg | KeySCancel
        --     | KeySCommand | KeySCopy | KeySCreate | KeySDC | KeySDL
        --     | KeySelect | KeySEnd | KeySEOL | KeySExit | KeySFind
        --     | KeySHelp | KeySHome | KeySIC | KeySLeft | KeySMessage
        --     | KeySMove | KeySNext | KeySOptions | KeySPrevious
        --     | KeySPrint | KeySRedo | KeySReplace | KeySRight
        --     | KeySRsume | KeySSave | KeySSuspend | KeySUndo
        --     | KeySuspend | KeyUndo | KeyUnknown Int
        --     deriving Eq
    getCh) -- :: IO Key
    
    --------------------------------------------------------------------
    where

import Prelude hiding (pi)
import Control.Monad          (liftM)
import Data.Char           (chr, ord)
import Data.Ix             (Ix)
import Data.Bits
import System.IO.Unsafe (unsafePerformIO)

import Foreign
import Foreign.C
#if HAVE_UNICODE
import QCompilerDepend
import ConvBase        (Conv, Byte)
import ConvLocal       (toLocal)
#endif

#if HAVE_NCURSES_CURSES_H
#include <ncurses/curses.h>
#elif HAVE_CURSES_H
#include <curses.h>
#else
#error Where is curses.h?
#endif

------------------------------------------------------------------------

throwIfErr :: (Eq a, Num a) => String -> IO a -> IO a
throwIfErr name act = do
    res <- act
    if res == (#const ERR)
        then ioError (userError ("Curses: "++name++" failed"))
        else return res

throwIfErr_ :: (Eq a, Num a) => String -> IO a -> IO ()
throwIfErr_ name act = void $ throwIfErr name act

------------------------------------------------------------------------

data WindowTag = WindowTag
type Window = Ptr WindowTag

stdScr :: Window
stdScr = unsafePerformIO (peek stdscr)
#if !HAVE_FOREIGN_LABEL
foreign import ccall unsafe "hs_stdscr" stdscr :: Ptr Window
#def inline WINDOW **hs_stdscr (void) {return &stdscr;}
#else
foreign import ccall unsafe "&stdscr" stdscr :: Ptr Window
#endif

foreign import ccall unsafe "initscr" initScr :: IO Window

cBreak :: Bool -> IO ()
cBreak True  = throwIfErr_ "cbreak"   cbreak
cBreak False = throwIfErr_ "nocbreak" nocbreak
foreign import ccall unsafe cbreak :: IO CInt
foreign import ccall unsafe nocbreak :: IO CInt

raw :: Bool -> IO ()
raw False = throwIfErr_ "noraw" noraw
raw True  = throwIfErr_ "raw"   raw_c
foreign import ccall unsafe noraw :: IO CInt
foreign import ccall unsafe "raw" raw_c :: IO CInt

echo :: Bool -> IO ()
echo False = throwIfErr_ "noecho" noecho
echo True  = throwIfErr_ "echo"   echo_c
foreign import ccall unsafe noecho :: IO CInt
foreign import ccall unsafe "echo" echo_c :: IO CInt

nl :: Bool -> IO ()
nl True  = throwIfErr_ "nl"   nl_c
nl False = throwIfErr_ "nonl" nonl
foreign import ccall unsafe "nl" nl_c :: IO CInt
foreign import ccall unsafe nonl :: IO CInt

intrFlush :: Bool -> IO ()
intrFlush bf =
    throwIfErr_ "intrflush" $ intrflush stdScr (if bf then 1 else 0)
foreign import ccall unsafe intrflush :: Window -> (#type bool) -> IO CInt

keypad :: Window -> Bool -> IO ()
keypad win bf =
    throwIfErr_ "keypad" $ keypad_c win (if bf then 1 else 0)
foreign import ccall unsafe "keypad" keypad_c :: Window -> (#type bool) -> IO CInt

initCurses :: IO ()
initCurses = do
    initScr; cbreak; noecho; nonl; intrflush stdScr 0; keypad_c stdScr 1; return ()
--    initScr; cBreak True; echo False; nl False; intrFlush False; keypad stdScr True

endWin :: IO ()
endWin = throwIfErr_ "endwin" endwin
foreign import ccall unsafe endwin :: IO CInt

------------------------------------------------------------------------

scrSize :: IO (Int, Int)
scrSize = do
    lines <- peek linesPtr
    cols  <- peek colsPtr
    return (fromIntegral lines, fromIntegral cols)
#if !HAVE_FOREIGN_LABEL
foreign import ccall unsafe "hs_lines" linesPtr :: Ptr CInt
#def inline int *hs_lines (void) {return &LINES;}
foreign import "hs_cols" unsafe colsPtr :: Ptr CInt
#def inline int *hs_cols (void) {return &COLS;}
#else
foreign import ccall "&LINES" linesPtr :: Ptr CInt
foreign import ccall "&COLS"  colsPtr  :: Ptr CInt
#endif

refresh :: IO ()
refresh = throwIfErr_ "refresh" refresh_c
foreign import ccall unsafe "refresh" refresh_c :: IO CInt

#if HAVE_UNICODE
setCursesOutConv :: IO (Conv Char Byte) -> IO ()
setCursesOutConv newConv = newConv >>= writeIORef cursesOutConv

{-# NOINLINE cursesOutConv #-}
cursesOutConv :: IORef (Conv Char Byte)
cursesOutConv = unsafePerformIO $ newIORef (unsafePerformIO toLocal)
#endif

------------------------------------------------------------------------

hasColors :: IO Bool
hasColors = liftM (/= 0) has_colors
foreign import ccall unsafe has_colors :: IO (#type bool)

startColor :: IO ()
startColor = throwIfErr_ "start_color" start_color
foreign import ccall unsafe start_color :: IO CInt

newtype Pair = Pair Int deriving (Eq, Ord, Ix)

colorPairs :: IO Int
colorPairs = liftM fromIntegral $ peek colorPairsPtr
#if !HAVE_FOREIGN_LABEL
foreign import ccall unsafe "hs_color_pairs" colorPairsPtr :: Ptr CInt
#def inline int *hs_color_pairs (void) {return &COLOR_PAIRS;}
#else
foreign import ccall "&COLOR_PAIRS" colorPairsPtr :: Ptr CInt
#endif

newtype Color = Color Int deriving (Eq, Ord, Ix)

colors :: IO Int
colors = liftM fromIntegral $ peek colorsPtr
#if !HAVE_FOREIGN_LABEL
foreign import ccall unsafe "hs_colors" colorsPtr :: Ptr CInt
#def inline int *hs_colors (void) {return &COLORS;}
#else
foreign import ccall "&COLORS" colorsPtr :: Ptr CInt
#endif

black, red, green, yellow, blue, magenta, cyan, white :: Color
black   = Color (#const COLOR_BLACK)
red     = Color (#const COLOR_RED)
green   = Color (#const COLOR_GREEN)
yellow  = Color (#const COLOR_YELLOW)
blue    = Color (#const COLOR_BLUE)
magenta = Color (#const COLOR_MAGENTA)
cyan    = Color (#const COLOR_CYAN)
white   = Color (#const COLOR_WHITE)

initPair :: Pair -> Color -> Color -> IO ()
initPair (Pair p) (Color f) (Color b) =
    throwIfErr_ "init_pair" $
        init_pair (fromIntegral p) (fromIntegral f) (fromIntegral b)
foreign import ccall unsafe init_pair :: CShort -> CShort -> CShort -> IO CInt

pairContent :: Pair -> IO (Color, Color)
pairContent (Pair p) =
    alloca $ \fPtr ->
    alloca $ \bPtr -> do
        throwIfErr "pair_content" $ pair_content (fromIntegral p) fPtr bPtr
        f <- peek fPtr
        b <- peek bPtr
        return (Color (fromIntegral f), Color (fromIntegral b))
foreign import ccall unsafe pair_content :: CShort -> Ptr CShort -> Ptr CShort -> IO CInt

canChangeColor :: IO Bool
canChangeColor = liftM (/= 0) can_change_color
foreign import ccall unsafe can_change_color :: IO (#type bool)

initColor :: Color -> (Int, Int, Int) -> IO ()
initColor (Color c) (r, g, b) =
    throwIfErr_ "init_color" $
        init_color (fromIntegral c) (fromIntegral r) (fromIntegral g) (fromIntegral b)
foreign import ccall unsafe init_color :: CShort -> CShort -> CShort -> CShort -> IO CInt

colorContent :: Color -> IO (Int, Int, Int)
colorContent (Color c) =
    alloca $ \rPtr ->
    alloca $ \gPtr ->
    alloca $ \bPtr -> do
        throwIfErr "color_content" $ color_content (fromIntegral c) rPtr gPtr bPtr
        r <- peek rPtr
        g <- peek gPtr
        b <- peek bPtr
        return (fromIntegral r, fromIntegral g, fromIntegral b)
foreign import ccall unsafe color_content :: CShort -> Ptr CShort -> Ptr CShort -> Ptr CShort -> IO CInt

foreign import ccall unsafe "hs_curses_color_pair" colorPair :: Pair -> (#type chtype)
#def chtype hs_curses_color_pair (HsInt pair) {return COLOR_PAIR (pair);}

------------------------------------------------------------------------

newtype Attr = Attr (#type attr_t) deriving Eq

attr0 :: Attr
attr0 = Attr 0

isAltCharset, isBlink, isBold, isDim, isInvis,
    isProtect, isReverse, isStandout, 
    isUnderline
    :: Attr -> Bool
isAltCharset = isAttr (#const A_ALTCHARSET)
isBlink      = isAttr (#const A_BLINK)
isBold       = isAttr (#const A_BOLD)
isDim        = isAttr (#const A_DIM)
isInvis      = isAttr (#const A_INVIS)
isProtect    = isAttr (#const A_PROTECT)
isReverse    = isAttr (#const A_REVERSE)
isStandout   = isAttr (#const A_STANDOUT)
isUnderline  = isAttr (#const A_UNDERLINE)

isAttr :: (#type attr_t) -> Attr -> Bool
isAttr bit (Attr a) = a .&. bit /= 0

setAltCharset, setBlink, setBold, setDim, setInvis,
    setProtect, setReverse, setStandout,
    setUnderline
    :: Attr -> Bool -> Attr
setAltCharset = setAttr (#const A_ALTCHARSET)
setBlink      = setAttr (#const A_BLINK)
setBold       = setAttr (#const A_BOLD)
setDim        = setAttr (#const A_DIM)
setInvis      = setAttr (#const A_INVIS)
setProtect    = setAttr (#const A_PROTECT)
setReverse    = setAttr (#const A_REVERSE)
setStandout   = setAttr (#const A_STANDOUT)
setUnderline  = setAttr (#const A_UNDERLINE)

setAttr :: (#type attr_t) -> Attr -> Bool -> Attr
setAttr bit (Attr a) False = Attr (a .&. complement bit)
setAttr bit (Attr a) True  = Attr (a .|.            bit)

attrSet :: Attr -> Pair -> IO ()
attrSet (Attr attr) (Pair p) = throwIfErr_ "attrset" $
    wattrset stdScr (fromIntegral (attr .|. (fromIntegral (colorPair (Pair p)))))
foreign import ccall unsafe wattrset :: Window -> CInt -> IO Int

------------------------------------------------------------------------

#if HAVE_UNICODE

ulCorner, llCorner, urCorner, lrCorner, rTee, lTee, bTee, tTee, hLine,
    vLine, plus, s1, s9, diamond, ckBoard, degree, plMinus, bullet,
    lArrow, rArrow, dArrow, uArrow, board, lantern, block,
    s3, s7, lEqual, gEqual, pi, nEqual, sterling
    :: Char

ulCorner = chr 0x250C
llCorner = chr 0x2514
urCorner = chr 0x2510
lrCorner = chr 0x2518
rTee     = chr 0x2524
lTee     = chr 0x251C
bTee     = chr 0x2534
tTee     = chr 0x252C
hLine    = chr 0x2500
vLine    = chr 0x2502
plus     = chr 0x253C
s1       = chr 0x23BA -- was: 0xF800
s9       = chr 0x23BD -- was: 0xF804
diamond  = chr 0x25C6
ckBoard  = chr 0x2592
degree   = chr 0x00B0
plMinus  = chr 0x00B1
bullet   = chr 0x00B7
lArrow   = chr 0x2190
rArrow   = chr 0x2192
dArrow   = chr 0x2193
uArrow   = chr 0x2191
board    = chr 0x2591
lantern  = chr 0x256C
block    = chr 0x2588
s3       = chr 0x23BB -- was: 0xF801
s7       = chr 0x23BC -- was: 0xF803
lEqual   = chr 0x2264
gEqual   = chr 0x2265
pi       = chr 0x03C0
nEqual   = chr 0x2260
sterling = chr 0x00A3

recognize :: Char -> IO a -> ((#type chtype) -> IO a) -> IO a
recognize ch noConvert convert
    | ch <= '\x7F'   = noConvert -- Handle the most common case first.
    | ch == ulCorner = convert =<< hs_curses_acs_ulcorner
    | ch == llCorner = convert =<< hs_curses_acs_llcorner
    | ch == urCorner = convert =<< hs_curses_acs_urcorner
    | ch == lrCorner = convert =<< hs_curses_acs_lrcorner
    | ch == rTee     = convert =<< hs_curses_acs_rtee
    | ch == lTee     = convert =<< hs_curses_acs_ltee
    | ch == bTee     = convert =<< hs_curses_acs_btee
    | ch == tTee     = convert =<< hs_curses_acs_ttee
    | ch == hLine    = convert =<< hs_curses_acs_hline
    | ch == vLine    = convert =<< hs_curses_acs_vline
    | ch == plus     = convert =<< hs_curses_acs_plus
    | ch == s1       = convert =<< hs_curses_acs_s1
    | ch == s9       = convert =<< hs_curses_acs_s9
    | ch == diamond  = convert =<< hs_curses_acs_diamond
    | ch == ckBoard  = convert =<< hs_curses_acs_ckboard
    | ch == degree   = convert =<< hs_curses_acs_degree
    | ch == plMinus  = convert =<< hs_curses_acs_plminus
    | ch == bullet   = convert =<< hs_curses_acs_bullet
    | ch == lArrow   = convert =<< hs_curses_acs_larrow
    | ch == rArrow   = convert =<< hs_curses_acs_rarrow
    | ch == dArrow   = convert =<< hs_curses_acs_darrow
    | ch == uArrow   = convert =<< hs_curses_acs_uarrow
    | ch == board    = convert =<< hs_curses_acs_board
    | ch == lantern  = convert =<< hs_curses_acs_lantern
    | ch == block    = convert =<< hs_curses_acs_block
#  ifdef ACS_S3
    | ch == s3       = convert =<< hs_curses_acs_s3
    | ch == s7       = convert =<< hs_curses_acs_s7
    | ch == lEqual   = convert =<< hs_curses_acs_lequal
    | ch == gEqual   = convert =<< hs_curses_acs_gequal
    | ch == pi       = convert =<< hs_curses_acs_pi
    | ch == nEqual   = convert =<< hs_curses_acs_nequal
    | ch == sterling = convert =<< hs_curses_acs_sterling
#  endif
    | otherwise      = noConvert

foreign import ccall unsafe hs_curses_acs_ulcorner :: IO (#type chtype)
foreign import ccall unsafe hs_curses_acs_llcorner :: IO (#type chtype)
foreign import ccall unsafe hs_curses_acs_urcorner :: IO (#type chtype)
foreign import ccall unsafe hs_curses_acs_lrcorner :: IO (#type chtype)
foreign import ccall unsafe hs_curses_acs_rtee     :: IO (#type chtype)
foreign import ccall unsafe hs_curses_acs_ltee     :: IO (#type chtype)
foreign import ccall unsafe hs_curses_acs_btee     :: IO (#type chtype)
foreign import ccall unsafe hs_curses_acs_ttee     :: IO (#type chtype)
foreign import ccall unsafe hs_curses_acs_hline    :: IO (#type chtype)
foreign import ccall unsafe hs_curses_acs_vline    :: IO (#type chtype)
foreign import ccall unsafe hs_curses_acs_plus     :: IO (#type chtype)
foreign import ccall unsafe hs_curses_acs_s1       :: IO (#type chtype)
foreign import ccall unsafe hs_curses_acs_s9       :: IO (#type chtype)
foreign import ccall unsafe hs_curses_acs_diamond  :: IO (#type chtype)
foreign import ccall unsafe hs_curses_acs_ckboard  :: IO (#type chtype)
foreign import ccall unsafe hs_curses_acs_degree   :: IO (#type chtype)
foreign import ccall unsafe hs_curses_acs_plminus  :: IO (#type chtype)
foreign import ccall unsafe hs_curses_acs_bullet   :: IO (#type chtype)
foreign import ccall unsafe hs_curses_acs_larrow   :: IO (#type chtype)
foreign import ccall unsafe hs_curses_acs_rarrow   :: IO (#type chtype)
foreign import ccall unsafe hs_curses_acs_darrow   :: IO (#type chtype)
foreign import ccall unsafe hs_curses_acs_uarrow   :: IO (#type chtype)
foreign import ccall unsafe hs_curses_acs_board    :: IO (#type chtype)
foreign import ccall unsafe hs_curses_acs_lantern  :: IO (#type chtype)
foreign import ccall unsafe hs_curses_acs_block    :: IO (#type chtype)
#  ifdef ACS_S3
foreign import ccall unsafe hs_curses_acs_s3       :: IO (#type chtype)
foreign import ccall unsafe hs_curses_acs_s7       :: IO (#type chtype)
foreign import ccall unsafe hs_curses_acs_lequal   :: IO (#type chtype)
foreign import ccall unsafe hs_curses_acs_gequal   :: IO (#type chtype)
foreign import ccall unsafe hs_curses_acs_pi       :: IO (#type chtype)
foreign import ccall unsafe hs_curses_acs_nequal   :: IO (#type chtype)
foreign import ccall unsafe hs_curses_acs_sterling :: IO (#type chtype)
#  endif

#def inline chtype hs_curses_acs_ulcorner (void) {return ACS_ULCORNER;}
#def inline chtype hs_curses_acs_llcorner (void) {return ACS_LLCORNER;}
#def inline chtype hs_curses_acs_urcorner (void) {return ACS_URCORNER;}
#def inline chtype hs_curses_acs_lrcorner (void) {return ACS_LRCORNER;}
#def inline chtype hs_curses_acs_rtee     (void) {return ACS_RTEE;}
#def inline chtype hs_curses_acs_ltee     (void) {return ACS_LTEE;}
#def inline chtype hs_curses_acs_btee     (void) {return ACS_BTEE;}
#def inline chtype hs_curses_acs_ttee     (void) {return ACS_TTEE;}
#def inline chtype hs_curses_acs_hline    (void) {return ACS_HLINE;}
#def inline chtype hs_curses_acs_vline    (void) {return ACS_VLINE;}
#def inline chtype hs_curses_acs_plus     (void) {return ACS_PLUS;}
#def inline chtype hs_curses_acs_s1       (void) {return ACS_S1;}
#def inline chtype hs_curses_acs_s9       (void) {return ACS_S9;}
#def inline chtype hs_curses_acs_diamond  (void) {return ACS_DIAMOND;}
#def inline chtype hs_curses_acs_ckboard  (void) {return ACS_CKBOARD;}
#def inline chtype hs_curses_acs_degree   (void) {return ACS_DEGREE;}
#def inline chtype hs_curses_acs_plminus  (void) {return ACS_PLMINUS;}
#def inline chtype hs_curses_acs_bullet   (void) {return ACS_BULLET;}
#def inline chtype hs_curses_acs_larrow   (void) {return ACS_LARROW;}
#def inline chtype hs_curses_acs_rarrow   (void) {return ACS_RARROW;}
#def inline chtype hs_curses_acs_darrow   (void) {return ACS_DARROW;}
#def inline chtype hs_curses_acs_uarrow   (void) {return ACS_UARROW;}
#def inline chtype hs_curses_acs_board    (void) {return ACS_BOARD;}
#def inline chtype hs_curses_acs_lantern  (void) {return ACS_LANTERN;}
#def inline chtype hs_curses_acs_block    (void) {return ACS_BLOCK;}
#  ifdef ACS_S3
#def inline chtype hs_curses_acs_s3       (void) {return ACS_S3;}
#def inline chtype hs_curses_acs_s7       (void) {return ACS_S7;}
#def inline chtype hs_curses_acs_lequal   (void) {return ACS_LEQUAL;}
#def inline chtype hs_curses_acs_gequal   (void) {return ACS_GEQUAL;}
#def inline chtype hs_curses_acs_pi       (void) {return ACS_PI;}
#def inline chtype hs_curses_acs_nequal   (void) {return ACS_NEQUAL;}
#def inline chtype hs_curses_acs_sterling (void) {return ACS_STERLING;}
#  endif

addStr :: String -> IO ()
addStr str =
--    throwIfErr_ "addstr" $
-- NOTE: The ncurses implementation of addch() (and, consequently, addstr())
-- returns ERR when the cursor is at the bottom-left corner of the window
-- and cannot be advanced. I don't want this to be checked, so I throw out
-- the result and pretend to succeed every time.
    void $ 
    withCStringConv (readIORef cursesOutConv) str addstr
foreign import ccall unsafe addstr :: Ptr CChar -> IO CInt

addStrLn :: String -> IO ()
addStrLn str = do addStr str; addLn

addGraphStr :: String -> IO ()
addGraphStr str = do
    conv <- readIORef cursesOutConv
    let
        convStr f = case f [] of
            [] -> return ()
            s  -> throwIfErr_ "addstr" $
                withCStringConv (return conv) s addstr
        loop []        acc = convStr acc
        loop (ch:str') acc = recognize
            ch
            (loop str' (acc . (ch:)))
            (\ch' -> do
                convStr acc
                -- throwIfErr "addch" $ addch ch'
                void $ addch ch' -- see above
                loop str' id)
    loop str id

addGraphStrLn :: String -> IO ()
addGraphStrLn str = do addGraphStr str; addLn

#else /* !HAVE_UNICODE */

ulCorner, llCorner, urCorner, lrCorner, rTee, lTee, bTee, tTee, hLine,
    vLine, plus, s1, s9, diamond, ckBoard, degree, plMinus, bullet,
    lArrow, rArrow, dArrow, uArrow, board, lantern, block,
    s3, s7, lEqual, gEqual, pi, nEqual, sterling
    :: Char

ulCorner = '+'
llCorner = '+'
urCorner = '+'
lrCorner = '+'
rTee     = '+'
lTee     = '+'
bTee     = '+'
tTee     = '+'
hLine    = '-'
vLine    = '|'
plus     = '+'
s1       = '-'
s9       = '_'
diamond  = '+'
ckBoard  = ':'
degree   = '\''
plMinus  = '#'
bullet   = 'o'
lArrow   = '<'
rArrow   = '>'
dArrow   = 'v'
uArrow   = '^'
board    = '#'
lantern  = '#'
block    = '#'
s3       = '-'
s7       = '-'
lEqual   = '<'
gEqual   = '>'
pi       = '*'
nEqual   = '!'
sterling = 'f'

addStr :: String -> IO ()
addStr str =
--    throwIfErr_ "addstr" $ withCString str addstr
-- NOTE: The ncurses implementation of addch() (and, consequently, addstr())
-- returns ERR when the cursor is at the bottom-left corner of the window
-- and cannot be advanced. I don't want this to be checked, so I throw out
-- the result and pretend to succeed every time.
    void $ withCString str addstr
foreign import ccall unsafe addstr :: Ptr CChar -> IO CInt

addStrLn :: String -> IO ()
addStrLn str = do addStr str; addLn

addGraphStr :: String -> IO ()
addGraphStr = addStr

addGraphStrLn :: String -> IO ()
addGraphStrLn = addStrLn

#endif /* !HAVE_UNICODE */

addLn :: IO ()
-- addLn = throwIfErr_ "addch" $ addch (fromIntegral (ord '\n'))
addLn = void $ addch (fromIntegral (ord '\n')) -- see above

foreign import ccall unsafe addch :: (#type chtype) -> IO CInt

------------------------------------------------------------------------

#let translate_attr attr =                              \
    "(if a .&. %lu /= 0 then %lu else 0) .|.",          \
    (unsigned long) A_##attr, (unsigned long) A_##attr

bkgrndSet :: Attr -> Pair -> IO ()
bkgrndSet (Attr a) p = bkgdset $
    fromIntegral (ord ' ') .|.
    #translate_attr ALTCHARSET
    #translate_attr BLINK
    #translate_attr BOLD
    #translate_attr DIM
    #translate_attr INVIS
    #translate_attr PROTECT
    #translate_attr REVERSE
    #translate_attr STANDOUT
    #translate_attr UNDERLINE
    colorPair p
foreign import ccall unsafe bkgdset :: (#type chtype) -> IO ()

erase :: IO ()
erase = throwIfErr_ "erase" erase_c
foreign import ccall unsafe "erase" erase_c :: IO CInt

clrToEol :: IO ()
clrToEol = throwIfErr_ "clrtoeol" clrtoeol
foreign import ccall unsafe clrtoeol :: IO CInt

move :: Int -> Int -> IO ()
move y x =
    throwIfErr_ "move" $ move_c (fromIntegral y) (fromIntegral x)
foreign import ccall unsafe "move" move_c :: CInt -> CInt -> IO CInt

data Visibility = Invisible | Visible | VeryVisible

cursSet :: Visibility -> IO Visibility
cursSet vis = do
    oldVis_c <- throwIfErr "curs_set" $ curs_set vis_c
    case oldVis_c of
        0 -> return Invisible
        1 -> return Visible
        2 -> return VeryVisible
        _ -> ioError $ userError $
            "Curses: Unexpected curs_set result ("++show oldVis_c++")"
    where
    vis_c :: CInt
    vis_c = case vis of
        Invisible   -> 0
        Visible     -> 1
        VeryVisible -> 2
foreign import ccall unsafe curs_set :: CInt -> IO CInt

------------------------------------------------------------------------

data Key
    = KeyChar Char | KeyBreak | KeyDown | KeyUp | KeyLeft | KeyRight
    | KeyHome | KeyBackspace | KeyF Int | KeyDL | KeyIL | KeyDC
    | KeyIC | KeyEIC | KeyClear | KeyEOS | KeyEOL | KeySF | KeySR
    | KeyNPage | KeyPPage | KeySTab | KeyCTab | KeyCATab | KeyEnter
    | KeySReset | KeyReset | KeyPrint | KeyLL | KeyA1 | KeyA3
    | KeyB2 | KeyC1 | KeyC3 | KeyBTab | KeyBeg | KeyCancel | KeyClose
    | KeyCommand | KeyCopy | KeyCreate | KeyEnd | KeyExit | KeyFind
    | KeyHelp | KeyMark | KeyMessage | KeyMove | KeyNext | KeyOpen
    | KeyOptions | KeyPrevious | KeyRedo | KeyReference | KeyRefresh
    | KeyReplace | KeyRestart | KeyResume | KeySave | KeySBeg
    | KeySCancel | KeySCommand | KeySCopy | KeySCreate | KeySDC
    | KeySDL | KeySelect | KeySEnd | KeySEOL | KeySExit | KeySFind
    | KeySHelp | KeySHome | KeySIC | KeySLeft | KeySMessage | KeySMove
    | KeySNext | KeySOptions | KeySPrevious | KeySPrint | KeySRedo
    | KeySReplace | KeySRight | KeySRsume | KeySSave | KeySSuspend
    | KeySUndo | KeySuspend | KeyUndo | KeyUnknown Int
    deriving (Eq, Show)

decodeKey :: CInt -> Key
decodeKey key = case key of
    _ | key >= 0 && key <= 255 -> KeyChar (chr (fromIntegral key))
    (#const KEY_BREAK)         -> KeyBreak
    (#const KEY_DOWN)          -> KeyDown
    (#const KEY_UP)            -> KeyUp
    (#const KEY_LEFT)          -> KeyLeft
    (#const KEY_RIGHT)         -> KeyRight
    (#const KEY_HOME)          -> KeyHome
    (#const KEY_BACKSPACE)     -> KeyBackspace
    _ | key >= (#const KEY_F0) && key <= (#const KEY_F(63))
                               -> KeyF (fromIntegral (key - #const KEY_F0))
    (#const KEY_DL)            -> KeyDL
    (#const KEY_IL)            -> KeyIL
    (#const KEY_DC)            -> KeyDC
    (#const KEY_IC)            -> KeyIC
    (#const KEY_EIC)           -> KeyEIC
    (#const KEY_CLEAR)         -> KeyClear
    (#const KEY_EOS)           -> KeyEOS
    (#const KEY_EOL)           -> KeyEOL
    (#const KEY_SF)            -> KeySF
    (#const KEY_SR)            -> KeySR
    (#const KEY_NPAGE)         -> KeyNPage
    (#const KEY_PPAGE)         -> KeyPPage
    (#const KEY_STAB)          -> KeySTab
    (#const KEY_CTAB)          -> KeyCTab
    (#const KEY_CATAB)         -> KeyCATab
    (#const KEY_ENTER)         -> KeyEnter
    (#const KEY_SRESET)        -> KeySReset
    (#const KEY_RESET)         -> KeyReset
    (#const KEY_PRINT)         -> KeyPrint
    (#const KEY_LL)            -> KeyLL
    (#const KEY_A1)            -> KeyA1
    (#const KEY_A3)            -> KeyA3
    (#const KEY_B2)            -> KeyB2
    (#const KEY_C1)            -> KeyC1
    (#const KEY_C3)            -> KeyC3
    (#const KEY_BTAB)          -> KeyBTab
    (#const KEY_BEG)           -> KeyBeg
    (#const KEY_CANCEL)        -> KeyCancel
    (#const KEY_CLOSE)         -> KeyClose
    (#const KEY_COMMAND)       -> KeyCommand
    (#const KEY_COPY)          -> KeyCopy
    (#const KEY_CREATE)        -> KeyCreate
    (#const KEY_END)           -> KeyEnd
    (#const KEY_EXIT)          -> KeyExit
    (#const KEY_FIND)          -> KeyFind
    (#const KEY_HELP)          -> KeyHelp
    (#const KEY_MARK)          -> KeyMark
    (#const KEY_MESSAGE)       -> KeyMessage
    (#const KEY_MOVE)          -> KeyMove
    (#const KEY_NEXT)          -> KeyNext
    (#const KEY_OPEN)          -> KeyOpen
    (#const KEY_OPTIONS)       -> KeyOptions
    (#const KEY_PREVIOUS)      -> KeyPrevious
    (#const KEY_REDO)          -> KeyRedo
    (#const KEY_REFERENCE)     -> KeyReference
    (#const KEY_REFRESH)       -> KeyRefresh
    (#const KEY_REPLACE)       -> KeyReplace
    (#const KEY_RESTART)       -> KeyRestart
    (#const KEY_RESUME)        -> KeyResume
    (#const KEY_SAVE)          -> KeySave
    (#const KEY_SBEG)          -> KeySBeg
    (#const KEY_SCANCEL)       -> KeySCancel
    (#const KEY_SCOMMAND)      -> KeySCommand
    (#const KEY_SCOPY)         -> KeySCopy
    (#const KEY_SCREATE)       -> KeySCreate
    (#const KEY_SDC)           -> KeySDC
    (#const KEY_SDL)           -> KeySDL
    (#const KEY_SELECT)        -> KeySelect
    (#const KEY_SEND)          -> KeySEnd
    (#const KEY_SEOL)          -> KeySEOL
    (#const KEY_SEXIT)         -> KeySExit
    (#const KEY_SFIND)         -> KeySFind
    (#const KEY_SHELP)         -> KeySHelp
    (#const KEY_SHOME)         -> KeySHome
    (#const KEY_SIC)           -> KeySIC
    (#const KEY_SLEFT)         -> KeySLeft
    (#const KEY_SMESSAGE)      -> KeySMessage
    (#const KEY_SMOVE)         -> KeySMove
    (#const KEY_SNEXT)         -> KeySNext
    (#const KEY_SOPTIONS)      -> KeySOptions
    (#const KEY_SPREVIOUS)     -> KeySPrevious
    (#const KEY_SPRINT)        -> KeySPrint
    (#const KEY_SREDO)         -> KeySRedo
    (#const KEY_SREPLACE)      -> KeySReplace
    (#const KEY_SRIGHT)        -> KeySRight
    (#const KEY_SRSUME)        -> KeySRsume
    (#const KEY_SSAVE)         -> KeySSave
    (#const KEY_SSUSPEND)      -> KeySSuspend
    (#const KEY_SUNDO)         -> KeySUndo
    (#const KEY_SUSPEND)       -> KeySuspend
    (#const KEY_UNDO)          -> KeyUndo
    _                          -> KeyUnknown (fromIntegral key)

getCh :: IO Key
getCh = liftM decodeKey $ throwIfErr "getch" getch

foreign import ccall unsafe "dummy_getch" getch :: IO CInt
