CC      = gcc
HC	= ghc
HSC2HS	= hsc2hs
RESULT	= haze
HFLAGS	= -package ghc -package array
DEFINES = -DHAVE_CURSES_H=1 -DHAVE_FOREIGN_LABEL=1
CFLAGS	= -std=gnu89 $(DEFINES)

POBJS	= Data/FiniteMap.o ZMachine/Random.o ZMachine/Stack.o ZMachine/Screen.o ZMachine/Core.o
POBJS  += ZMachine/Arithmetics.o ZMachine/Text.o ZMachine/Object.o
POBJS  += ZMachine/Lexical.o ZMachine/ZCode/Data.o ZMachine/ZCode/Impl.o
POBJS  += ZMachine/ZCode/Execution.o ZMachine/Processor.o Main.o
OBJECTS	= win32/getch.o Curses.o Curses_hsc.o $(POBJS)

LIBS	=
LDFLAGS = -lncurses

all: $(RESULT)

$(RESULT): $(OBJECTS)
	$(HC) -o $(RESULT) $(OBJECTS) $(LDFLAGS)

win32/getch.o: win32/getch.c
	$(CC) -c $(CFLAGS) -o $@ $<

Curses.o: Curses.hs
	$(HC) -c $(HFLAGS) -ffi Curses.hs

Curses_hsc.o: Curses_hsc.c
	$(HC) -c $(HFLAGS) Curses_hsc.c

%.o: %.hs
	$(HC) -c $(HFLAGS) $<

Curses.hs Curses_hsc.c: Curses.hsc
	$(HSC2HS) $(DEFINES) Curses.hsc

clean:
	rm -rf Curses_hsc.* Curses.hs *.hi ZMachine/*.hi ZMachine/ZCode/*.hi \
	Data/*.hi $(OBJECTS)

mrproper: clean
	rm -rf *.bak *~ ZMachine/*.bak ZMachine/*~ ZMachine/ZCode/*~ $(RESULT)
