# About

This is HAZE, the Haskellish Abominable Z-code Emulator.

I wrote it in 2004, ostensibly as a uni class project but also as a way to teach myself Haskell and have fun; I had the infrastructure in place and was trying to run Zork, implementing opcodes, one by one, when finally I saw the familiar message "You are standing in an open field west of a white house, with a boarded front door. There is a small mailbox here." The sense of accomplishment was rewarding and elating.

In 2014, I put the salvaged code on GitHub. In 2023, I got it to build on modern GHC.

Don't treat it as idiomatic Haskell. This code was written mostly in isolation, in pre-Cabal days, originally targetting GHC 6.2. Expect warnings and general breakage.

# Building it

This is what works for me on macOS:

```
$ cabal install random
$ cd src
$ make
```
