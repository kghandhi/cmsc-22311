# Tetris
To install you need
1. _Exactly_ ghc version 7.8.4
2. SDL2 version 2.0.3
3. Probably linux

```
cabal sandbox init
cabal install gtk2hs-buildtools
cabal install glib-0.12.5.4
cabal install pango-0.12.5.3
cabal install cairo-0.12.5.3
cabal install helm
cabal install --dependencies-only --enable-tests
cabal configure --enable-tests
```
then to run the test squite
```
cabal test
```
To run the game
```
cabal run
```
