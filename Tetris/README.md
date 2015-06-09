# Tetris
To install you need
1. _Exactly_ ghc version 7.8.4
2. SDL2 version 2.0.3 (build from source)
3. Then on linux do:
```
sudo apt-get install libcairo2 libcairo2-dev
sudo apt-get install libpango1.0.0 libpango1.0.0-dev
```
4. Start a sandbox and run these commands in this order:
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
then to run the tests
```
cabal test
```
To run the game
```
cabal run
```
