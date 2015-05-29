# Running the Chat Server
```
cabal sandbox init
cabal install --dependencies-only --enable-tests
cabal configure --enable-tests
```
Now choose a port `N` where `N` > 1,000.
- If you want to run the tests (for some reason)
`CHAT_SERVER_PORT="N" cabal test`
- If you want to run the server
`CHAT_SERVER_PORT="N" cabal run`
Then you will have to open another terminal and do `telnet localhost N` to connect to the server.
Then you can chat with yourself all you want!
