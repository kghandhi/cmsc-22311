-- | CMSC 22311 Lab 2 Chat Server
module Chat (chat, sendMsg, acceptClient, talk, leave, Peers, Client) where

import Control.Concurrent (forkFinally)
import Data.List (delete)
import Data.IORef (atomicModifyIORef, newIORef, readIORef, IORef)
import Network
import System.Environment (lookupEnv)
import System.IO


type Client = (Int, Handle)
type Peers = [Client] -- list of active clients

-- Sends a message to all other clients on the server excluding `me`
sendMsg :: String -> Int -> Client -> IO ()
sendMsg msg me (you, h) = do
  if me /= you
    then hPutStrLn h msg
    else return ()

-- Accepts connections, modifies the state to add the new connection
-- relays the "n has joined" message to the server and forks
acceptClient :: IORef Peers -> Socket -> Int -> IO ()
acceptClient mps sock n = do
  (h, _, _) <- accept sock
  hSetBuffering h LineBuffering
  let new = (n, h)
  atomicModifyIORef mps (\peers -> (new:peers, ()))
  ps <- readIORef mps
  let msg = (show n) ++ " has joined"
  mapM_ (sendMsg msg n) ps
  hPutStrLn h msg
  _ <- forkFinally (talk new mps) (\_ -> leave mps new)
  return ()

-- Once a client is accepted whenever they write to the socket
-- (their handle) relay that line to the rest of the server and loop
talk :: Client -> IORef Peers -> IO ()
talk (n, h) mps = do
  loop
    where
      loop = do
        line <- hGetLine h
        case line of
         ('\EOT':_) -> return ()
         _ -> do
           ps <- readIORef mps
           let msg = (show n) ++ " : " ++ line
           mapM_ (sendMsg msg n) ps
           loop

-- When the connection to a client is lost (called from the forkFinally)
-- we close their file, remove them from the state, then relay the goodbye
leave :: IORef Peers -> Client -> IO ()
leave mps cli@(n, h) = do
  hClose h
  atomicModifyIORef mps (\peers -> (delete cli peers, ()))
  let msg = (show n) ++ " has left"
  ps <- readIORef mps
  mapM_ (sendMsg msg n) ps

getPort :: String -> PortID
getPort port = PortNumber (fromIntegral (read port :: Int))

-- Open a port from the environment variable, create a socket to listen on
-- initalize the state and then accept each client one at a time and
-- continue doing so with the mapM over the infinite list
-- We start at 1 because the first person to enter is called "1"
-- | Chat server entry point.
chat :: IO ()
chat = withSocketsDo $ do
  mport <- lookupEnv "CHAT_SERVER_PORT"
  case mport of
   Nothing -> putStrLn "CHAT_SERVER_PORT environment variable not set"
   Just port -> do
     sock <- listenOn $ getPort port
     putStrLn $ "Listening on port " ++ port
     mps <- newIORef ([] :: Peers)
     mapM_ (acceptClient mps sock) [1..]
