-- | CMSC 22311 Lab 2 Chat Server
module Chat (chat) where

import System.Environment (getEnv)
import Network -- (listenOn, withSocketDo, accept, PortId(..), Socket)
import System.IO
import Control.Concurrent (forkFinally)
import Data.IORef (atomicModifyIORef, newIORef, readIORef, IORef)
import Data.List (delete)

type Client = (Int, Handle)
type Peers = [Client] -- list of active clients

sendMsg :: String -> Int -> Client -> IO ()
sendMsg msg me (you, h) = do
  if me /= you then hPutStrLn h msg
    else return ()

acceptClient :: IORef Peers -> Socket -> Int -> IO ()
acceptClient mps sock n = do
  (handle, _, _) <- accept sock
  hSetBuffering handle LineBuffering
  let msg = (show n) ++ " has joined"
  ps <- readIORef mps
  atomicModifyIORef mps (\peers -> ((n, handle):peers, ()))
  -- hPutStrLn handle $ show id
  mapM_ (sendMsg msg n) ps
  forkFinally (talk (n, handle) mps) (\_ -> leave mps (n, handle))
  return ()

talk :: Client -> IORef Peers -> IO ()
talk (n, h) mps = do
  loop
    where
      loop = do
        line <- hGetLine h
        ps <- readIORef mps
        let msg = (show n) ++ " : " ++ line
        mapM_ (sendMsg msg n) ps
        loop

leave :: IORef Peers -> Client -> IO ()
leave mps (n, h) = do
  let msg = (show n) ++ " has left"
  ps <- readIORef mps
  mapM_ (sendMsg msg n) ps
  atomicModifyIORef mps (\peers -> (delete (n, h) peers, ()))
  hClose h
  -- let msg = (show n) ++ " has left."
  -- ps <- readIORef mps
  -- mapM_ (sendMsg msg n) ps
  -- hClose h

-- | Chat server entry point.
chat :: IO ()
chat = withSocketsDo $ do
  port <- getEnv "CHAT_SERVER_PORT"
  sock <- listenOn (PortNumber (fromIntegral (read port :: Int)))
  putStrLn $ "Listening on port " ++ port
  mps <- newIORef ([] :: Peers)
  mapM_ (acceptClient mps sock) [1..]
  --return ()
