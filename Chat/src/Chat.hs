-- | CS240h Lab 2 Chat Server
module Chat (chat) where

import System.Environment (getEnv)
import Network -- (listenOn, withSocketDo, accept, PortId(..), Socket)
import System.IO
import Control.Monad (forever)
import Control.Concurrent (forkFinally)
import Data.IORef (atomicModifyIORef, newIORef, readIORef, IORef)
import Data.List (delete)

type Client = (Int, Handle)
type Peers = [Client] -- list of active clients

sendMsg :: String -> Int -> Client -> IO ()
sendMsg msg me (you, h) = do
  if me /= you then hPutStrLn h msg
    else return ()

join :: IORef Peers -> Socket -> Int -> IO ()
join mps sock id = do
  (handle, _, _) <- accept sock
  hSetBuffering handle LineBuffering
  let msg = (show id) ++ " has joined"
  ps <- readIORef mps
  atomicModifyIORef mps (\peers -> ((id, handle):peers, ()))
  -- hPutStrLn handle $ show id
  mapM_ (sendMsg msg id) ps
  forkFinally (talk (id, handle) mps) (\_ -> leave mps (id, handle))
  return ()

talk :: Client -> IORef Peers -> IO ()
talk (id, h) mps = do
  hSetBuffering h LineBuffering
  loop
    where
      loop = do
        ps <- readIORef mps
        line <- hGetLine h
        mapM_ (sendMsg ((show id) ++ " : " ++ line) id) ps
        loop

leave :: IORef Peers -> Client -> IO ()
leave mps (id, h) = do
  atomicModifyIORef mps (\peers -> (delete (id, h) peers, ()))
  let msg = (show id) ++ " has left."
  ps <- readIORef mps
  mapM_ (sendMsg msg id) ps
  hClose h

-- | Chat server entry point.
chat :: IO ()
chat = withSocketsDo $ do
  port <- getEnv "CHAT_SERVER_PORT"
  sock <- listenOn (PortNumber (fromIntegral (read port :: Int)))
  putStrLn $ "Listening on port " ++ port
  mps <- newIORef ([] :: Peers)
  mapM_ (join mps sock) [1..]
  --return ()
