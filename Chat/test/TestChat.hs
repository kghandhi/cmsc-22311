-- | Test our chat server.
module Main (main) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
import Control.Exception (bracket)
import Control.Monad
import Data.Functor
import Network
import System.Environment (setEnv)
import System.IO
import Test.Hspec

import Chat

port :: Int
port = 1803

main :: IO ()
main = withSocketsDo $ do
  setEnv "CHAT_SERVER_PORT" (show port)
  void $ async chat
  threadDelay 1000
  doTests
  return ()

findNewInput :: Handle -> IO (Maybe String)
findNewInput h = do
  line <- async $ hGetLine h
  threadDelay 1000
  cancel line
  ret <- poll line
  case ret of
   Just (Right s) -> return (Just s)
   _ -> return Nothing


-- gotNothing :: Handle -> IO ()
-- gotNothing h = findNewInput h `shouldReturn` Nothing

getInput :: Handle -> IO String
getInput h = do
  ms <- findNewInput h
  case ms of
   Nothing -> return ""
   Just s -> return s

addClient :: IO Handle
addClient = do
  handle <- connectTo "localhost" (PortNumber (fromIntegral port))
  hSetBuffering handle LineBuffering
  return handle

-- Checks that when a client enters a room they get a hello message
testAcceptClient :: IO (Int, Handle)
testAcceptClient = do
  h <- addClient
  msg <- getInput h
  putStrLn msg
  return (whoSentHello msg, h)

safeHead :: [String] -> String
safeHead [] = "0"
safeHead (a:_) = a

whoSentGoodbye :: String -> Int
whoSentGoodbye msg = read $ safeHead $ words msg

whoSentHello :: String -> Int
whoSentHello = whoSentGoodbye

dyingClient :: (Handle -> IO a) -> IO a
dyingClient f = bracket addClient hClose f

doTests :: IO ()
doTests = hspec $ describe "Testing Lab 2" $ do
  describe "Accepting clients onto server with welcome messages" $ do
    it "first client is number 1" $ do
      (c1, _) <- testAcceptClient
      c1 `shouldBe` 1
    it "increments client numbers" $ do
      (c2, _) <- testAcceptClient
      c2 `shouldBe` 2
      threadDelay 2000
      (c3, _) <- testAcceptClient
      c3 `shouldBe` 3
    it "sends out a welcome message" $ do
      (c4, h4) <- testAcceptClient
      (c5, h5) <- testAcceptClient
      threadDelay 2000
      msg <- getInput h4
      putStrLn msg
      threadDelay 2000
      whoSentHello msg `shouldBe` 5
  describe "Clients can leave with goodbye messages" $ do
    it "sends a goodbye message to all other clients" $ do
      (c6, h6) <- testAcceptClient
      threadDelay 2000
      --c6 `shouldBe` 6
      (c7, h7) <- testAcceptClient
      threadDelay 2000
      (c8, h8) <- testAcceptClient
      threadDelay 2000
      hClose h8
      threadDelay 4000
      msg7 <- getInput h7
      print msg7
      whoSentGoodbye msg7 `shouldBe` 8
      msg6 <- getInput h6
      whoSentGoodbye msg6 `shouldBe` 8
--   -- describe "Clients can leave with parting messages" $ do
--   --   it "server sends goodbye message" $ dyingClient $ \h -> do
--   --     (whoSentHello <$> getInput h) `shouldReturn` 6
--   --     testAcceptClient `shouldReturn` 7
--   --     (whoSentHello <$> getInput h) `shouldReturn` 7
--   --     (whoSentGoodbye <$> getInput h) `shouldReturn` 7
--   -- messages sent

--   -- client ID increments
--   -- client enters is added to peers and sends hello
--   -- client leaves is removed from peers and says bye
--   -- sendMsg sends messages to all but the sender
--   -- clients receive messages when others send them
