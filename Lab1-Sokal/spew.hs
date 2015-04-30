import System.Random
import System.IO
import Data.Array
import Data.List
import Control.Monad.State.Lazy
import System.Environment (getArgs)

type FastModel = Array Int (String, [(Int, Int)])
type RandState = State StdGen

feed :: String -> FastModel
feed raw = listArray (0, (length processModel) - 1) processModel
  where processModel = map read $ lines raw

weighter :: [(Int, Int)] -> Int -> Int
weighter succs rand = wait succs 0
  where
    wait ((weight, st):rest) acc
      | acc + weight >= rand = st
      | otherwise = wait rest (acc + weight)
    wait [] acc = acc

-- select :: [(Int, Int)] -> RandState Maybe Int
-- select corp = fmap (snd $ corp !!) $ state $ randomR (0, sum $ map (snd) corp)
--   where
--     checkAndWack idx
--       | inRange (bounds corp) idx = Just (snd $ corp !!)
--       | otherwise = Nothing
-- -- take a random number between 0 and the sum of the weights
-- if number between

-- runModel :: FastModel -> Int -> RandState Int
-- runModel fm start = iter start where
--   iter idx = do
--     let (word, succs) = fm ! idx
--     succ <- select $ snd $ succs
--     case succ of
--      Nothing -> return $ idx
--      Just w -> fmap (idx:) $ iter w

isTerminator :: String -> Bool
isTerminator s = any (== last s) ['.', '?', '!']

linefill :: Int -> [String] -> String
linefill _ [] = "\n"
linefill n (x:xs) = iter x xs where
    iter x (y:ys)
        | length x + length y + 1 > n = x ++ "\n" ++ linefill n (y:ys)
        | otherwise                   = iter (x ++ " " ++ y) ys
    iter x [] = x ++ "\n"

main = do
  args <- getArgs
  let spewSize = head args
  raw <- readFile "sokal.model"
  let model = feed raw
  print $ [model ! 0, model ! 1, model ! 2, model ! 3]
  -- gen <- getnStdGen
  -- let ws = evalState (runModel model) gen
