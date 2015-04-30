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
    wait [] acc = -1

select :: [(Int, Int)] -> RandState Int
select succs = fmap (weighter succs) $ state $ randomR (0, sum $ map (fst) succs)

runModel :: FastModel -> RandState [String]
runModel fm = do
  start <- state.randomR $ bounds fm
  iter start where
    iter idx = do
      let (word, succs) = fm ! idx
      nxt <- select succs
      case inRange (bounds fm) nxt of
       False -> fmap (word:) $ (state . randomR) (bounds fm) >>= (iter)
       True -> fmap (word:) $ iter nxt

isTerminator :: String -> Bool
isTerminator s = any (== last s) ['.', '?', '!']

takeEnough :: Int -> [String] -> [String]
takeEnough need ws = map fst $ takeWhile' notDone $ zip ws [1..]
  where
    notDone (w, i) = (i < need) || ((i >= need) && ((not . isTerminator) w))
    takeWhile' _ [] = []
    takeWhile' p (x:xs)
      | p x = x:(takeWhile' p xs)
      | otherwise = [x]

linefill :: Int -> [String] -> String
linefill _ [] = "\n"
linefill n (x:xs) = iter x xs where
    iter x (y:ys)
        | length x + length y + 1 > n = x ++ "\n" ++ linefill n (y:ys)
        | otherwise                   = iter (x ++ " " ++ y) ys
    iter x [] = x ++ "\n"

getSpewSize :: [String] -> Int
getSpewSize [] = 10
getSpewSize (x:xs) = read x

main = do
  args <- getArgs
  let spewSize = getSpewSize args
  raw <- readFile "sokal.model"
  let model = feed raw
  gen <- getStdGen
  let ws = evalState (runModel model) gen
  putStr $ linefill 72 $ takeEnough spewSize ws
