import qualified Data.Array as A
import qualified Data.List as L
import System.IO
import Data.Array (Array, (!))
import System.Environment (getArgs)

type FastModel = Array Int (String, [(Int, Int)])

isTerminator :: String -> Bool
isTerminator s = L.any (== L.last s) ['.', '?', '!']

feed :: String -> FastModel
feed raw = A.listArray (0, length processModel) processModel
  where processModel = map read $ L.lines raw


main = do
  args <- getArgs
  let spewSize = L.head args
  raw <- readFile "sokal.model"
  print $ show $ isTerminator "HIS"
