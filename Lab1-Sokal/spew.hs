import System.IO
import Data.Array

type FastModel = Array Int (String, [(Int, Int)])

main = do
  src <- readFile "sokal.model"
