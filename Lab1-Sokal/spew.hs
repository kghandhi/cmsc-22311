import Data.Array.ST
import Control.Monad.ST
import System.IO

type FastModel = Array Int (String, [(Int, Int)])


main = do
  src <- readFile "sokal.model"
