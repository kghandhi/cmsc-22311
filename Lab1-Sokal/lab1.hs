import Data.Maybe
import Data.List hiding (insert, empty)
import Data.Map hiding (foldl)
import Network.HTTP
import Text.HTML.TagSoup
import Control.Monad

type PrimitiveModel = Map (String, String) [String]
type ProcessModel = [(String, [(Int, Int)])]


openURL :: String -> IO String
openURL url = simpleHTTP (getRequest url) >>= getResponseBody

harvest :: [Tag String] -> [Tag String]
harvest body =
  foldl (++) [] (sections (~== "<p>") body)

getBody :: [Tag String] -> [Tag String]
getBody tags =
  takeWhile (~/= "<div id=back>") $ head $ sections (~== "<div id=body>") tags

extractWords :: [Tag String] -> [String]
extractWords tags = words $ innerText $ harvest $ getBody tags

makePairs :: [String] -> [(String, String)]
makePairs txt = zip txt (drop 1 txt)

makePrim :: [String] -> PrimitiveModel
makePrim txt = foldl helper (empty :: PrimitiveModel) txt
  where
    helper acc (x:y:z:rest) = if member (x,y) acc then adjust (++ [z]) (x,y) acc
                              else insert (x, y) [z] acc

-- makeProcess :: PrimitiveModel -> ProcessModel
-- makeProcess prim =
--   let
--     countFreq ss = sort ss

-- suck :: [String] -> (PrimitiveModel, ProcessModel) -> (PrimitiveModel, ProcessModel)
-- suck txt (prim, process) =

main = do
  tags <- fmap parseTags $ openURL "http://muse.jhu.edu/journals/postmodern_culture/v024/24.1.mickalites.html"
  print $ extractWords tags
