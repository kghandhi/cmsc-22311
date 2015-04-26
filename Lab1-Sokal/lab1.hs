import Data.Maybe
import Data.List
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

-- makePrim txt = fold helper txt Map ("","") []
--   let
--     helper (x:y:z:rest)  = if (x,y) in thing then thing[(x,y)].append(t) else thing[(x,y)] = [t]

-- suck :: [String] -> (PrimitiveModel, ProcessModel) -> (PrimitiveModel, ProcessModel)
-- suck txt (prim, process) =

main = do
  tags <- fmap parseTags $ openURL "http://muse.jhu.edu/journals/postmodern_culture/v024/24.1.mickalites.html"
  print $ extractWords tags
