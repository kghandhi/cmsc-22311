import Data.Maybe
import Data.Ord
import Data.List hiding (insert, empty, findIndex)
import qualified Data.List as L (map)
import Data.Map hiding (foldl)
import qualified Data.Map as M (map)
import Network.HTTP
import Text.HTML.TagSoup
import Control.Monad
import System.IO

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

makePrim :: [String] -> PrimitiveModel
makePrim txt = specialFold (empty :: PrimitiveModel) txt
  where
    specialFold acc (x:y:[]) = acc
    specialFold acc (x:y:z:rest) =
      if member (x,y) acc then specialFold (adjust (++ [z]) (x,y) acc) (y:z:rest)
      else specialFold (insert (x, y) [z] acc) (y:z:rest)

frequency :: Ord a => [a] -> [(Int, a)]
frequency xs = sortBy (flip $ comparing fst) $
               L.map (\x -> (length x, head x)) (group $ sort xs)

makeFreq :: PrimitiveModel -> Map (String, String) [(Int, String)]
makeFreq prim = M.map frequency prim

makeProcess :: Map (String, String) [(Int, String)] -> ProcessModel
makeProcess freq = fst $ mapAccumWithKey mapper [] freq where
  mapper process (x,y) zs =
    ((y, L.map (\(f,z) -> (f, findIndex (y,z) freq)) zs):process, zs)

-- Apparently theres a reverse happening between making the frequency and making
-- the process model. TODO: fix
suck :: [String] -> ProcessModel
suck txt = reverse $ makeProcess $ makeFreq $ makePrim txt

main = do
  src <- readFile "urls.txt"
  let urls = lines src
  tags <- mapM (fmap parseTags . openURL) urls
  let txt = foldl (++) [] (L.map extractWords tags)
  writeFile "sokal.model" $ unlines (L.map show (suck txt))
