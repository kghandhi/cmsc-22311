import qualified Data.Map as M (map)
import qualified Data.List as L (map, filter, foldl, concatMap)

import Data.Map (Map, mapWithKey, lookupIndex, assocs, empty, insertWith)
import Data.List (drop, zip3, takeWhile, head, group, sortBy, sort)
import Data.Maybe (mapMaybe)
import Data.Ord (comparing)

import System.IO (readFile)
import Network.HTTP
import Text.HTML.TagSoup
import Data.Char (isAscii)
import Control.Monad
import Control.Applicative ((<$>))

type PrimitiveModel = Map (String, String) [String]
type FrequencyModel = Map (String, String) [(Int, String)]
type ProcessModel = [(String, [(Int, Int)])]

tester = words "x y z0 x y z1 x y z0 x y z0 x y z1 x y z2 x y z0"

makeTrips :: [a] -> [(a, a, a)]
makeTrips xs = zip3 xs (drop 1 xs) (drop 2 xs)

makePrim :: [String] -> PrimitiveModel
makePrim = L.foldl addAssoc (empty :: PrimitiveModel) . makeTrips
  where
    addAssoc acc (x, y, z) = insertWith (++) (x, y) [z] acc

frequency :: Ord a => [a] -> [(Int, a)]
frequency xs = sortBy (flip $ comparing fst) $
               L.map (\x -> (length x, head x)) (group $ sort xs)

makeFreq :: PrimitiveModel -> FrequencyModel
makeFreq prim = M.map frequency prim

makeProcess :: FrequencyModel -> ProcessModel
makeProcess freq = L.map chop $ assocs $ mapWithKey relabel freq
  where
    chop ((_,y), zs) = (y, zs)
    relabel (_, y) = mapMaybe $ idxs
      where
        idxs (f, z) = (,) f <$> lookupIndex (y, z) freq

suck :: [String] -> ProcessModel
suck wds = makeProcess $ makeFreq $ makePrim wds

openURL :: String -> IO String
openURL url = simpleHTTP (getRequest url) >>= getResponseBody

getBody :: [Tag String] -> [Tag String]
getBody tags =
  takeWhile (~/= "<div id=back>") $ head $ sections (~== "<div id=body>") tags

clean :: String -> String
clean = L.filter isAscii

extractWords :: [Tag String] -> [String]
extractWords = words . clean . innerText . getBody

main :: IO ()
main = do
  src <- readFile "urls.txt"
  let urls = lines src
  tags <- mapM (fmap parseTags . openURL) urls
  let wds = L.concatMap extractWords tags
  writeFile "sokal.model" $ unlines (L.map show (suck wds))
