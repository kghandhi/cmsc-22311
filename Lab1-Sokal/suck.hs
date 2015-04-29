import qualified Data.Map as M (map)
import qualified Data.List as L (map, filter, foldl, concatMap)

import Data.Map (Map, unionsWith, singleton, mapWithKey, lookupIndex, assocs)
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
makePrim wds = unionsWith (++) [singleton (x,y) [z] | (x,y,z) <- makeTrips wds]

-- complete :: PrimitiveModel -> PrimitiveModel
-- complete prim = unions [singleton (x,y) [] | <-

frequency :: Ord a => [a] -> [(Int, a)]
frequency xs = sortBy (flip $ comparing fst) $
               L.map (\x -> (length x, head x)) (group $ sort xs)

makeFreq :: PrimitiveModel -> FrequencyModel
makeFreq prim = M.map frequency prim

makeProcess :: FrequencyModel -> ProcessModel
makeProcess freq = L.map chop $ assocs $ mapWithKey relabel freq
  where
    chop ((_,y), vs) = (y, vs)
    relabel (_, y) vs =
      let
        idxs (c, z) = flip (,) c <$> lookupIndex (y, z) freq
      in
       mapMaybe idxs vs

suck :: [String] -> ProcessModel
suck wds = makeProcess $ makeFreq $ makePrim wds

openURL :: String -> IO String
openURL url = simpleHTTP (getRequest url) >>= getResponseBody

harvest :: [Tag String] -> [Tag String]
harvest body =
  L.foldl (++) [] (sections (~== "<p>") body)

getBody :: [Tag String] -> [Tag String]
getBody tags =
  takeWhile (~/= "<div id=back>") $ head $ sections (~== "<div id=body>") tags

clean :: [String] -> [String]
clean = L.map (L.filter isAscii)

extractWords :: [Tag String] -> [String]
extractWords tags = clean $ words $ innerText $ harvest $ getBody tags

-- main :: IO ()
-- main = do
--   src <- readFile "urls.txt"
--   let urls = lines src
--   tags <- mapM (fmap parseTags . openURL) urls
--   let wds = L.foldl (++) [] (L.map extractWords tags)
--   writeFile "sokal.model" $ unlines (L.map show (suck wds))


main = do
  src <- readFile "urls.txt"
  let urls = lines src
  tags <- mapM (fmap parseTags . openURL) urls
  let wds = L.concatMap extractWords tags
  --writeFile "sokal.model" $ unlines (L.map show (suck wds))
  putStr $ unlines $ L.map show $ suck wds
