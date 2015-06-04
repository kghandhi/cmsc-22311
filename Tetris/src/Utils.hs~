module Utils where

import Tetris

-- Since this is often necessary when we don't care about what type of
-- tetrimino we have
extractLocs :: Tetrimino -> [Location]
extractLocs t =
  case t of
   I ps -> ps
   J ps -> ps
   L ps -> ps
   O ps -> ps
   S ps -> ps
   T ps -> ps
   Z ps -> ps
   _ -> []

findCenter :: [Location] -> Float
findCenter ps =
  let
    findMaxDist [] max = max
    findMaxDist (((x,y), (v,w)):pairs) max
      | abs (x-v) > max || abs (y - w) > max = findMaxDistPair pairs (max (abs (x-v)) (abs (y-w)))
      | otherwise = findMaxDistPair pairs max
    len = findMaxDist (zip ps (tail ps)) 0 -- put it in a len by len box
  in
   len / 2

normalize :: Float -> [Location] -> [(Float, Float)]
normalize c ps = map (\(x,y) -> ((toNum x)-c, (toNum y)-c)) ps

turnBack :: Float -> [(Float, Float)] -> [Location]
turnBack c ps = map (\(x,y) -> (x+c, y+c)) ps

-- | Must be a full, 4 box tet
rotate :: Tetrimino -> Tetrimino
rotate t =
  let
    ps = extractLocs t
    c = findCenter ps
  in
   turnBack c $ map (\(x,y) -> (y, -x)) $ normalize c ps
