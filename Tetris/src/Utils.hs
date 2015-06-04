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
    findMaxDist :: [(Location, Location)] -> Int -> Int
    findMaxDist xs m =
      case xs of
        [] ->  m
        (((x,y),(v,w)):pairs) ->
          let
            m' = max (abs (x-v)) (abs (y-w))
          in
           if m' > m then findMaxDist pairs m'
           else findMaxDist pairs m
    len = findMaxDist (zip ps (tail ps)) 0 -- put it in a len by len box
  in
   (toNum len) / 2

toNum :: (Num c, Integral a) => a -> c
toNum = fromInteger . toInteger

normalize :: Float -> [Location] -> [(Float, Float)]
normalize c ps = map (\(x,y) -> ((toNum x)-c, (toNum y)-c)) ps

turnBack :: Float -> [(Float, Float)] -> [Location]
turnBack c fs = map (\(x,y) -> ((floor $ x + c), (floor $ y + c))) fs

-- | Must be a full, 4 box tet
rotate :: Tetrimino -> Tetrimino
rotate t =
  let doForT c ps = turnBack c $ map (\(x,y)->(y,-x)) $ normalize c ps in
  case t of
    I ps -> I (doForT (findCenter ps) ps)
    J ps -> J (doForT (findCenter ps) ps)
    L ps -> L (doForT (findCenter ps) ps)
    O ps -> O (doForT (findCenter ps) ps)
    S ps -> S (doForT (findCenter ps) ps)
    T ps -> T (doForT (findCenter ps) ps)
    Z ps -> Z (doForT (findCenter ps) ps)
    None -> None
