module Utils where

import Data.Array
import Data.Array.ST (getBounds, writeArray, readArray)

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

inBoard :: Board -> Location -> Bool
inBoard bd (x,y) = x <= xmax && y <= ymax
  where
    (_, (xmax, ymax)) = bounds bd

findCenter :: [Location] -> Float
findCenter ps =
  let
    xs = map (\(x,_) -> x) ps
    ys = map (\(_,y) -> y) ps
    maxx = maximum xs
    maxy = maximum ys
    minx = minimum xs
    miny = minimum ys
    totalMx = 1 + max (maxx - minx) (maxy - miny)
  in
   (toNum totalMx) / 2

toNum :: (Num c, Integral a) => a -> c
toNum = fromInteger . toInteger

normalize :: Float -> [Location] -> [(Float, Float)]
normalize c ps = map (\(x,y) -> ((toNum x)-c, (toNum y)-c)) ps

turnBack :: Float -> [(Float, Float)] -> [Location]
turnBack c fs = map (\(x,y) -> ((floor $ x + c), (floor $ y + c - 1))) fs
-- maybe this iwll do it

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
