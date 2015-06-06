module Utils where

import Data.Array

import Tetris

-- Since this is often necessary when we don't care about what type of
-- tetrimino we have
extractLocs :: Tetrimino -> [Location]
extractLocs t =
  case t of
   I ps _ -> ps
   J ps _ -> ps
   L ps _ -> ps
   O ps _ -> ps
   S ps _ -> ps
   T ps _ -> ps
   Z ps _ -> ps
   _ -> []

inBoard :: Board -> Location -> Bool
inBoard bd (x,y) = x <= xmax && y <= ymax
  where
    (_, (xmax, ymax)) = bounds bd

extractCenter :: Tetrimino -> Center
extractCenter t =
  case t of
   I _ c -> c
   J _ c -> c
   L _ c -> c
   O _ c -> c
   S _ c -> c
   T _ c -> c
   Z _ c -> c
   None -> (0,0)

toNum :: (Num c, Integral a) => a -> c
toNum = fromInteger . toInteger

normalize :: Center -> [Location] -> [(Double, Double)]
normalize (cx, cy) ps = map (\(x,y) -> ((toNum x)-cx, (toNum y)-cy)) ps

turnBack :: Center -> [(Double, Double)] -> [Location]
turnBack (cx, cy) fs = map (\(x,y) -> ((floor $ x + cx), (floor $ y + cy - 1))) fs
-- maybe this iwll do it

-- | Must be a full, 4 box tet
rotate :: Tetrimino -> Tetrimino
rotate t =
  let doForT c ps = turnBack c $ map (\(x,y)->(y,-x)) $ normalize c ps in
  case t of
    I ps c -> I (doForT c ps) c
    J ps c -> J (doForT c ps) c
    L ps c -> L (doForT c ps) c
    O ps c -> O (doForT c ps) c
    S ps c -> S (doForT c ps) c
    T ps c -> T (doForT c ps) c
    Z ps c -> Z (doForT c ps) c
    None -> None
