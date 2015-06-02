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

-- rotateCCW :: Tetrimino -> Tetrimino
-- rotateCCW t =
--   let
--     ps = extractLocs t
