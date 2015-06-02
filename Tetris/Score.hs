module Score (scores) where

import Data.Array

type Level = Int

-- | Look up the score at (l, n) where l is the level and n is the number
-- of rows cleared.
-- This array is only 100X4 because Arrays must be finite (so I cap the number
-- of levels at 100) and by the tetris guidelines scores are only definied for
-- up to 4 lines cleared at once. We will award the same number of points for
-- n >= 4 lines on any level.

scores :: Array (Int, Int) Int
scores = array ((1,1), (100, 4)) $ 1line ++ 2line ++ 3line ++ 4line
  where
    1line = [((l,1), 40*(l+1)) | l <- [1..100]]
    2line = [((l,2), 100*(l+1)) | l <- [1..100]]
    3line = [((l,3), 300*(l+1)) | l <- [1..100]]
    4line = [((l,4), 1200*(l+1)) | l <- [1..100]]
