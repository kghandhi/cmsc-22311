module Controller where

import Tetris

import FRP.Helm
import qualified FRP.Helm.Keyboard as Keyboard

data Action = KeyAction Keyboard.Key | TimeAction

data Move = Left | Right | Down | Rotate

-- clearRows :: State -> State
-- clearRows st =
--   let
--     b = board st
--     canClear i = all (\j -> (b ! (j, i)) == Empty) [1..10]
--     rowsCleared = map canClear [1..20]


-- -- Clear rows that can be cleared, drop piece that is falling if it can drop more
-- tick :: State -> State
-- tick st = map canClear [1..10]



keyPress :: Keyboard.Key -> State -> State
keyPress key st =
  case key of
   SpaceKey -> st -- Drop Hard
   RightKey -> st -- Move right
   LeftKey -> st -- Move left
   DownKey -> st -- Move down
   UpKey -> st -- rotate clockwise
   XKey -> st
-- if length (holding st) == 0 then holding st = [(fst (falling st))] -- holdkey
     -- RKey -> -- restart game
     -- MKey -> -- toggle music
     -- PKey -> -- Pause
     _ -> st



upstate :: Action -> State -> State
upstate action oldSt =
  case action of
   KeyAction key -> keyPress key oldSt
   TimeAction -> oldSt
