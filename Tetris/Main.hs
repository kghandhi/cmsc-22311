module Main where

import FRP.Helm
import FRP.Helm.Keyboard
import FRP.Helm.Signal
import FRP.Helm.Time
import qualified FRP.Helm.Window as Window

import Controller
import Tetris
import View

-- This merge does not work on all machines. Why, I do not know.
-- merge :: Signal a -> Signal a -> Signal a
-- merge s1 s2 =
--   let
--     tsMerge (t1,v1) (t2,v2)
--       | t1 >= t2 = v1
--       | otherwise = v2
--   in
--    tsMerge <~ timestamp s1 ~~ timestamp s2

-- If there are no key presses then choose the time action, otherwise choose
-- they key action
fakeMerge :: Signal Time -> Signal [Key] -> Signal Action
fakeMerge st sk = lift2 (\t ks -> if ks == [] then TimeAction
                                  else KeyAction (head ks)) st sk

actions :: Signal Action
actions = mergy (fps 1.7) (keysDown)

-- Update state whenever a Signal Action fires
currState :: Signal State
currState = foldp upstate initState actions

-- Map view onto the dimensions of the window and the current state
main :: IO ()
main = do
  run defaultConfig $ view <~ Window.dimensions ~~ currState
