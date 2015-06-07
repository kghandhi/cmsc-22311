module Main where

import FRP.Helm
import qualified FRP.Helm.Window as Window
import FRP.Helm.Time
import FRP.Helm.Signal
import FRP.Helm.Keyboard

import Controller
import Tetris
import View

merge :: Signal a -> Signal a -> Signal a
merge s1 s2 =
  let
    tsMerge (t1,v1) (t2,v2)
      | t1 >= t2 = v1
      | otherwise = v2
  in
   tsMerge <~ timestamp s1 ~~ timestamp s2

hackHead :: [Key] -> Key
hackHead [] = TabKey
hackHead ks = head ks

--(Time -> b -> b) -> b -> Signal Time -> singal b
actions :: Signal Action
actions = merge (foldp (\_ _-> TimeAction) (TimeAction) (fps 2.4))
          (lift (\ks -> KeyAction (hackHead ks)) keysDown)

currState :: Signal State
currState = foldp upstate initState actions

main :: IO ()
main = do
  run defaultConfig $ view <~ Window.dimensions ~~ currState
