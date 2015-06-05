module Main where

import FRP.Helm
import qualified FRP.Helm.Window as Window
import FRP.Helm.Time
import FRP.Helm.Signal
import FRP.Helm.Keyboard

import Controller
import View

merge :: Signal a -> Signal a -> Signal a
merge s1 s2 =
  let
    tsMerge (t1,v1) (t2,v2)
      | t1 >= t2 = v1
      | otherwise = v2
  in
   tsMerge <~ timestamp s1 ~~ timestamp s2


(Time -> b -> b) -> b -> Signal Time -> singal b
actions :: Signal Action
actions = merge (lift (\ks -> KeyAction (head ks)) keysDown)
          (foldp (\_ -> TimeAction) (TimeAction) (fps 40))

currState :: Signal State
currState = foldp upstate initState actions

main :: IO ()
main = do
  engine <- startup defaultConfig
  run engine $ view <~ Window.dimensions ~~ currState
