module Main where

import FRP.Helm
import qualified FRP.Helm.Time as Time
import qualified FRP.Helm.Text as T
import FRP.Helm.Signal ((<~), (~), Signal)
import qualified FRP.Helm.Signal as Signal
import qualified FRP.Helm.Window as Window
import qualified FRP.Helm.Color as Color
import qualified FRP.Helm.Mouse as Mouse
import qualified FRP.Helm.Mouse as G

import Data.List

data Click = (Time.Time, (Int, Int))
data State = (Time.Time, [Click])

initState = (0, [])

data Update = NewTime Time.Time | NewClick Click

tfade = 2 * Time.second

pruneOld :: Time.Time -> [Click] -> [Click]
pruneOld now clicks =
  case clicks of
   [] -> []
   (t, xy):clicks' -> if (now-t) > tfade then []
                      else (t,xy):(pruneOld now clicks')

upstate :: Update -> State -> State
upstate u (_, clicks) =
  case u of
   NewTime t -> (t, pruneOld t clicks)
   NewClick (t, xy) -> (t, (t, xy):(pruneOld t clicks))

setAlpha :: Double -> Color.Color -> Color.Color
setAlpha a c = Color.rgba rgb.red rgb.green rgb.blue a
  where rgb = Color.toRgb c

view :: (Int, Int) -> State -> G.Element
view (w, h) (now, clicks) = G.collage w h dots
  where
    (fw, fh) = (fromIntegral w, fromIntegral h)
    (dx, dy) = (-fw/2, fh/2)
    color a = setAlpha a Color.navy
    rad pct = 20 + 100 *pct
    circ pct = G.filled (color (1-pct))  (G.circle (rad pct))
    dots = map (\(t,(x,y)) ->
                 let pct = (now-t) / tfade in
                  C.move (fromIntegral x + dx, fromIntegral (-y) + dy) $ circ pct)
           clicks


time :: Signal Time.Time
time = Signal.foldp (+) 0 (Time.fps 40)

timestamp :: Signal a -> Signal (Time.Time, a)
timestamp sig = (,) <~ seq sig time ~~ sig

clicks :: Signal Click
clicks = timestamp $ seq Mouse.clicks Mouse.position

state :: Signal State
state = Signal.foldp upstate initState (head $ Signal.combine [NewTime <~ time, NewClick <~ clicks])

main = view <~ Window.dimensions ~~ state
