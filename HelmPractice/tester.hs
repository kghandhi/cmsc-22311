module Main where

import FRP.Helm
import qualified FRP.Helm.Time as Time
import qualified FRP.Helm.Text as T
import FRP.Helm.Signal ((<~), (~~), Signal)
import qualified FRP.Helm.Signal as Signal
import qualified FRP.Helm.Window as Window
import qualified FRP.Helm.Color as Color
import qualified FRP.Helm.Mouse as Mouse
import qualified FRP.Helm.Graphics as G
import Control.Applicative

import Data.List

type Click = (Time.Time, (Int, Int))
type State = (Time.Time, [Click])

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
upstate u (now, clicks) =
  case u of
    NewTime t -> (t, (pruneOld t clicks))
    NewClick (t, xy) -> (t, (t, xy):(pruneOld t clicks))

setAlpha :: Double -> Color.Color -> Color.Color
setAlpha a (Color r g b _) = Color.rgba r g b a

toNum :: (Num c, Integral a) => a -> c
toNum = fromInteger . toInteger

view :: (Int, Int) -> State -> G.Element
view (w, h) (now, clicks) = G.centeredCollage w h dots
  where
    (fw, fh) = (toNum w, toNum h)
    (dx, dy) = (-fw/2, fh/2)
    color a
      | a <= 1 && a >= 0 = setAlpha a Color.blue
      | otherwise = Color.blue
    rad pct = 20 + 100 *pct
    circ pct = G.filled (color (1-pct))  (G.circle (rad pct))
    dots = map (\(t,(x,y)) ->
                 let pct = (now-t) / tfade in
                 G.move ((toNum x) + dx, (toNum (-y)) + dy) $ G.group [circ pct, G.toForm $ T.asText (x,y)]) clicks
                 --G.move (toNum x + dx, toNum (-y) + dy) $ circ pct)


time :: Signal Time.Time
time = Signal.foldp (+) 0 (Time.fps 40)

clicks :: Signal Click
clicks = Time.timestamp $ Mouse.clicks *> Mouse.position

merge :: Signal a -> Signal a -> Signal a
merge sigL sigR =
  let
    tsMerge (t1,v1) (t2,v2) =
        if t1 >= t2
          then v1
          else v2
  in
    tsMerge <~ Time.timestamp sigL ~~ Time.timestamp sigR

main :: IO ()
main = run defaultConfig $ view <~ Window.dimensions ~~ stepper
  where
    stepper = Signal.foldp upstate initState $
              merge (NewTime <~ time) (NewClick <~ clicks)
