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

upstate :: [Update] -> State -> State
upstate us (now, clicks) =
  case us of
    [] ->  (now, clicks)
    (u:us') ->
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
    color a = setAlpha a Color.white
    rad pct = 20 + 100 *pct
    circ pct = G.filled (color (1-pct))  (G.circle (rad pct))
    dots = map (\(t,(x,y)) ->
                 let pct = (now-t) / tfade in
                  G.move (toNum x + dx, toNum (-y) + dy) $ circ pct)
           clicks

-- view :: (Int, Int) -> State -> G.Element
-- view (w, h) (now, clicks) = G.centeredCollage w h dots
--   where
--     (fw, fh) = (toNum w, toNum h)
--     (dx, dy) = (-fw/2, fh/2)
--     color a = setAlpha a Color.white
--     rad pct = 20 + 100 *pct
--     circ pct = G.filled (color (1-pct))  (G.circle (rad pct))
--     dots = map (\(t,(x,y)) ->
--                  let pct = (now-t) / tfade in
--                   G.move (toNum x + dx, toNum (-y) + dy) $ circ pct)
--            clicks


-- time :: Signal Time.Time
-- time = Signal.foldp (+) 0 (Time.fps 40)

-- timestamp :: Signal a -> Signal (Time.Time, a)
-- timestamp sig = (,) <~ seq sig time ~~ sig

-- clicks :: Signal Click
-- clicks = timestamp $ seq Mouse.clicks Mouse.position

-- state :: Signal State
-- state = Signal.foldp upstate initState (Signal.combine [NewTime <~ time, NewClick <~ clicks])

-- main :: IO ()
-- main = run defaultConfig $ view <~ Window.dimensions ~~ state


time :: Signal Time.Time
time = Signal.foldp (+) 0 (Time.fps 40)

timestamp :: Signal a -> Signal (Time.Time, a)
timestamp sig = (,) <~ seq sig time ~~ sig

clicks :: Signal Click
clicks = timestamp $ seq Mouse.clicks Mouse.position

-- state :: Signal State
-- state = Signal.foldp upstate initState (Signal.combine [NewTime <~ time, NewClick <~ clicks])

main :: IO ()
main = run defaultConfig $ view <~ Window.dimensions ~~ stepper
  where
    stepper = Signal.foldp upstate initState (Signal.combine [NewTime <~ time, NewClick <~ clicks])
