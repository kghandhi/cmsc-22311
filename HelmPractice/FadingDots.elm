module FadingDots where

import List ((::))
import List
import Time
import Signal ((<~),(~),Signal)
import Signal
import Window
import Mouse
import Text as T
import Graphics.Collage as C
import Color

type alias Click = (Time.Time, (Int,Int))

type alias State = (Time.Time, List Click)

initState = (0, [])

type Update = NewTime Time.Time | NewClick Click

tfade = 2 * Time.second

pruneOld now clicks = case clicks of
  [] -> []
  (t,xy) :: clicks' -> if
    | (now-t) > tfade -> []
    | otherwise       -> (t,xy) :: pruneOld now clicks'

upstate u (_,clicks) = case u of
  NewTime t       -> (t, pruneOld t clicks)
  NewClick (t,xy) -> (t, (t,xy) :: pruneOld t clicks)

setAlpha : Float -> Color.Color -> Color.Color
setAlpha a c =
  let rgb = Color.toRgb c in
  Color.rgba rgb.red rgb.green rgb.blue a

view (w,h) (now,clicks) =
  let
      (fw,fh) = (toFloat w, toFloat h)
      (dx,dy) = (-fw/2, fh/2)
      color a = setAlpha a Color.darkBlue
      rad pct = 20 + 100 * pct
      circ pct = C.filled (color (1-pct)) (C.circle (rad pct))
      dots =
        clicks |> List.map (\(t,(x,y)) ->
          let pct = (now-t) / tfade in
          circ pct |> C.move (toFloat x + dx, toFloat (-y) + dy))
  in
    C.collage w h dots

time : Signal Time.Time
time = Signal.foldp (+) 0 (Time.fps 40)

timestamp : Signal a -> Signal (Time.Time, a)
timestamp sig =
  (,) <~ Signal.sampleOn sig time ~ sig

clicks : Signal Click
clicks = timestamp (Signal.sampleOn Mouse.clicks Mouse.position)

state : Signal State
state = Signal.foldp upstate initState
          (Signal.merge (NewTime  <~ time)
                        (NewClick <~ clicks))

main =
  view <~ Window.dimensions ~ state