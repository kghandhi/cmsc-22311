module View where

import qualified Control.Lens as Lens
import Data.Array
import qualified FRP.Helm.Graphics as G
import qualified FRP.Helm.Text as T
import qualified FRP.Helm.Color as C

import Tetris
--import Controller
import Model
import Utils

cellToForm :: Cell -> Double -> G.Form
cellToForm c bSide =
  case c of
   Empty -> empty bSide
   Wall -> wall bSide
   Filled t ->
     case t of
      I _ _ -> blockOfI bSide
      J _ _ -> blockOfJ bSide
      L _ _ -> blockOfL bSide
      O _ _ -> blockOfO bSide
      S _ _ -> blockOfS bSide
      T _ _ -> blockOfT bSide
      Z _ _ -> blockOfS bSide
      None -> empty bSide
   _ -> empty bSide

formatText :: Double -> String -> G.Form
formatText shift = G.toForm . T.text . (T.height shift) . (T.color C.white) . T.toText

makeTitle :: Double -> String -> G.Form
makeTitle shift = G.move (6*shift, -2*shift) . G.toForm . T.text . T.header
                  . (T.color C.white) . T.toText

buildBoard :: Board -> Int -> G.Form
buildBoard bd bSide =
  let
    -- the forms already built and where we are in x and y
    buildRow y = (foldl (\acc x -> (G.move (toNum ((12-x) * bSide), toNum ((20-y) * bSide))
                                  $ cellToForm (bd ! (x,y)) (toNum bSide)):acc) [] [0..11])
  in
   G.group $ foldl (\acc y -> (buildRow y) ++ acc) [] [0..20]

-- Block height is maybe the size of the window divided by 25?
view :: (Int, Int) -> State -> G.Element
view (w,h) st =
  let
    bSide = h `div` 25
    shift = toNum bSide
    title = makeTitle shift "Hetris"
    scr = (formatText (shift/2)) $ "Score: " ++ (show $ Lens.view score st)
    lvl = (formatText (shift/2)) $ "Level: " ++ (show $ Lens.view level st)
    highScr = (formatText (shift/2)) $ "High Score: " ++ (show $ Lens.view highScore st)
    stats = G.group [scr, G.moveY (2+shift) lvl, G.moveY (2*shift+4) highScr]
    currScore = (G.move (15*shift, 10*shift)) stats
    -- Maybe do some magic about where to start building
    currBoard = buildBoard (Lens.view board st) bSide
    toDisplay = G.move (-6*shift, (-9)*shift)
                $ G.group [
      background (toNum w) (toNum h)
      , currScore
      , title
      , currBoard]
  in
   G.centeredCollage w h [toDisplay]
