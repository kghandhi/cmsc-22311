module View where

import qualified Control.Lens as Lens
import Data.Array
import qualified FRP.Helm.Graphics as G
import qualified FRP.Helm.Text as T

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
      I _ -> blockOfI bSide
      J _ -> blockOfJ bSide
      L _ -> blockOfL bSide
      O _ -> blockOfO bSide
      S _ -> blockOfS bSide
      T _ -> blockOfT bSide
      Z _ -> blockOfS bSide
      None -> empty bSide
   _ -> empty bSide

buildBoard :: Board -> Int -> [G.Form]
buildBoard bd bSide =
  let
    -- the forms already built and where we are in x and y
    buildRow y = (foldl (\acc x -> (G.move (toNum (x * bSide), toNum (y * bSide))
                                  $ cellToForm (bd ! (x,y)) (toNum bSide)):acc) [] [0..11])
  in
   foldl (\acc y -> (buildRow y) ++ acc) [] [0..20]

-- Block height is maybe the size of the window divided by 25?
view :: (Int, Int) -> State -> G.Element
view (w,h) st =
  let
    bSide = h `div` 25
    intScr = Lens.view score st
    currScore =  G.toForm . T.asText $ "SCORE: " ++ (show intScr)
    -- Maybe do some magic about where to start building
    currBoard = buildBoard (Lens.view board st) bSide
  in
   G.centeredCollage w h (currBoard ++ [
     background (toNum w) (toNum h)
     , currScore
     ])
