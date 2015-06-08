module View where

import qualified Control.Lens as Lens
import Data.Array
import qualified FRP.Helm.Graphics as G
import qualified FRP.Helm.Text as T
import qualified FRP.Helm.Color as C

import Tetris
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
      Z _ _ -> blockOfZ bSide
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
    buildRow y = (foldl (\acc x ->
                          (G.move (toNum (x * bSide), toNum ((20-y) * bSide))
                           $ cellToForm (bd ! (x,y)) (toNum bSide)):acc) [] [0..11])
  in
   G.group $ foldl (\acc y -> (buildRow y) ++ acc) [] [0..20]

gameOverView :: (Int, Int) -> G.Element
gameOverView (w, h) = G.centeredCollage w h [background (toNum w) (toNum h)
                                         , title
                                         , instructions]
  where
    bSide = h `div` 25
    shift = toNum bSide
    title = makeTitle shift "Game Over"
    instructions = G.move (8*shift, 8*shift)
                   $ formatText (shift/2) "To start a new game press R"

pauseView :: (Int, Int) -> G.Element
pauseView (w, h) =
  G.centeredCollage w h [background (toNum w) (toNum h), title, instructions]
  where
    bSide = h `div` 25
    shift = toNum bSide
    title = makeTitle shift "Welcome to Tetris"
    mv = formatText (shift/2) "To move left, right, or down use arrow keys"
    upArr = formatText (shift/2) "To rotate the Tetrimino use the up arrow"
    sp = formatText (shift/2) "To drop hard, press space"
    rkey = formatText (shift / 2) "To start or restart the game use R"
    pkey = formatText (shift/2) "To find instructions and pause game use P"
    xkey = formatText (shift/2) "To hold use X"
    instructions = G.move (8*shift, 8*shift)
                   $ G.group [ mv
                             , G.moveY (2+shift) upArr
                             , G.moveY (2*(shift + 2)) sp
                             , G.moveY (3*(shift+2)) rkey
                             , G.moveY (4*(shift + 2)) pkey
                             , G.moveY (5*(shift+2)) xkey]

gameView :: (Int, Int) -> State -> G.Element
gameView (w,h) st =
  let
    bSide = h `div` 25
    shift = toNum bSide
    title = makeTitle shift "Tetris"
    scr = (formatText (shift/2)) $ "Score: " ++ (show $ Lens.view score st)
    lvl = (formatText (shift/2)) $ "Level: " ++ (show $ Lens.view level st)
    highScr = (formatText (shift/2))
              $ "High Score: " ++ (show $ Lens.view highScore st)
    stats = G.group [scr, G.moveY (2+shift) lvl, G.moveY (2*shift+4) highScr]
    currScore = (G.move (16*shift, 14*shift)) stats
    -- Maybe do some magic about where to start building
    currBoard = buildBoard (Lens.view board st) bSide
    toDisplay = G.move (-6*shift, (-9)*shift)
                $ G.group [background (toNum w) (toNum h)
                          , currScore
                          , title
                          , currBoard]
  in
   G.centeredCollage w h [toDisplay]

-- Block height is maybe the size of the window divided by 25?
view :: (Int, Int) -> State -> G.Element
view (w,h) st =
  case (Lens.view gameSt st) of
   Active -> gameView (w,h) st
   Over -> gameOverView (w,h)
   _ -> pauseView (w,h)
