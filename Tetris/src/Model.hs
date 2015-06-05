module Model where

import FRP.Helm
import qualified FRP.Helm.Graphics as G
import qualified FRP.Helm.Color as C


import Tetris
import Controller

orange :: C.Color
orange = C.rgb 255 165 0

background :: Double -> Double -> G.Form
background w h =
  G.filled C.black $ G.rect w h

blockOfI :: Double -> G.Form
blockOfI r = G.filled C.cyan $ G.square r

blockOfJ :: Double -> G.Form
blockOfJ r = G.filled C.blue $ G.square r

blockOfL :: Double -> G.Form
blockOfL r = G.filled orange $ G.square r

blockOfO :: Double -> G.Form
blockOfO r = G.filled C.yellow $ G.square r

blockOfS :: Double -> G.Form
blockOfS r = G.filled C.green $ G.square r

blockOfT :: Double -> G.Form
blockOfT r = G.filled C.purple $ G.square r

blockOfZ :: Double -> G.Form
blockOfZ r = G.filled C.red $ G.square r

empty :: Double -> G.Form
empty r = G.filled C.black $ G.square r

wall :: Double -> G.Form
wall r = G.filled C.magenta $ G.square r
