module Model where

import qualified FRP.Helm.Graphics as G
import qualified FRP.Helm.Color as C

-- | my colors
myOrange :: C.Color
myOrange = C.rgb 0.804 0.522 0.247

myRed :: C.Color
myRed = C.rgb 0.698 0.1333 0.1333

background :: Double -> Double -> G.Form
background w h =
  G.filled C.black $ G.rect w h

blockOfI :: Double -> G.Form
blockOfI r = G.filled C.cyan $ G.square r

blockOfJ :: Double -> G.Form
blockOfJ r = G.filled C.blue $ G.square r

blockOfL :: Double -> G.Form
blockOfL r = G.filled myOrange $ G.square r

blockOfO :: Double -> G.Form
blockOfO r = G.filled C.yellow $ G.square r

blockOfS :: Double -> G.Form
blockOfS r = G.filled C.green $ G.square r

blockOfT :: Double -> G.Form
blockOfT r = G.filled C.purple $ G.square r

blockOfZ :: Double -> G.Form
blockOfZ r = G.filled myRed $ G.square r

empty :: Double -> G.Form
empty r = G.filled C.black $ G.square r

wall :: Double -> G.Form
wall r = G.filled C.magenta $ G.square r
