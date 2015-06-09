{-# LANGUAGE RankNTypes, TemplateHaskell, FlexibleContexts #-}
module Tetris where

import Control.Lens
import Data.Array (Array, array)
import Data.List (sort)
import System.Random (randomRs, mkStdGen)

type Location = (Int, Int)
type Center = (Double, Double)

data GameState = Start | Paused | Active | Over deriving (Eq, Show)

-- But well modify it using freeze and thaw
type Board = Array Location Cell

-- The Ghost piece is the shadow, is not implemented (next version)
data Cell = Wall | Empty | Filled Tetrimino | Ghost Tetrimino
          deriving (Show, Eq)

data Tetrimino = I [Location] Center -- Cyan 0
               | J [Location] Center -- Blue 1
               | L [Location] Center -- Orange 2
               | O [Location] Center -- Yellow 3
               | S [Location] Center -- Green 4
               | T [Location] Center -- Purple 5
               | Z [Location] Center -- Red 6
               | None
               deriving (Read, Show)

-- Two tetriminos are the same if they occupy the same locations and
-- are the same shape. For testing purposes don't care about the center
instance Eq Tetrimino where
  t1 == t2 =
    case (t1,t2) of
     (I ps1 _, I ps2 _) -> (sort ps1) == (sort ps2)
     (J ps1 _, J ps2 _) -> (sort ps1) == (sort ps2)
     (L ps1 _, L ps2 _) -> (sort ps1) == (sort ps2)
     (O ps1 _, O ps2 _) -> (sort ps1) == (sort ps2)
     (S ps1 _, S ps2 _) -> (sort ps1) == (sort ps2)
     (T ps1 _, T ps2 _) -> (sort ps1) == (sort ps2)
     (Z ps1 _, Z ps2 _) -> (sort ps1) == (sort ps2)
     (None, None) -> True
     (_, _) -> False

data State = State {
    _board :: Board
  , _falling :: Tetrimino
  , _speed :: Int
  , _score :: Int
  , _level :: Int
  , _gameSt :: GameState
  , _randomBag :: [Tetrimino]
  , _holding :: [Tetrimino] --length at most 1
  , _highScore :: Int
  , _landedTets :: [Tetrimino]
  } deriving Show
makeLenses ''State

-- These are where each tetrimino starts out when it is dropped
initI :: Tetrimino
initI = I [(4,20), (5,20), (6,20), (7,20)] (6,21)

initJ :: Tetrimino
initJ = J [(4,20), (4,19), (5,19), (6,19)] (5.5, 19.5)

initL :: Tetrimino
initL = L [(4,19), (5,19), (6,19), (6,20)] (5.5, 19.5)

initO :: Tetrimino
initO = O [(5,20), (6,20), (5,19), (6,19)] (6, 20)

initS :: Tetrimino
initS = S [(4,19), (5,19), (5,20), (6,20)] (5.5, 19.5)

initT :: Tetrimino
initT = T [(4,19), (5,19), (5,20), (6,19)] (5.5, 19.5)

initZ :: Tetrimino
initZ = Z [(4,20), (5,20), (5,19), (6,19)] (5.5, 19.5)

-- We should be able to pull a random Tetrimino out of a bag so we need
-- infinite list
pickRandomBag :: Int -> [Tetrimino]
pickRandomBag seed = map (tets !!)
                     $ randomRs (0,6) (mkStdGen seed)
  where
    tets = [ initI, initJ, initL, initO, initS, initT, initZ ]

-- Start with a blank board
initBoard :: Board
initBoard = array ((0,0), (11,20))
            $ leftBorder ++ rightBorder ++ bottomBorder ++ mid
  where
    leftBorder = [((0,y), Wall) | y <- [0..20]]
    rightBorder = [((11,y), Wall) | y <- [0..20]]
    bottomBorder = [((x,0), Wall) | x <- [1..10]]
    mid = [((x,y), Empty) | x <- [1..10], y <- [1..20]]

-- In a better game I would make this random each time so that the first
-- game was not always the same...
initBag :: [Tetrimino]
initBag = pickRandomBag 17

-- I want to be able to pick from the bag and if for some reason the bag
-- were empty, choose a new bag
safeHead :: [Tetrimino] -> Tetrimino
safeHead [] = head $ pickRandomBag 19
safeHead ls = head ls

initState :: State
initState = State initBoard (safeHead initBag) 1 0 1 Start
            (drop 1 initBag) [] 0 []
