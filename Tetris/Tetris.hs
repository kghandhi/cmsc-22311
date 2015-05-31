module Tetris where

import FRP.Helm
import qualified FRP.Helm.Keyboard as Keyboard
import System.Random (randomRs, mkStdGen)
import Data.Array (Array, array)

data State = State { board :: Board
                   , score :: Int
                   , level :: Int
                   , gameSt :: GameState
                   , randomBag :: [Tetrimino] --next 5
                   , nTetrises :: Int
                   , songChoice :: String
                   , highScore :: Int
                   , tetriminosOnBoard :: [Tetrimino]
                   }

data GameState = Start | Loading | Active | Over

type Board = Array (Int, Int) Cell

-- Proj is a projection of a tetrimino
data Cell = Wall | Empty | Filled Tetrimino | Proj Tetrimino

-- Should each have an associated color? So it is easy to change the color?
data Tetrimino = I  -- Cyan 0
               | J  -- Blue 1
               | L  -- Orange 2
               | O  -- Yellow 3
               | S  -- Green 4
               | T  -- Purple 5
               | Z  -- Red 6
               deriving (Read, Show, Eq)

-- We should be able to pull a random Tetrimino out of a bag so we need
pickRandomBag seed sz = map ([I, J, L, O, S, T, Z] !!)
                        $ take sz
                        $ randomRs (0,6) (mkStdGen seed)

data Action = KeyAction Keyboard.Key | TimeAction

initBoard :: Board
initBoard = array ((0,0), (12,24))
            $ leftBorder ++ rightBorder ++ bottomBorder ++ inside
  where
    leftBorder = [((0,y), Wall) | y <- [0,24]]
    rightBorder = [((12,y), Wall) | y <- [0,24]]
    --topBorder = [((x,24), Wall) | x <- [1,11]]
    bottomBorder = [((x,0), Wall) | x <- [1,11]]
    inside = [((x,y), Empty) | x <- [1,11], y <- [1,24]]

initState :: State
initState = State { board=initBoard
                  , score=0
                  , level=0
                  , gameSt=Loading
                  , randomBag= pickRandomBag 17 5
                  , nTetrises=0
                  , songChoice=""
                  , highScore=0
                  , tetriminosOnBoard = []
                  }
