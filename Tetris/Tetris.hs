module Tetris where

import Control.Lens
import FRP.Helm
--import qualified FRP.Helm.Keyboard as Keyboard
import System.Random (randomRs, mkStdGen)
import Data.Array (Array, array)


type Location = (Int, Int)

data State = State { _board :: Board
                   , _falling :: (Tetrimino, Location)
                   , _score :: Int
                   , _level :: Int
                   , _gameSt :: GameState
                   , _randomBag :: [Tetrimino] --next 5
                   , _nTetrises :: Int
                   , _holding :: [Tetrimino] --length at most 1
                   , _songChoice :: String
                   , _highScore :: Int
                   , _tetriminosOnBoard :: [Tetrimino]
                   } deriving Show
makeLenses ''State

data GameState = Start | Loading | Active | Over

type Board = Array (Int, Int) Cell

-- The Ghost piece is the shadow
data Cell = Wall | Empty | Filled Tetrimino | Ghost Tetrimino

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
-- infinite list
pickRandomBag seed = map ([I, J, L, O, S, T, Z] !!)
                     $ randomRs (0,6) (mkStdGen seed)


initBoard :: Board
initBoard = array ((0,0), (11,20))
            $ leftBorder ++ rightBorder ++ bottomBorder ++ inside
  where
    leftBorder = [((0,y), Wall) | y <- [0,20]]
    rightBorder = [((12,y), Wall) | y <- [0,20]]
    bottomBorder = [((x,0), Wall) | x <- [1,10]]
    inside = [((x,y), Empty) | x <- [1,10], y <- [1,20]]

initBag = pickRandomBag 17

dropPiece :: [Tetrimino] -> (Tetrimino, Location)
dropPiece ts =
  case head ts of
   I -> (I, (4, 20))
   J -> (J, (4, 19))
   L -> (L, (4, 19))
   O -> (O, (5, 19))
   S -> (S, (4, 19))
   T -> (T, (4, 19))
   Z -> (Z, (5, 19))

initState :: State
initState = State initBoard (dropPiece $ take 1 initBag) 0 0 Loading
            (drop 1 initBag) 0 [] "" 0 []
