module Tetris where

data State = { board :: Board
             , score :: Int
             , level :: Int
             , gameSt :: GameState
             , randomBag :: [Tetrimino]
             , nTetrises :: Int
             , songChoice :: String
             , highScore :: Int
             , tetriminosOnBoard :: [Tetrimino]
             }

data GameState = Start | Loading | Active | Over

--array or map or vector?
data Board = STArray State Int Box

--Not sure if this works.
data Box = Wall | Empty | Tetrimino | Projection

-- Should each have an associated color? So it is easy to change the color?
data Tetrimino = I c -- Cyan
                | J c -- Blue
                | L c -- Orange
                | O c -- Yellow
                | S c -- Green
                | T c -- Purple
                | Z c -- Red


data Action = KeyAction Keyboard.Key | TimeAction

initState :: State
