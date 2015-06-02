module Controller where

import Tetris
import Score (scores)
import Utils

import Control.Lens
import Data.Array
import Data.Array.ST
import Data.List (group)
import Data.Maybe (fromJust)
import Control.Monad.ST
import FRP.Helm
import qualified FRP.Helm.Keyboard as Keyboard

data Action = KeyAction Keyboard.Key | TimeAction

data Direction = Left | Right | Down | Rotate


-- -- Clear rows that can be cleared, drop piece that is falling if it can drop more
-- CHECK IF GAME OVERgtuy6
tick :: State -> State
tick st = clearRows . (advanceFalling Down) st

-- we have only landed if there is a barrier directly below us
hasLanded :: [Location] -> Board -> Bool
hasLanded ps b = any (\(x,y) -> isBarrier (x,y-1) ps b) ps

-- ps is the points in a tetrimino
isBarrier :: Location -> [Location] -> Board -> Bool
isBarrier (x, y) ps b =
  case b ! (x, y) of
   Wall -> True
   Filled _ -> not $ (x, y) `elem` ps
   _ -> False

-- Index (starting at 1) of the first true. We start at 0 because we want the
-- last index before true
firstTrue :: [Bool] -> Maybe Int
firstTrue = helper 0
  where
    helper _ [] = Nothing
    helper i (b:bs') = if b then Just i else helper (i+1) bs'

doMove :: [Location] -> Int -> Board -> Direction -> [Location]
doMove ps spd b d =
  let
    (px, py) =
      case dir of
       Left -> (-1, 0)
       Right -> (1, 0)
       Down -> (0, -1)
       _ -> (0, 0)
    mapper ps s = any (\(x,y) -> isBarrier (x,y) ps b)
                  (map (\(x,y) -> (x+px*s, y+py*s)) ps)
    scale = fromJust $ firstTrue map (mapper ps) [1..spd]
    (dx, dy) = (px*scale, py*scale)
  in
   map (\(x,y) -> (x + dx, y + dy)) ps

-- | advanceFalling
-- 1. Advance the falling piece (based on speed). If there isn't a piece
-- falling, put a new one on the board from the randomBag
-- 2. Update the board to account for the move or the addition of a new
-- piece
-- 3. Modify the random bag so that you take off the piece that just went
-- into play
-- 4. If a new one has landed, add it to the list of those on the board
advanceFalling :: State -> Direction -> State
advanceFalling st dir = advanceBag . dropNew . addFallen . showChange

moveFalling :: State -> State
moveFalling st dir = over falling fallingFn
  where
    spd = view speed st
    bd = view board st
    fallingFn f =
      case f of
       I ps -> I (doMove ps spd b dir)
       J ps -> J (doMove ps spd b dir)
       L ps -> L (doMove ps spd b dir)
       O ps -> O (doMove ps spd b dir)
       S ps -> S (doMove ps spd b dir)
       T ps -> T (doMove ps spd b dir)
       Z ps -> Z (doMove ps spd b dir)
       None -> head (view randomBag st) -- Drop a new one, modify randomBag

showChange :: State -> State
showChange st = over board modifyBoard
  where
    modifyBoard bd = freeze mbd
      where
        t = view falling st
        ps = extractLocs t
        mbd = runStArray $ do
          a <- thaw bd
          mapM_ (\xy -> writeArray mbd xy (Filled t)) ps
          return a

addFallen :: State -> State
addFallen st = over tetriminosOnBoard checkAndAdd
  where
    checkAndAdd tot =
      let f = view falling st in
      if hasLanded (extractLocs f) (view board st) then f:tot else tot

dropNew :: State -> State
dropNew st = over falling drop
  where
    b = view board st
    drop f = if hasLanded (extractLocs f) b then None else f

advanceBag :: State -> State
advanceBag st = over randomBag nextTet
  where
    nextTet rb =
      case view falling st of
       None -> tail rb
       _ -> rb

-- | ClearRows
-- 1. Find the largest number of consecutive rows that are full and update
-- the score accordingly
-- 2. Remove all of those full rows (updating the board)
-- 3. Update the high score
clearRows :: State -> State
clearRows st = newHighScore . (updateScore bonus) $ doClear st
  where
    bonus = countRuns (view board st) ps

doClear :: State -> State
doClear st = over board $ clear 1
  where
    clear 20 bd = bd
    clear y bd
      | rowIsFull y bd ps = clear y (deleteRow y bd ps)
      | otherwise = clear (y + 1) bd
    ps = extractLocs $ view falling st

deleteRow :: Int -> Board -> [Location] -> Board
deleteRow y bd ps = freeze mbd
  where
    ignoreFalling a x
      | y + 1 > 20 = writeArray a (x, y) Empty
      | (x, y+1) `elem` ps = writeArray a (x, y) Empty
      | otherwise = writeArray a (x, y) (readArray a (x, y+1))
    mbd = runSTarray $ do
      a <- thaw bd
      mapM_ (ignoreFalling a) [1..10]
      mapM_ (\x -> writeArray a (x, 20) Empty) [1..10]
      return a

rowIsFull :: Int -> Board -> [Location] -> Bool
rowIsFull y bd ps =
  let
    cellIsFull x =
      case bd ! (x,y) of
       Empty -> False
       Filled _ -> not $ (x, y) `elem` ps
       _ -> False
  in
   all cellIsFull [1..10]

-- Finds the maximum number of consecutive rows that are full
countRuns :: Board -> [Location] -> Int
countRuns bd ps = maximum
                  $ map length
                  $ group
                  $ map (\y -> rowIsFull y bd ps) [1..20]

updateScore :: Int -> State -> State
updateScore bonus st = over score lookup
  where
    l = view level st
    n = min 4 bonus
    lookup s
      | n > 0 = s + (scores ! (l, n))
      | otherwise = s

newHighScore :: State -> State
newHighScore st = over highScore check
  where
    curr = view score st
    check high
      | curr > high = curr
      | otherwise = high

-- | keyPress : handles user key press
-- Space -> Drop hard
-- Right/Left/Down Key -> Move one right/left/down
-- Up Key -> Rotate clockwise
-- XKey -> Hold
-- R key -> Restart the game
-- M key -> toggle music
-- P key -> Pause (show menu)
keyPress :: Keyboard.Key -> State -> State
keyPress key st =
  case key of
   SpaceKey -> st -- Drop Hard
   RightKey -> st -- Move right
   LeftKey -> st -- Move left
   DownKey -> st -- Move down
   UpKey -> st -- rotate clockwise
   XKey -> st
-- if length (holding st) == 0 then holding st = [(fst (falling st))] -- holdkey
     -- RKey -> -- restart game
     -- MKey -> -- toggle music
     -- PKey -> -- Pause
     _ -> st



upstate :: Action -> State -> State
upstate action oldSt =
  case action of
   KeyAction key -> keyPress key oldSt
   TimeAction -> tick oldSt
