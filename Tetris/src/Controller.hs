module Controller where

import Score (scores)
import Tetris
import Utils

import Control.Lens
import Data.Array
import Data.Array.ST
import Data.List (group)
import qualified FRP.Helm.Keyboard as Key

-- An action can either be a key press or a tick
data Action = KeyAction Key.Key | TimeAction

-- Because the prelude uses Left and Right, and [L,R,D,Rotate] are not
-- informative
data Direction = Lft | Rgt | Down | Rotate

-- | tick is the handler for advancing the state when a TimeAction Action fires
-- 1. Advance the falling piece down according to it's speed (this includes more
--    change to the state than just advancing the piece, see the description of
--    advanceFalling).
-- 2. If the change in there are rows that are now full, clear them (this also
--    involves more change to state).
tick :: State -> State
tick st = clearRows $ advanceFalling st Down

-- Since all tetriminos must be at or above 1, this will never be out of bounds
-- Claim: the falling tetrimino has landed if there is a barrier directly below
-- We ignore the locations on the board that are elements of the falling piece,
-- though, because a tet hasn't landed if for one of it's locations, the spot
-- directly below it is Filled by another cell of that piece
hasLanded :: [Location] -> Board -> Bool
hasLanded ps b = any (\(x,y) -> isBarrier (x,y-1) ps b) ps

-- Checks if a location on the board is a barrier. Ignores the falling tetrimino
isBarrier :: Location -> [Location] -> Board -> Bool
isBarrier (x, y) ps b
  | inBoard b (x,y)=
    case b ! (x, y) of
     Wall -> True
     Filled _ -> not $ (x, y) `elem` ps
     _ -> False
  | otherwise = True

-- Index (starting at 1) of the first True. We start at 0 because we want the
-- last index before true
firstTrue :: [Bool] -> Maybe Int
firstTrue = helper 0
  where
    helper _ [] = Nothing
    helper i (b:bs') = if b then Just i else helper (i+1) bs'

-- Advances a tetrimino's locations (including center) by as far as it can go
-- A tet can move at most its speed time a vector in the direction of its travel
-- To ensure the piece does not fall off the side of the board, we check how
-- many cells it can go (up to the direction vector times speed vector) and call
-- that (dx,dy) which we add to all the tet's locations before returning
doMove :: ([Location], Center) -> Int -> Board -> Direction
          -> ([Location], Center)
doMove (ps, (cx,cy)) spd b dir =
  let
    (px, py) =
      case dir of
       Lft -> (-1, 0)
       Rgt -> (1, 0)
       Down -> (0, -1)
       _ -> (0, 0)
    mapper points s = any (\(x,y) -> isBarrier (x, y) points b)
                      (map (\(x,y) -> (x+px*s, y+py*s)) points)
    mscale = firstTrue $ map (mapper ps) [1..spd]
    scale =
      case mscale of
        Just sc -> sc
        Nothing -> spd -- if its not going to hit a wall at all
    (dx, dy) = (px*scale, py*scale)
  in
   (map (\(x,y) -> (x + dx, y + dy)) ps, (cx + (toNum dx), cy + (toNum dy)))


-- | advanceFalling
-- 1. Advance the falling piece (based on speed). If there isn't a piece
-- falling, put a new one on the board from the randomBag and advance
-- random bag
-- 2. Update the board to account for the move or the addition of a new
-- piece
-- 3. If the falling piece has landed, add it to the list of pieces on the
-- board
-- 4. If the falling piece has landed drop a new one from the random bag
-- 5. If a new tetrimino has been dropped, reset its speed to the normal start
-- speed (1)
advanceFalling :: State -> Direction -> State
advanceFalling st dir = resetSpeed
                        . dropNew
                        . addFallen
                        . (showChangeInFalling ps)
                        $ moveFalling st dir
  where
    ps = extractLocs $ view falling st

-- Does 1.
moveFalling :: State -> Direction -> State
moveFalling st dir
  | (view falling st == None) = set randomBag (tail $ view randomBag st)
                                $ over falling fallingFn st
  | otherwise = over falling fallingFn st
  where
    spd = view speed st
    bd = view board st
    fallingFn f =
      case f of
       I ps c -> let (ps', c') = doMove (ps,c) spd bd dir in I ps' c'
       J ps c -> let (ps', c') = doMove (ps,c) spd bd dir in J ps' c'
       L ps c -> let (ps', c') = doMove (ps,c) spd bd dir in L ps' c'
       O ps c -> let (ps', c') = doMove (ps,c) spd bd dir in O ps' c'
       S ps c -> let (ps', c') = doMove (ps,c) spd bd dir in S ps' c'
       T ps c -> let (ps', c') = doMove (ps,c) spd bd dir in T ps' c'
       Z ps c -> let (ps', c') = doMove (ps,c) spd bd dir in Z ps' c'
       None -> head (view randomBag st) -- Drop a new one, modify randomBag

-- Does 2.
showChangeInFalling :: [Location] -> State -> State
showChangeInFalling oldps st = over board modifyBoard st
  where
    modifyBoard bd = mbd
      where
        t = view falling st
        ps = extractLocs t
        mbd = runSTArray $ do
          a <- thaw bd
          mapM_ (\xy -> if inBoard bd xy
                        then writeArray a xy (Filled t)
                        else return ()) ps
          mapM_ (\xy -> if not $ xy `elem` ps
                        then (if inBoard bd xy
                              then writeArray a xy Empty
                              else return ())
                        else return ()) oldps
          return a

-- Does 3.
addFallen :: State -> State
addFallen st = over landedTets checkAndAdd st
  where
    checkAndAdd tot =
      let f = view falling st in
      if hasLanded (extractLocs f) (view board st) then f:tot else tot

-- Does 4. Notice None is used so that next time we drop a new one since it
-- looks bad to land then drop a new one immediately.
dropNew :: State -> State
dropNew st = over falling dropIt st
  where
    b = view board st
    dropIt f = if hasLanded (extractLocs f) b then None else f

-- Does 5.
resetSpeed :: State -> State
resetSpeed st = over speed ifDropped st
  where
    f = view falling st
    ifDropped s =
      case f of
       None -> 1
       _ -> s

-- | ClearRows
-- 1. Find the largest number of consecutive rows that are full and update
-- the score accordingly
-- 2. Remove all of those full rows (updating the board)
-- 3. Update the high score
clearRows :: State -> State
clearRows st = newHighScore . (updateScore bonus) $ doClear st
  where
    f = view falling st
    ps = extractLocs f
    bonus = countRuns (view board st) ps

-- Clears the rows that are full recursively. If row y is full, delete the row
-- then look at it again until it is no longer full, then look at the next row
-- up.
doClear :: State -> State
doClear st = over board (clear 1) st
  where
    clear 19 bd = bd
    clear y bd
      | rowIsFull y bd ps = clear y (deleteRow y bd ps)
      | otherwise = clear (y + 1) bd
    ps = extractLocs $ view falling st

-- Modify the board. Shift all the rows starting at yStart down one (killing
-- yStart). The top row should bring in only empty squares and that top row
-- wont be a falling piece because falling pieces start at row at most 20.
deleteRow :: Int -> Board -> [Location] -> Board
deleteRow yStart bd ps = mbd
  where
    ignoreFalling a y x
      | not $ inBoard bd (x,y) = return ()
      | y + 1 > 20 = writeArray a (x, y) Empty
      | (x, y+1) `elem` ps = writeArray a (x, y) Empty
      | otherwise = (readArray a (x, y+1)) >>= (\v -> writeArray a (x, y) v)
    mbd = runSTArray $ do
      a <- thaw bd
      mapM_ (\y -> mapM_ (ignoreFalling a y) [1..10]) [yStart..20]
      return a

-- Returns true if the row is full, a cell is not full if it is part of the
-- falling piece.
rowIsFull :: Int -> Board -> [Location] -> Bool
rowIsFull y bd ps =
  let
    cellIsFull x
      | inBoard bd (x,y) =
        case bd ! (x,y) of
         Empty -> False
         Filled _ -> not $ (x, y) `elem` ps
         _ -> False
      | otherwise = False
  in
   all cellIsFull [1..10]

-- Finds the maximum number of consecutive rows that are full
countRuns :: Board -> [Location] -> Int
countRuns bd ps = maximum
                  $ map (\bs -> case bs of
                                 [] -> 0
                                 b:_ -> if b then length bs
                                          else 0)
                  $ group
                  $ map (\y -> rowIsFull y bd ps) [1..20]

-- Based on the lookup array defined in Score
updateScore :: Int -> State -> State
updateScore bonus st = over score lookItUp st
  where
    l = min (view level st) 100
    n = min 4 bonus
    lookItUp s
      | n > 0 = s + (scores ! (l, n))
      | otherwise = s

-- If the new score is higher than the old high.
newHighScore :: State -> State
newHighScore st = over highScore check st
  where
    curr = view score st
    check high
      | curr > high = curr
      | otherwise = high

-- | gameOver checks if the game is over and if so ends the game
gameOver :: State -> State
gameOver st
  | didFail (view board st) (extractLocs $ view falling st) = endGame st
  | otherwise = st

-- Returns true if the game is over
didFail :: Board -> [Location] -> Bool
didFail bd ps = any (\x -> (not $ ((x,20) `elem` ps)) &&
                           Empty /= (bd ! (x,20))) [1..10]

-- Defines the transition to game over. Maintains the old high score.
endGame :: State -> State
endGame st = set randomBag newBag
             . set falling None
             . set highScore oldHigh
             $ set gameSt Over initState
  where
    oldHigh = view highScore st
    newBag = pickRandomBag (oldHigh + (view score st) + 31)

-- | keyPress : handles user key press
-- Space -> Drop hard
-- Right/Left/Down Key -> Move one right/left/down
-- Up Key -> Rotate clockwise
-- XKey -> Hold
-- R key -> Restart the game
-- P key -> Pause (show menu)
keyPress :: Key.Key -> State -> State
keyPress key st =
  case key of
   Key.SpaceKey -> (over speed $ (\_ -> 3)) st
   Key.RightKey -> advanceFalling st Rgt
   Key.LeftKey -> advanceFalling st Lft
   Key.DownKey -> advanceFalling st Down
   Key.UpKey -> doRotation st
   Key.XKey -> doHold st
   Key.RKey -> restartGame st
   Key.PKey -> pauseGame st
   _ -> st

-- X key, holds the falling piece. If there is a piece int he hold alrady
-- it exchanges the two pieces. When we hold a piece we reset it to its
-- initial position, reset the speed and update the board accordingly.
doHold :: State -> State
doHold st = showChangeInFalling (extractLocs f)
            . resetSpeed
            . (set holding hs')
            . (set falling f')
            $ set randomBag rb' st
  where
    hs = view holding st
    f = view falling st
    rb = view randomBag st
    (hs', f', rb') =
      case hs of
       [] -> (putInBag f, head rb, tail rb)
       [held] -> (putInBag f, held, rb)
       xs -> ([head xs], f, rb)

-- Adds a tetrimino to the hold bag and resets its position to its start pos.
putInBag :: Tetrimino -> [Tetrimino]
putInBag t =
  case t of
   I _ _ -> [initI]
   J _ _ -> [initJ]
   L _ _ -> [initL]
   O _ _ -> [initO]
   S _ _ -> [initS]
   T _ _ -> [initT]
   Z _ _ -> [initZ]
   None -> []

-- | If R is pressed restart the game according to what state we are in.
-- Over: just switch to active because the board etc. has already been reset
-- by the game over handler.
-- Start: begin a new game, sitch the gameState to active from initState
-- Otherwise: set the high score to be the old high score, level to be the old
-- level, make a new random bag and add a new falling piece then make the state
-- active.
restartGame :: State -> State
restartGame st =
  case (view gameSt st) of
    Over -> set gameSt Active st
    Start -> set gameSt Active initState
    _ -> set gameSt Active
         . (set randomBag (tail newBag))
         . (set falling (head newBag))
         . (set level lvl)
         $ set highScore hScr initState
  where
    hScr = view highScore st
    lvl = view level st
    newBag = pickRandomBag (lvl + hScr + 17)


-- | If the game is paused make it active, otherwise pause it
pauseGame :: State -> State
pauseGame st =
  case (view gameSt st) of
  Paused -> set gameSt Active st
  _ -> set gameSt Paused st

-- | Up key
-- If the rotation is valid rotate the piece and do something similar to
-- advance falling. Otherwise do nothing
doRotation :: State -> State
doRotation st
  | isValidRotation st = resetSpeed
                         . dropNew
                         . addFallen
                         . (showChangeInFalling (extractLocs $ view falling st))
                         $ makeRotate st
  | otherwise = st

-- If there is no falling, makeRotate will drop a new one, so we want to check
-- if the head of the random bag can rotate. Otherwise just check if the
-- falling tetrimino can rotate about it's center.
isValidRotation :: State -> Bool
isValidRotation st =
  case view falling st of
   None -> checkT (head (view randomBag st)) (view board st)
   f -> checkT f (view board st)

-- Helper for isValidRotation
checkT :: Tetrimino -> Board -> Bool
checkT t bd =
  all (\(x,y) -> (inBoard bd (x,y)) && (not $ isBarrier (x,y) ps bd)) ps'
  where
    ps = extractLocs t
    ps' = extractLocs $ rotate t

-- Using function from utils rotate the piece
makeRotate :: State -> State
makeRotate st
  | (view falling st) == None =
      let rb = view randomBag st in
       over falling rotate
       . set randomBag (tail rb)
       $ set falling (head rb) st
  | otherwise = over falling rotate st

-- When the game is inactive only update the state on key press, otherwise
-- update the state based on time or key action.
upstate :: Action -> State -> State
upstate action oldSt =
  case (action, view gameSt oldSt) of
   (KeyAction key, Active) -> keyPress key $ gameOver oldSt
   (TimeAction, Active) -> tick $ gameOver oldSt
   (KeyAction key, Paused) -> keyPress key $ gameOver oldSt
   (KeyAction key, Start) -> keyPress key $ gameOver oldSt
   (KeyAction key, Over) -> keyPress key $ gameOver oldSt
   _ -> oldSt
