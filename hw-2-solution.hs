{-# LANGUAGE OverloadedStrings #-}
import CodeWorld

wall, ground, storage, box :: Picture
wall =    colored grey (solidRectangle 1 1)
ground =  colored yellow     (solidRectangle 1 1)
storage = solidCircle 0.3 & ground
box =     colored brown      (solidRectangle 1 1)

data Tile = Wall | Ground | Storage | Box | Blank deriving (Eq)

drawTile :: Tile -> Picture
drawTile Wall    = wall
drawTile Ground  = ground
drawTile Storage = storage
drawTile Box     = box
drawTile Blank   = blank

pictureOfMaze :: Picture
pictureOfMaze = draw21times (\r -> draw21times (\c -> drawTileAt r c))

draw21times :: (Integer -> Picture) -> Picture
draw21times something = go (-10)
  where
    go :: Integer -> Picture
    go 11 = blank
    go n  = something n & go (n+1)

drawTileAt :: Integer -> Integer -> Picture
drawTileAt r c = translated (fromIntegral r) (fromIntegral c) (drawTile (maze (C r c)))

maze :: Coord -> Tile
maze (C x y)
  | abs x > 4  || abs y > 4  = Blank
  | abs x == 4 || abs y == 4 = Wall
  | x ==  2 && y <= 0        = Wall
  | x ==  3 && y <= 0        = Storage
  | x >= -2 && y == 0        = Box
  | otherwise                = Ground

data Direction = R | U | L | D

data Coord = C Integer Integer

data PlayerState = PlayerState Coord Direction

initialState :: PlayerState
initialState = PlayerState (C 0 1) U

origin :: Coord
origin = C 0 0

atCoord :: Coord -> Picture -> Picture
atCoord (C x y) pic = translated (fromIntegral x) (fromIntegral y) pic

adjacentCoord :: Direction -> Coord -> Coord
adjacentCoord R (C x y) = C (x+1) y
adjacentCoord U (C x y) = C  x   (y+1)
adjacentCoord L (C x y) = C (x-1) y
adjacentCoord D (C x y) = C  x   (y-1)

handleTime :: Double -> PlayerState -> PlayerState
handleTime _ c = c

moveIfValid :: Coord -> Coord -> Coord
moveIfValid a b = if maze b == Ground || maze b == Storage then b else a

handleEvent :: Event -> PlayerState -> PlayerState
handleEvent (KeyPress key) ps@(PlayerState c d)
    | key == "Right" = (PlayerState (moveIfValid c (adjacentCoord R c)) R)
    | key == "Up"    = (PlayerState (moveIfValid c (adjacentCoord U c)) U)
    | key == "Left"  = (PlayerState (moveIfValid c (adjacentCoord L c)) L)
    | key == "Down"  = (PlayerState (moveIfValid c (adjacentCoord D c)) D)
    | otherwise      = ps
handleEvent _ c      = c


drawState :: PlayerState -> Picture
drawState (PlayerState c d) = atCoord c (player2 d) & atCoord origin pictureOfMaze

player :: Picture
player = colored red (solidCircle 0.2) & polyline [(0,0), (0.5,0)]

player2 :: Direction -> Picture
player2 U = colored red (solidCircle 0.2) & polyline [(0,0), (0,0.5)]
player2 D = colored red (solidCircle 0.2) & polyline [(0,0), (0,-0.5)]
player2 L = colored red (solidCircle 0.2) & polyline [(0,0), (-0.5,0)]
player2 R = colored red (solidCircle 0.2) & polyline [(0,0), (0.5,0)]


handleResetEvent :: Event -> PlayerState -> PlayerState
handleResetEvent (KeyPress key) _
  | key == "Esc" = initialState
handleResetEvent e ps = handleEvent e ps

resetableInteractionOf initialState handleTime handleEvent drawState =
  interactionOf initialState handleTime handleResetEvent drawState

exercise1 :: IO ()
exercise1 = resetableInteractionOf initialState handleTime handleEvent drawState

main :: IO ()
main = exercise1
