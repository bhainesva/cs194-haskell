{-# LANGUAGE OverloadedStrings #-}
import CodeWorld

-- Lists

data List a = Empty | Entry a (List a)

mapList :: (a -> b) -> List a -> List b
mapList _ Empty = Empty
mapList f (Entry c cs) = Entry (f c) (mapList f cs)

contains :: List Coord -> Coord -> Bool
contains Empty _ = False
contains (Entry a b) el = eqCoord a el || contains b el

allList :: List Bool -> Bool
allList Empty = True
allList (Entry a as) = a && allList as

combine :: List Picture -> Picture
combine Empty = blank
combine (Entry p ps) = p & combine ps

-- Coordinates


data Coord = C Integer Integer

data Direction = R | U | L | D

eqCoord :: Coord -> Coord -> Bool
eqCoord (C x1 y1) (C x2 y2)  = x1 == x2 && y1 == y2

adjacentCoord :: Direction -> Coord -> Coord
adjacentCoord R (C x y) = C (x+1) y
adjacentCoord U (C x y) = C  x   (y+1)
adjacentCoord L (C x y) = C (x-1) y
adjacentCoord D (C x y) = C  x   (y-1)

moveFromTo :: Coord -> Coord -> Coord -> Coord
moveFromTo from to for
  | eqCoord from for = to
  | otherwise        = for


-- The maze

data Tile = Wall | Ground | Storage | Box | Blank

isBox :: Tile -> Bool
isBox Box = True
isBox _ = False

isStorage :: Tile -> Bool
isStorage Storage = True
isStorage _ = False

isWalkable :: Tile -> Bool
isWalkable Ground = True
isWalkable Storage = True
isWalkable _ = False

maze :: Coord -> Tile
maze (C x y)
  | abs x > 4  || abs y > 4  = Blank
  | abs x == 4 || abs y == 4 = Wall
  | x ==  2 && y <= 0        = Wall
  | x ==  3 && y <= 0        = Storage
  | x >= -2 && y == 0        = Box
  | otherwise                = Ground

noBoxMaze :: Coord -> Tile
noBoxMaze c
  | isBox (maze c) = Ground
  | otherwise = tile
  where tile = maze c

mazeWithBoxes :: List Coord -> Coord -> Tile
mazeWithBoxes boxes c
  | contains boxes c = Box
  | otherwise = noBoxMaze c

-- The state

data State = State Coord Direction (List Coord)


initialBoxes :: List Coord
initialBoxes = go (-10) (-10)
  where
    go 11 11 = Empty
    go x 11 = go (x+1) (-10)
    go x y = case maze (C x y) of
      Box ->  Entry (C x y) (go x (y+1))
      otherwise -> (go x (y+1))

initialState :: State
initialState = State (C 0 1) U initialBoxes

-- Event handling

handleEvent :: Event -> State -> State
handleEvent (KeyPress k) s
  | k == "Up"    = step s U
  | k == "Down"  = step s D
  | k == "Left"  = step s L
  | k == "Right" = step s R
  | otherwise    = s
handleEvent _ s = s

step :: State -> Direction -> State
step s@(State c d1 bs) d2
  | isWon s   = s
  | otherwise = if succeed then
  (State adj d2 (mapList (moveFromTo adj adj2) bs))
  else (State c d2 bs)
  where adj = adjacentCoord d2 c
        adj2 = adjacentCoord d2 adj
        tile = mazeWithBoxes bs adj
        tile2 = mazeWithBoxes bs adj2
        succeed = isWalkable tile || (isBox tile && isWalkable tile2)

isWon :: State -> Bool
isWon (State _ _ bs) = allList (mapList isOnStorage bs)

isOnStorage :: Coord -> Bool
isOnStorage c = isStorage . noBoxMaze $ c

-- Drawing

wall, ground, storage, box :: Picture
wall =    colored grey (solidRectangle 1 1)
ground =  colored yellow     (solidRectangle 1 1)
storage = colored white (solidCircle 0.3) & ground
box =     colored brown      (solidRectangle 1 1)

drawTile :: Tile -> Picture
drawTile Wall    = wall
drawTile Ground  = ground
drawTile Storage = storage
drawTile Box     = box
drawTile Blank   = blank

pictureOfMaze :: Picture
pictureOfMaze = draw21times (\r -> draw21times (\c -> drawTileAt (C r c)))

draw21times :: (Integer -> Picture) -> Picture
draw21times something = go (-10)
  where
    go :: Integer -> Picture
    go 11 = blank
    go n  = something n & go (n+1)

drawTileAt :: Coord -> Picture
drawTileAt c = atCoord c (drawTile (noBoxMaze c))

atCoord :: Coord -> Picture -> Picture
atCoord (C x y) pic = translated (fromIntegral x) (fromIntegral y) pic

player :: Direction -> Picture
player R = translated 0 0.3 cranium
         & path [(0,0),(0.3,0.05)]
         & path [(0,0),(0.3,-0.05)]
         & path [(0,-0.2),(0,0.1)]
         & path [(0,-0.2),(0.1,-0.5)]
         & path [(0,-0.2),(-0.1,-0.5)]
  where cranium = circle 0.18
                & sector (7/6*pi) (1/6*pi) 0.18
player L = scaled (-1) 1 (player R) -- Cunning!
player U = translated 0 0.3 cranium
         & path [(0,0),(0.3,0.05)]
         & path [(0,0),(-0.3,0.05)]
         & path [(0,-0.2),(0,0.1)]
         & path [(0,-0.2),(0.1,-0.5)]
         & path [(0,-0.2),(-0.1,-0.5)]
  where cranium = solidCircle 0.18
player D = translated 0 0.3 cranium
         & path [(0,0),(0.3,-0.05)]
         & path [(0,0),(-0.3,-0.05)]
         & path [(0,-0.2),(0,0.1)]
         & path [(0,-0.2),(0.1,-0.5)]
         & path [(0,-0.2),(-0.1,-0.5)]
  where cranium = circle 0.18
                & translated   0.06  0.08 (solidCircle 0.04)
                & translated (-0.06) 0.08 (solidCircle 0.04)

pictureOfBoxes :: List Coord -> Picture
pictureOfBoxes cs = combine (mapList (\c -> atCoord c (drawTile Box)) cs)

drawState :: State -> Picture
drawState = draw

draw :: State -> Picture
draw s@(State c d bs) = if isWon s then scaled 3 3 (text "You Won!") else atCoord c (player d) & pictureOfBoxes bs & pictureOfMaze

-- The complete interaction

sokoban :: Interaction State
sokoban = Interaction initialState (\_ c -> c) handleEvent drawState

-- The general interaction type

data Interaction world = Interaction
        world
        (Double -> world -> world)
        (Event -> world -> world)
        (world -> Picture)


runInteraction :: Interaction s -> IO ()
runInteraction (Interaction state0 step handle draw)
  = interactionOf state0 step handle draw

-- Resetable interactions

resetable :: Interaction s -> Interaction s
resetable (Interaction state0 step handle draw)
  = Interaction state0 step handle' draw
  where handle' (KeyPress key) _ | key == "Esc" = state0
        handle' e s = handle e s

-- Start screen

startScreen :: Picture
startScreen = scaled 3 3 (text "Sokoban!")

data SSState world = StartScreen | Running world

withStartScreen :: Interaction s  -> Interaction (SSState s)
withStartScreen (Interaction state0 step handle draw)
  = Interaction state0' step' handle' draw'
  where
    state0' = StartScreen

    step' _ StartScreen = StartScreen
    step' t (Running s) = Running (step t s)

    handle' (KeyPress key) StartScreen | key == " " = Running state0
    handle' _              StartScreen              = StartScreen
    handle' e              (Running s)              = Running (handle e s)

    draw' StartScreen = startScreen
    draw' (Running s) = draw s


-- The main function

main :: IO ()
main = runInteraction . resetable . withStartScreen $ sokoban
