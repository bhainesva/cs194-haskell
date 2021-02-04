{-# OPTIONS_GHC -fno-warn-warnings-deprecations -fno-warn-unused-binds #-}
import CodeWorld

main :: IO ()
main = exercise2

-- Fill in the blanks! (When I say blanks, I mean undefineds)

-- Exercise 1

botCircle, topCircle :: Color -> Picture
botCircle c = colored c (translated 0 (-2.5) (solidCircle 1))
middleCircle c = colored c (translated 0   0  (solidCircle 1))
topCircle c = colored c (translated 0   2.5  (solidCircle 1))

frame :: Picture
frame = rectangle 2.5 8.5

data Phase = Red | Green | Yellow | RedYellow

trafficLight :: Phase -> Picture
trafficLight Red  = botCircle black & middleCircle black & topCircle red & frame
trafficLight Green = botCircle green & middleCircle black & topCircle black   & frame
trafficLight Yellow = botCircle black & middleCircle yellow & topCircle black   & frame
trafficLight RedYellow = botCircle black & middleCircle yellow & topCircle red   & frame


trafficController :: Double -> Picture
trafficController t
  | timeMod <= 3  = trafficLight Green
  | timeMod <= 4  = trafficLight Yellow
  | timeMod <= 7  = trafficLight Red
  | timeMod <= 10 = trafficLight RedYellow
  | otherwise    = trafficLight Green
  where timeMod = round (t*3) `mod` 10

trafficLightAnimation :: Double -> Picture
trafficLightAnimation = trafficController

exercise1 :: IO ()
exercise1 = animationOf trafficLightAnimation

-- Exercise 2
blossom :: Double -> Picture
blossom n = colored yellow (solidCircle (max 0.5 (n / 20)))

tree :: Integer -> Picture
tree 0 = blank
tree n = path [(0,0),(0,1)] & translated 0 1 (
  rotated (pi/10) (tree (n-1)) & rotated (- pi/10) (tree (n-1)))

treeWithBlossom :: Picture -> Double -> Picture
treeWithBlossom blossom 0 = blossom
treeWithBlossom blossom n = path [(0,0),(0,1)] & translated 0 1 (
  rotated (pi/10) (treeWithBlossom blossom (n-1)) & rotated (- pi/10) (treeWithBlossom blossom (n-1)))

simpleAnimatedBlossom :: Double -> Picture
simpleAnimatedBlossom n = colored yellow (solidCircle (min 0.5 (n/20)))

exercise2 :: IO ()
exercise2 = animationOf (\t -> treeWithBlossom (simpleAnimatedBlossom t) 8)

-- Exercise 3

wall, ground, storage, box :: Picture
wall =    colored gray (solidRectangle 1 1)
ground =  colored yellow (solidRectangle 1 1)
storage = solidCircle 0.2 & colored yellow (solidRectangle 1 1)
box =     colored brown (solidRectangle 1 1)

drawTile :: Integer -> Picture
drawTile 1 = wall
drawTile 2 = ground
drawTile 3 = storage
drawTile 4 = box
drawTile _ = blank

pictureOfMaze :: Picture
pictureOfMaze = foldl (&) blank [picture | x <- [-10..10], y <- [-10..10], let picture = translated (fromIntegral x) (fromIntegral y) (drawTile (maze x y))]

exercise3 :: IO ()
exercise3 = drawingOf pictureOfMaze

maze :: Integer -> Integer -> Integer
maze x y
  | abs x > 4  || abs y > 4  = 0
  | abs x == 4 || abs y == 4 = 1
  | x ==  2 && y <= 0        = 1
  | x ==  3 && y <= 0        = 3
  | x >= -2 && y == 0        = 4
  | otherwise                = 2
