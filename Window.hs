module Window (
   Window,
   Width,
   Height,
   window,
   middle,
   inBounds,
   dropOffBounds,
   randomEdgePosition
) where

import Position
import System.Random

type Window = (Width, Height)
type Width = Int
type Height = Int

window :: Width -> Height -> Window
window w h = (w, h)

middle :: Window -> Pos
middle (w, h) = posXY (w `div` 2) (h `div` 2)

inBounds :: Positioned a => Window -> a -> Bool
inBounds (w, h) pos = within 1 w (getX pos) && within 1 h (getY pos)

dropOffBounds :: Positioned a => Window -> [a] -> [a]
dropOffBounds w = filter (inBounds w)

within :: Int -> Int -> Int -> Bool
within a b x    = a <= x && x <= b

randomEdgePosition :: Window -> IO Pos
randomEdgePosition (w, h) = do
   x <- randomRIO (1, w) :: IO Int
   y <- randomRIO (1, h) :: IO Int
   side <- randomRIO (0, 3) :: IO Int
   case side of
      0 -> return $ posXY x 1
      1 -> return $ posXY w y
      2 -> return $ posXY x h
      3 -> return $ posXY 1 y
