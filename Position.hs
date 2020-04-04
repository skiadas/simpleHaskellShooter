module Position (
   Pos,
   posXY,
   Positioned(..),
   setPosition,
   collides
) where

import System.Console.ANSI (setCursorPosition)

data Pos = P Int Int   deriving (Eq, Show)   -- Position

posXY :: Int -> Int -> Pos
posXY = P

class Positioned a where
  getPos :: a -> Pos
  getX :: a -> Int
  getY :: a -> Int
  getXY :: a -> (Int, Int)
  setPos :: Pos -> a -> a

  getXY p = (x, y) where P x y = getPos p
  getX = fst . getXY
  getY = snd . getXY


instance Positioned Pos where
  getPos = id
  setPos p _ = p

setPosition :: Positioned a => a -> IO ()
setPosition o = setCursorPosition y x
    where P x y = getPos o

collides :: Positioned a => a -> a -> Bool
collides x y = getPos x == getPos y
