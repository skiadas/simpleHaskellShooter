module Direction (
   Dir(..),
   Movable(..),
   Directed(..),
   direction,
   selfMove
) where

import Position

data Dir = N | S | E | W | NE | NW | SE | SW deriving (Eq, Show) -- Direction

-- Returns the best direction in order to go from the first position
-- to the second
direction :: Positioned a => a -> a -> Dir
direction from to =
  let (x1, y1) = getXY from
      (x2, y2) = getXY to
      distanceCompare = abs(x2 - x1) `compare` abs(y2 - y1)
  in go (x2 `compare` x1, y2 `compare` y1, distanceCompare)
    where go (EQ, EQ, _) = E   -- Shouldn't matter
          go (LT, GT, EQ) = SW -- Diagonal cases
          go (LT, LT, EQ) = NW
          go (GT, LT, EQ) = NE
          go (GT, GT, EQ) = SE
          go (_, LT, LT) = N   -- prioritize vertical
          go (_, GT, LT) = S
          go (LT, _, GT) = W   -- prioritize horizontal
          go (GT, _, GT) = E

class Positioned a => Movable a where
  move :: a -> Dir -> a
  move x dir = setPos newPos x
    where newPos = getPos x `move` dir

instance Movable Pos where
  pos `move` dir = mv (getXY pos) dir
    where (x, y) `mv` N  = posXY x (y - 1)
          (x, y) `mv` S  = posXY x (y + 1)
          (x, y) `mv` E  = posXY (x + 1) y
          (x, y) `mv` W  = posXY (x - 1) y
          (x, y) `mv` NE = posXY (x + 1) (y - 1)
          (x, y) `mv` NW = posXY (x - 1) (y - 1)
          (x, y) `mv` SE = posXY (x + 1) (y + 1)
          (x, y) `mv` SW = posXY (x - 1) (y + 1)


class Movable a => Directed a where
  getDir :: a -> Dir
  setDir :: Dir -> a -> a

selfMove :: Directed a => a -> a
selfMove o = o `move` getDir o

