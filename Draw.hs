module Draw (
   Drawable(..),
   Draw,
   static,
   Type(..),
   drawFromType
) where

import Position
import Direction (Dir(..))

class Positioned a => Drawable a where
   symbol :: a -> Char
   draw :: a -> IO ()

   draw o = do
      setPosition o
      putChar (symbol o)

type Draw = Dir -> Char -- Function that draws the "object" based on direction

static :: Char -> Draw
static c = \_ -> c

data Type = Player | Projectile | Enemy deriving (Eq, Show)

drawFromType :: Type -> Draw
drawFromType Player     = playerDraw
drawFromType Projectile = static projChar
drawFromType Enemy      = static enemyChar

playerDraw :: Dir -> Char
playerDraw N  = '\x25B2'
playerDraw S  = '\x25BC'
playerDraw E  = '\x25B6'
playerDraw W  = '\x25C0'
playerDraw NE = '\x25E5'
playerDraw NW = '\x25E4'
playerDraw SE = '\x25E2'
playerDraw SW = '\x25E3'

projChar :: Char
projChar = '\x25CF'

enemyChar :: Char
enemyChar = 'o'

