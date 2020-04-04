module Object (
   Object, object,
   player, startPlayer,
   enemy,
   projectile, fire,
   isPlayer, isEnemy, isProjectile
) where

import Position
import Direction
import Draw

data Object = Obj Type Pos Dir deriving (Eq, Show)

instance Positioned Object where
  getPos (Obj _ pos _) = pos
  setPos pos (Obj ty _ dir) = Obj ty pos dir

instance Movable Object where
  Obj ty pos _ `move` dir = Obj ty (pos `move` dir) dir

instance Directed Object where
  getDir (Obj _ _ dir) = dir
  setDir dir (Obj ty pos _) = Obj ty pos dir

instance Drawable Object where
   symbol (Obj ty _ dir) = drawFromType ty dir

object :: Type -> Pos -> Dir -> Object
object = Obj

player :: Pos -> Dir -> Object
player = object Player

startPlayer :: Pos -> Object
startPlayer = (`player` N)

projectile :: Pos -> Dir -> Object
projectile = object Projectile

fire :: Directed a => a -> Object
fire obj = projectile (getPos obj) (getDir obj)

enemy :: Pos -> Dir -> Object
enemy = object Enemy

getType :: Object -> Type
getType (Obj ty _ _) = ty

isPlayer :: Object -> Bool
isPlayer = (== Player) . getType

isEnemy :: Object -> Bool
isEnemy = (== Enemy) . getType

isProjectile :: Object -> Bool
isProjectile = (== Projectile) . getType
