module World (
   World,
   world,
   isOver, endGame,
   drawWorld,
   mapObjects,
   mapPlayer,
   mapProjectiles,
   mapEnemies,
   addObject,
   updateEnemies,
   getPlayer,
   getWindow,
   updateProjectiles,
   resolveProjHittingTargets,
   checkEnemiesOnPlayer
) where

import Control.Monad.State.Lazy
import Window (Window, Width, Height, window, middle, dropOffBounds, inBounds)
import Object(Object, startPlayer, isPlayer, isEnemy, isProjectile)
import Direction (selfMove, move, direction)
import Position (setPosition, posXY, collides)
import Draw (draw)
import Status (Status, start)
import qualified Status

type World = (Window, [Object], Status, Int)

world :: Width -> Height -> World
world w h = (win, [startPlayer (middle win)], start, 0)
      where win = window w h

writeScore :: Int -> IO ()
writeScore sc = do
  setPosition (posXY 0 0)
  putStr ("Score: " ++ show sc)

drawWorld :: World -> IO ()
drawWorld (_, objs, _, sc) = do
  sequence_ (draw <$> objs)
  writeScore sc

getObjects :: World -> [Object]
getObjects (_, objs, _, _) = objs

getPlayer :: World -> Object
getPlayer = head . filter isPlayer . getObjects

getWindow :: World -> Window
getWindow (win, _, _, _) = win

getStatus :: World -> Status
getStatus (_, _, st, _) = st

getScore :: World -> Int
getScore (_, _, _, sc) = sc

isOver :: World -> Bool
isOver = Status.isOver . getStatus

endGame :: World -> World
endGame (w, objs, _, sc) = (w, objs, Status.over, sc)

mapObjects :: ([Object] -> [Object]) -> World -> World
mapObjects f (w,objs, st, sc) = (w, (f objs), st, sc)

mapIf :: (Object -> Bool) -> (Object -> Object) -> World -> World
mapIf cond f = mapObjects (map tr)
  where tr obj | cond obj  = f obj
               | otherwise = obj

mapPlayer :: (Object -> Object) -> World -> World
mapPlayer f = mapIf isPlayer f

mapProjectiles :: (Object -> Object) -> World -> World
mapProjectiles f = mapIf isProjectile f

mapEnemies :: (Object -> Object) -> World -> World
mapEnemies f = mapIf isEnemy f

mapScore :: (Int -> Int) -> World -> World
mapScore f (w, objs, status, sc) = (w, objs, status, f sc)

addObject :: Object -> World -> World
addObject o = mapObjects (o :)

filterObjects :: (Object -> Bool) -> World -> World
filterObjects pr =  mapObjects (filter pr)


keep = inBounds . getWindow

updateEnemies :: World -> World
updateEnemies world = filterObjects (keep world) $ mapEnemies go world
  where pl = getPlayer world
        go enemy = enemy `move` (direction enemy pl)

updateProjectiles :: World -> World
updateProjectiles world = filterObjects (keep world) $ mapProjectiles selfMove world

resolveProjHittingTargets :: World -> World
resolveProjHittingTargets world = (filterObjects toKeep . mapScore addScore) world
    where collisions = getProjEnemyCollisions (getObjects world)
          toBeRemoved = map fst collisions ++ map snd collisions
          toKeep = not . (`elem` toBeRemoved)
          addScore =  (+ (length collisions))

checkEnemiesOnPlayer :: World -> World
checkEnemiesOnPlayer world = if playerIsHit then endGame world else world
    where pl = getPlayer world
          enemies = filter (isEnemy) . getObjects $ world
          playerIsHit = any (`collides` pl) enemies

getProjEnemyCollisions :: [Object] -> [(Object, Object)]
getProjEnemyCollisions os = [(p, e) | p <- os, e <- os,
                                isProjectile p, isEnemy e, p `collides` e]
