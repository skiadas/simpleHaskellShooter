module WorldIO (
   actions
) where

import Action (getPlayerChanges)
import World
import Window (randomEdgePosition)
import Object (enemy)
import Direction (direction)
import Position (getPos, posXY, Pos)
import System.Console.ANSI (clearScreen)

import Control.Concurrent.MVar (MVar, modifyMVar_, withMVar)
import Control.Concurrent (threadDelay)

refreshRate    = 20000
projectileRate = 100000
enemyMoveRate  = 500000
enemySpawnRate = 3000000

refreshIO :: World -> IO World
refreshIO w = do
  clearScreen
  drawWorld w
  return w

addEnemy :: World -> IO World
addEnemy world = do
  pos <- randomEdgePosition (getWindow world)
  let dir = direction pos (getPos $ getPlayer world)
  return $ addObject (enemy pos dir) world


-- Actors operate on the world
type Actor a = MVar World -> IO a

-- Repeat action every number of microseconds,
-- until the stopping condition is true
repeatUntil :: Int -> (World -> Bool) -> (World -> IO World) -> Actor ()
repeatUntil rate cond f worldMV = go where
   go = do
      threadDelay rate
      modifyMVar_ worldMV f
      over <- withMVar worldMV (return . cond)
      if over
         then return ()
         else go


refreshScreen :: Actor ()
refreshScreen = repeatUntil refreshRate isOver refreshIO

moveProjectiles :: Actor ()
moveProjectiles = repeatUntil projectileRate isOver (return . updateProjectiles)

moveEnemies :: Actor ()
moveEnemies = repeatUntil enemyMoveRate isOver (return . updateEnemies)

spawnEnemies :: Actor ()
spawnEnemies = repeatUntil enemySpawnRate isOver addEnemy

destroyHitEnemies :: Actor ()
destroyHitEnemies = repeatUntil refreshRate isOver (return . resolveProjHittingTargets)

gameOverIfEnemiesReachPlayer :: Actor ()
gameOverIfEnemiesReachPlayer = repeatUntil refreshRate isOver (return . checkEnemiesOnPlayer)

playerIO :: Actor ()
playerIO worldMV = do
    playerChanges <- getPlayerChanges
    modifyMVar_ worldMV (return . playerChanges)
    over <- withMVar worldMV (return . isOver)
    if over then return () else playerIO worldMV

actions :: [Actor ()]
actions = [
  refreshScreen,
  moveProjectiles,
  moveEnemies,
  spawnEnemies,
  destroyHitEnemies,
  gameOverIfEnemiesReachPlayer,
  playerIO
  ]
