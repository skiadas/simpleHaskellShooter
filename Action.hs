module Action (
   getPlayerChanges
) where

import Direction (Dir(..), move)
import Object (fire)
import World (World, mapObjects, getPlayer, mapPlayer, endGame, getWindow)
import Window (inBounds)

data Action = Fire | Move Dir | End  deriving (Eq, Show)

getPlayerAction :: IO Action
getPlayerAction = getChar >>= go
  where go 'q' = return End
        go ' ' = return Fire
        go 'w' = return $ Move N
        go 'a' = return $ Move W
        go 's' = return $ Move S
        go 'd' = return $ Move E
        go _   = getPlayerAction

performAction :: Action -> World -> World
performAction Fire w = mapObjects (proj :) w
              where proj = fire (getPlayer w)
performAction End w = endGame w
performAction (Move dir) w = mapPlayer moveIfPossible w
   where moveIfPossible pl = let pl' = pl `move` dir
                             in if inBounds (getWindow w) pl' then pl' else pl

getPlayerChanges :: IO (World -> World)
getPlayerChanges = performAction <$> getPlayerAction
