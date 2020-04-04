import Control.Concurrent.MVar (newMVar)
import Control.Concurrent (forkIO)
import WorldIO (actions)
import World (World, world)

import System.Console.ANSI (clearScreen, showCursor, hideCursor, getTerminalSize)
import System.IO (stdin, stdout, hSetEcho, BufferMode(..), hSetBuffering)

setup :: IO ()
setup = do
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  hSetEcho stdin False
  hideCursor
  clearScreen

teardown :: IO ()
teardown = do
   showCursor
   clearScreen

makeWorld :: IO World
makeWorld = do
  size <- getTerminalSize
  case size of
    Nothing -> makeWorld
    Just (h, w) -> return $ world (w - 1) (h - 1)

main :: IO ()
main = do
  setup
  world <- makeWorld
  worldMV <- newMVar world
  forkAll (fmap ($ worldMV) actions)
  teardown

forkAll :: [IO ()] -> IO ()
forkAll [io] = io
forkAll (io:ios) = forkIO io >> forkAll ios
