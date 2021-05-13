module Main where

import Bot
import CLI
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad.Reader
import Reporter
import Types

main :: IO ()
main = do
  state <- initializeState
  _ <- forkIO $ runReporter
  _ <- forkIO $ runOverlay
  _ <- forkIO $ runCLI state
  _ <- forkIO $ runReaderT runBot state
  forever (threadDelay maxBound)
