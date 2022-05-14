module Main where

import Bot
import CLI
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever)
import Control.Monad.Reader
import Countries
import Reporter
import Types

main :: IO ()
main = do
  state <- initializeState
  forkIO $ runReporter
  forkIO $ runOverlay
  forkIO $ runCLI state
  forkIO $ runReaderT runBot state
  forever (threadDelay maxBound)