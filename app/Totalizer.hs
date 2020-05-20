module Totalizer where

import Control.Concurrent.STM.TVar
import Types

import Control.Monad.Reader

runTotalizer :: Env -> IO ()
runTotalizer st = do
  x <- liftIO getLine
  case x of
    ":clear" -> runReaderT (modifyState (const mempty)) st >> runTotalizer st
    _ -> runTotalizer st