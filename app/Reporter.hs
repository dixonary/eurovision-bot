module Reporter where

import Control.Concurrent.STM.TVar
import Types

runReporter :: TVar State -> IO ()
runReporter = const $ return ()