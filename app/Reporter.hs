module Reporter where

import Control.Concurrent.STM.TVar
import Control.Monad.Reader
import System.IO
import System.Process
import Types

-- This is actually just a webserver, and python has a better one

runReporter :: IO ()
runReporter =
  -- Silence
  withFile "/dev/null" WriteMode $
    \null -> do
      (x, y, z, ph) <-
        createProcess
          (proc "python3" ["-m", "http.server", "3000"])
            { cwd = Just "app/reporter",
              std_err = UseHandle null,
              std_out = UseHandle null,
              std_in = UseHandle null
            }
      return ()

runOverlay :: IO ()
runOverlay =
  -- Silence
  withFile "/dev/null" WriteMode $
    \null -> do
      (x, y, z, ph) <-
        createProcess
          (proc "python3" ["-m", "http.server", "3001"])
            { cwd = Just "app/totalizer",
              std_err = UseHandle null,
              std_out = UseHandle null,
              std_in = UseHandle null
            }
      return ()
