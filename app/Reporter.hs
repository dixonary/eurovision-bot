module Reporter where

import System.IO
import System.Process

-- This is actually just a webserver, and python has a better one

runReporter :: IO ()
runReporter =
  -- Silence
  withFile "/dev/null" WriteMode $
    \devNull -> do
      _ <-
        createProcess
          (proc "python3" ["-m", "http.server", "3000"])
            { cwd = Just "app/reporter",
              std_err = UseHandle devNull,
              std_out = UseHandle devNull,
              std_in = UseHandle devNull
            }
      return ()

runOverlay :: IO ()
runOverlay =
  -- Silence
  withFile "/dev/null" WriteMode $
    \devNull -> do
      _ <-
        createProcess
          (proc "python3" ["-m", "http.server", "3001"])
            { cwd = Just "app/totalizer",
              std_err = UseHandle devNull,
              std_out = UseHandle devNull,
              std_in = UseHandle devNull
            }
      return ()
