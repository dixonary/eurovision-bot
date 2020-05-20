module CLI where

import Control.Arrow ((>>>))
import Control.Concurrent (threadDelay)
import Control.Concurrent.STM.TVar
import Control.Monad.Reader
import Countries
import Data.Function ((&))
import Data.List (elemIndex)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import System.Console.Haskeline
import System.IO hiding (getLine)
import System.Process
import Text.Read (readMaybe)
import Types
import Util
import Websockets
import Prelude hiding (getLine)

runCLI :: Env -> IO ()
runCLI = runReaderT loop

loop :: Eurovision ()
loop = do
  x <- (>>= readMaybe) <$> liftIO (getLine "> ")
  case x of
    Just Reset -> modifyState (const empty)
    Just Titles -> liftIO $ playTitles
    Just PlayNext -> playNext
    Just TakeBallots -> takeBallots
    Just (Play n) -> liftIO $ play n
    Just (User u) -> setUser u
    Just Ten -> ten
    Just Twelve -> twelve
    Just Totals -> printTotals
    Just (AddAllFor cc) -> addAllFor cc
    Just (AddAll) -> void $ sequence (addAllFor <$> [minBound .. maxBound])
    Nothing -> liftIO $ putStrLn "??"
  loop

printTotals :: Eurovision ()
printTotals = do
  totals <- get totals
  liftIO $ print totals

takeBallots :: Eurovision ()
takeBallots = modifyState (\s@State {..} -> s {untotaledBallots = ballots})

ten :: Eurovision ()
ten = addOneScore 10

twelve :: Eurovision ()
twelve = addOneScore 12

addOneScore :: Integer -> Eurovision ()
addOneScore i = do
  s@State {..} <- getState
  case currentUser of
    Nothing -> return ()
    Just uid -> do
      let ballot = untotaledBallots !@ uid
      let country = Map.lookup i ballot
      let ballot' = untotaledBallots !@ uid & Map.delete i
      let totals' = case country of
            Nothing -> totals
            Just cc -> Map.insertWith (+) cc i totals
      modifyState
        ( const
            s
              { untotaledBallots = Map.insert uid ballot' untotaledBallots,
                totals = totals'
              }
        )

addAllFor :: CountryCode -> Eurovision ()
addAllFor cc = do
  ballots <- get untotaledBallots
  -- get total of scores in all ballots where
  let total = sum $ fmap getCC ballots
      ballots' = fmap removeCC ballots
      getCC = Map.assocs >>> fmap (\(x, y) -> (y, x)) >>> lookup cc >>> fromMaybe 0
      removeCC = Map.toAscList >>> filter (\(x, y) -> y /= cc) >>> Map.fromAscList
  modifyState
    ( \s@State {..} ->
        s
          { totals = Map.insertWith (+) cc total totals,
            untotaledBallots = ballots'
          }
    )

-- remove from untotaled ballots

setUser :: String -> Eurovision ()
setUser s =
  do
    unames <- get userNames
    let uidM =
          unames
            & Map.toAscList
            & fmap (\(x, y) -> (y, x))
            & lookup s
    case uidM of
      Nothing -> liftIO $ putStrLn $ "User not found: " ++ s
      Just uid ->
        modifyState (\s@State {..} -> s {currentUser = Just uid})

playTitles :: IO ()
playTitles = setScene "titles"

playNext :: Eurovision ()
playNext = do
  lp <- get lastPlayed
  let nowPlay =
        if (lp + 1) > (length ([minBound .. maxBound] :: [CountryCode]))
          then 0
          else (lp + 1)
  modifyState (\s@State {..} -> s {lastPlayed = nowPlay})
  liftIO $ play (toEnum nowPlay)

-- Play a song
play :: CountryCode -> IO ()
play code = do
  let i = (elemIndex code [minBound .. maxBound] ?? 0) + 1
  putStrLn $ show code
  writeFile ".countryCode" $ show code
  writeFile ".longName" $ getName code
  writeFile ".num" $ show i
  withFile "/dev/null" WriteMode $
    \null -> do
      (x, y, z, ph) <-
        createProcess
          (proc "vlc" ["--fullscreen", show code ++ ".mp4"])
            { cwd = Just "/run/media/alex/data/eurovision-2020",
              std_err = UseHandle null,
              std_out = UseHandle null,
              std_in = UseHandle null
            }
      setScene "song"
      waitForProcess ph
      setScene "sting"

data Command
  = Reset
  | Titles
  | User String
  | TakeBallots
  | Ten
  | Twelve
  | Totals
  | Remaining
  | AddAll
  | AddAllFor CountryCode
  | Play CountryCode
  | PlayNext
  deriving (Read)

getLine :: String -> IO (Maybe String)
getLine = runInputT defaultSettings . getInputLine
