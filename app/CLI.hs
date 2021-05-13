module CLI where

import Control.Arrow ((>>>))
import Control.Monad.Reader
import Countries
import Data.Function ((&))
import Data.List (elemIndex)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import System.Console.Haskeline
import System.IO hiding (getLine)
import System.Process
import Text.Read (readMaybe)
import Types
import Util
import Websockets
import Prelude hiding (getLine)


type CLI = ReaderT EuroState' IO

runCLI :: EuroState' -> IO ()
runCLI = runReaderT loop

loop :: CLI ()
loop = do
  line <- liftIO $ getLine "> "
  let xM = line >>= readMaybe

  case xM of
    Nothing -> liftIO $ putStrLn "??"
    Just x -> case x of
      FullState      -> get >>= liftIO . print
      Reset          -> set empty
      Titles         -> liftIO playTitles
      PlayNext       -> playNext
      TakeBallots    -> takeBallots
      Play n         -> liftIO $ play n
      User u         -> setUser u
      Remaining      -> pure ()
      Ten            -> ten
      Twelve         -> twelve
      Totals         -> printTotals
      AddAllFor cc   -> addAllFor cc
      AddAll         -> sequence_ (addAllFor <$> [minBound .. maxBound])

  loop

printTotals :: CLI ()
printTotals = do
  totals <- gets totals
  liftIO $ print totals

takeBallots :: CLI ()
takeBallots = modify (\s@State {..} -> s {untotaledBallots = ballots})

ten :: CLI ()
ten = addOneScore 10

twelve :: CLI ()
twelve = addOneScore 12

addOneScore :: Integer -> CLI ()
addOneScore i = do
  s@State {..} <- get
  case currentUser of
    Nothing -> return ()
    Just uid -> do
      let ballot = untotaledBallots !@ uid
      let country = Map.lookup i ballot
      let ballot' = untotaledBallots !@ uid & Map.delete i
      let totals' = case country of
            Nothing -> totals
            Just cc -> Map.insertWith (+) cc i totals
      set s
        { untotaledBallots = Map.insert uid ballot' untotaledBallots,
          totals = totals'
        }

addAllFor :: CountryCode -> CLI ()
addAllFor cc = do
  ballots <- gets untotaledBallots
  -- get total of scores in all ballots where
  let total = sum $ fmap getCC ballots
      ballots' = fmap removeCC ballots
      getCC = Map.assocs >>> fmap (\(x, y) -> (y, x)) >>> lookup cc >>> fromMaybe 0
      removeCC = Map.toAscList >>> filter (\(_, y) -> y /= cc) >>> Map.fromAscList
  modify
    ( \s@State {..} ->
        s
          { totals = Map.insertWith (+) cc total totals,
            untotaledBallots = ballots'
          }
    )

-- remove from untotaled ballots

setUser :: String -> CLI ()
setUser s =
  do
    unames <- gets userNames
    let uidM =
          unames
            & Map.toAscList
            & fmap (\(x, y) -> (y, x))
            & lookup s
    case uidM of
      Nothing -> liftIO $ putStrLn $ "User not found: " ++ s
      Just uid ->
        modify (\s -> s {currentUser = Just uid})

playTitles :: IO ()
playTitles = setScene "titles"

playNext :: CLI ()
playNext = do
  lp <- gets lastPlayed
  let nowPlay =
        if (lp + 1) > length ([minBound .. maxBound] :: [CountryCode])
          then 0
          else lp + 1
  modify (\s -> s {lastPlayed = nowPlay})
  liftIO $ play (toEnum nowPlay)

-- Play a song
play :: CountryCode -> IO ()
play code = do
  let i = (elemIndex code [minBound .. maxBound] ?? 0) + 1
  print code
  writeFile ".countryCode" $ show code
  writeFile ".longName" $ getName code
  writeFile ".num" $ show i
  withFile "/dev/null" WriteMode $
    \devnull -> do
      (_, _, _, ph) <-
        createProcess
          (proc "vlc" ["--fullscreen", show code ++ ".mp4"])
            { cwd = Just "/run/media/alex/data/eurovision-2020",
              std_err = UseHandle devnull,
              std_out = UseHandle devnull,
              std_in = UseHandle devnull
            }
      setScene "song"
      _ <- waitForProcess ph
      setScene "sting"

data Command
  = Reset
  | FullState
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
