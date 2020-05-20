module Types where

import Control.Arrow ((>>>))
import Control.Concurrent.STM
import Control.Monad (unless)
import Control.Monad.Reader
import Countries
import Data.Function ((&))
import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map as Map
import Discord.Types
import System.Directory (doesFileExist)
import Util

type Totals = Map CountryCode Integer

type Ballot = Map Integer CountryCode

type Env = TVar State

type Eurovision = ReaderT (TVar State) IO

--------------------------------------------------------------------------------
-- Game State
data State
  = State
      { ballots :: Map UserId Ballot,
        totals :: Map CountryCode Integer,
        dmChannels :: Map UserId ChannelId,
        scoreMessages :: Map UserId MessageId,
        userNames :: Map UserId String,
        untotaledBallots :: Map UserId Ballot,
        lastPlayed :: Int,
        currentUser :: Maybe UserId
      }
  deriving (Show, Read, Eq)

empty :: State
empty = State mempty mempty mempty mempty mempty mempty (-1) Nothing

stateFile :: String
stateFile = ".state"

getState :: Eurovision State
getState = do
  st <- ask
  liftIO $ readTVarIO st

get :: (State -> a) -> Eurovision a
get func = do
  st <- ask
  liftIO $ func <$> readTVarIO st

modifyState :: (State -> State) -> Eurovision ()
modifyState func = do
  st <- ask
  liftIO $ atomically $ modifyTVar st func
  serialize

-- Read the current total state from a file.
-- Do this only on startup.
initializeState :: IO Env
initializeState = do
  exists <- doesFileExist stateFile
  unless exists $ newTVarIO empty >>= runReaderT serialize
  (readFile stateFile & fmap read) >>= newTVarIO

-- Write the current total state out to a file.
-- Do this every time the state changes.
serialize :: Eurovision ()
serialize = do
  state <- getState
  liftIO $ writeFile stateFile $ show state
  liftIO $ writeFile "app/reporter/report.json" $ getReport (userNames state) (ballots state)
  liftIO $ writeFile "app/totalizer/totals.json" $ getTotals (totals state)

getTotals :: Map CountryCode Integer -> String
getTotals totals = totals
  & Map.toAscList
  & fmap (\(s, c) -> show (show s) ++ ":" ++ show (show c))
  & List.intercalate ",\n"
  & \x -> "{ " ++ x ++ " }"

getReport :: Map UserId String -> Map UserId (Map Integer CountryCode) -> String
getReport userNames ballots =
  ballots
    & Map.toAscList
    & fmap (\(n, v) -> "{ \"name\" : \"" ++ (userNames !@ n) ++ "\" , \"scores\" : { " ++ getScores v ++ " } }")
    & List.intercalate ",\n"
    & (\x -> "[ " ++ x ++ " ]")

getScores :: Map Integer CountryCode -> String
getScores =
  Map.toDescList
    >>> fmap (\(s, c) -> show (show s) ++ ":" ++ show (show c))
    >>> List.intercalate ",\n"
