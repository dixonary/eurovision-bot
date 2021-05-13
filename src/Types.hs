{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Types where

import Control.Arrow ((>>>))
import Control.Concurrent.STM
import Control.Monad.Reader
import Countries
import Data.Function ((&))
import Data.Functor ((<&>))
import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map as Map
import System.Directory (doesFileExist)
import Util

import Discord
import Discord.Types

type Totals = Map CountryCode Integer

type Ballot = Map Integer CountryCode

type EuroState'  = TVar EuroState
type Eurovision  = ReaderT EuroState' IO
type EuroDiscord = ReaderT EuroState' DiscordHandler 

--------------------------------------------------------------------------------
-- Game State
data EuroState
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

class HasEuroState m where
  get    :: m EuroState
  gets   :: (EuroState -> a) -> m a
  modify :: (EuroState -> EuroState) -> m ()
  set    :: EuroState -> m ()

instance HasEuroState Eurovision where
  get         = ask >>= liftIO . readTVarIO
  gets func   = get <&> func  
  modify func = get >>= set    . func
  set state = do
    st' <- ask
    liftIO $ atomically $ writeTVar st' state
    liftIO $ serialize state

-- Defer to the definitions over IO
instance HasEuroState EuroDiscord where
  get    = io get
  gets   = io . gets
  set    = io . set
  modify = io . modify
  
io :: Eurovision a -> EuroDiscord a
io = mapReaderT lift

empty :: EuroState
empty = State mempty mempty mempty mempty mempty mempty (-1) Nothing

stateFile :: String
stateFile = ".state"

-- Read the current total state from a file.
-- Do this only on startup.
initializeState :: IO EuroState'
initializeState = do
  stateExists <- doesFileExist stateFile
  unless stateExists $ serialize empty
  readFile stateFile >>= newTVarIO . read

-- Write the current total state out to a file.
-- Do this every time the state changes.
serialize :: EuroState -> IO ()
serialize state = do
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
