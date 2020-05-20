module Types where

import Data.Map (Map)
import qualified Data.Map as Map

import Control.Monad.Reader
import Control.Concurrent.STM

import System.Directory (doesFileExist)
import Control.Monad (unless)

import Data.Function ((&))

import Discord.Types


import Countries


type Totals = Map CountryCode Integer

type Ballot = Map Integer CountryCode

data Move
  = Lower String
  | Twelve String
  deriving (Show, Read, Eq)

type Env = TVar State
type Eurovision = ReaderT (TVar State) IO


--------------------------------------------------------------------------------
-- Game State
data State = State
  { ballots :: Map UserId Ballot
  , totals :: Map CountryCode Integer
  , moves :: [Move]
  , dmChannels :: Map UserId ChannelId 
  , scoreMessages :: Map UserId MessageId
  }
  deriving (Show, Read, Eq)

instance Semigroup State where
  (State v t m c s) <> (State v2 t2 m2 c2 s2) 
    = State (v<>v2) (t<>t2) (m<>m2) (c<>c2) (s<>s2)

instance Monoid State where 
  mempty = State mempty mempty mempty mempty mempty


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
  unless exists $ newTVarIO mempty >>= runReaderT serialize
  (readFile stateFile & fmap read) >>= newTVarIO

-- Write the current total state out to a file.
-- Do this every time the state changes.
serialize :: Eurovision ()
serialize = do
  state <- getState
  liftIO $ writeFile stateFile $ show state
