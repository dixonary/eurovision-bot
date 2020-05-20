module Bot where

import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Monad (forM, forM_, unless, void)
import Control.Monad.Reader
import Countries
import qualified Data.ByteString as BS
import Data.Map ((!), Map)
import qualified Data.Map as Map
import Data.Text as T
import Discord
import qualified Discord.Internal.Rest.HTTP as Req
import qualified Discord.Requests as Req
import Discord.Types
import Score
import System.Environment
import Types
import Util

runBot :: Eurovision ()
runBot = do
  st <- ask
  getState >>= \s -> liftIO (print s)
  err <- do
    botToken <- liftIO $ getEnv "BOT_TOKEN"
    liftIO $ runDiscord $
      def
        { discordToken = T.pack botToken,
          discordOnEvent = \dh e -> runReaderT (eventHandler dh e) st
        }
  liftIO $ print err
  runBot

eventHandler :: DiscordHandle -> Event -> Eurovision ()
eventHandler dh event = case event of
  MessageCreate m -> newMessage dh m
  MessageReactionAdd r -> addReact dh r
  MessageReactionRemove r -> removeReact dh r
  _ -> return ()

newMessage :: DiscordHandle -> Message -> Eurovision ()
newMessage dh message@Message {..} = do
  dms <- get dmChannels
  if  -- Ignore bot messages
      | userIsBot messageAuthor ->
        return ()
      -- Start a new vote if we don't have one in progress
      | messageText == "!vote" && userId messageAuthor `Map.notMember` dms ->
        startVote message dh
      | messageChannel `elem` Map.elems dms ->
        -- Assume it's a score update and act appropriately
        case parseScore messageText of
          Left err ->
            call_ dh $
              Req.CreateMessage
                messageChannel
                (T.pack err)
          Right score -> do
            recordScore (userId messageAuthor) score
            reactFlag (userId messageAuthor) messageId (countryCode score) dh
            updateBallotVis (userId messageAuthor) dh
      | otherwise -> return ()

-- Add a flag react to a message
reactFlag :: UserId -> MessageId -> CountryCode -> DiscordHandle -> Eurovision ()
reactFlag uid mid code dh = do
  dmChannelM <- get $ (!? uid) . dmChannels
  case dmChannelM of
    Nothing -> return ()
    Just dmChannel ->
      call_ dh $ Req.CreateReaction (dmChannel, mid) (getEmoji code)

-- Given a score, add it to the ballot.
recordScore :: UserId -> Score -> Eurovision ()
recordScore uid score = do
  ballots <- get ballots
  let ballot' = updateBallot (ballots !@ uid) score
      ballots' = Map.insert uid ballot' ballots
  modifyState (\state -> state {ballots = ballots'})

-- Update the rendering of a user's ballot.
updateBallotVis :: UserId -> DiscordHandle -> Eurovision ()
updateBallotVis uid dh = do
  ballot <- get $ (!@ uid) . ballots
  channelIdM <- get $ (!? uid) . dmChannels
  messageIdM <- get $ (!? uid) . scoreMessages
  case (,) <$> channelIdM <*> messageIdM of
    Nothing -> return ()
    Just ballotMsg -> do
      call_ dh $
        Req.EditMessage
          ballotMsg
          (showBallot ballot)
          Nothing

-- Get someone ready to vote.
startVote :: Message -> DiscordHandle -> Eurovision ()
startVote msg dh = do
  let uid = userId $ messageAuthor msg
  dmChannelE <- call dh $ Req.CreateDM uid
  case dmChannelE of
    Left err -> liftIO $ print err
    Right dmChannel -> do
      -- Delete all messages in the channel
      messagesE <-
        call dh $
          Req.GetChannelMessages
            (channelId dmChannel)
            (100, Req.LatestMessages)
      case messagesE of
        Left err -> liftIO $ print err
        Right messages ->
          forM_ messages $ \message ->
            call dh $
              Req.DeleteMessage
                (channelId dmChannel, messageId message)
      -- Send a friendly image
      imgBytes <- liftIO $ BS.readFile "/home/alex/eurovision-logo.png"
      res <-
        call dh $
          Req.CreateMessageUploadFile
            (channelId dmChannel)
            "euro-2020.png"
            imgBytes
      call dh
        $ Req.CreateMessage
          (channelId dmChannel)
        $ T.unlines
          [ "Send me a message with a 3 letter country code and the points to award (eg. `GBR 12`).",
            "If you assign the same amount of points more than once, I'll move other countries down to make space."
          ]
      -- Send the ballot message
      ballotMsgE <-
        call dh
          $ Req.CreateMessage
            (channelId dmChannel)
          $ showBallot mempty
      case ballotMsgE of
        Left err -> liftIO $ print err
        Right ballotMsg -> do
          modifyState
            ( \s@State {..} ->
                s
                  { scoreMessages = Map.insert uid (messageId ballotMsg) scoreMessages,
                    ballots = Map.insert uid mempty ballots,
                    dmChannels = Map.insert uid (channelId dmChannel) dmChannels,
                    userNames = Map.insert uid (T.unpack $ userName $ messageAuthor msg) userNames
                  }
            )

-- Render a ballot as shown in Discord.
showBallot :: Ballot -> Text
showBallot ballot = T.unlines $ "**Your Scorecard**" : scoreLines
  where
    scoreLines = flip fmap [12, 10, 8, 7, 6, 5, 4, 3, 2, 1] $ \i ->
      let rhs = case Map.lookup i ballot of
            Nothing -> "-"
            Just c ->
              T.concat
                [ getFlag c,
                  " ",
                  getName c,
                  " (",
                  T.pack $ show c,
                  ")"
                ]
       in "**" <> T.pack (show i) <> " Points:** " <> rhs

-- Call an API endpoint.
call ::
  (Req.Request (r a), FromJSON a) =>
  DiscordHandle ->
  r a ->
  Eurovision (Either RestCallErrorCode a)
call dh req = liftIO $ restCall dh req

-- Call an API endpoint, discarding the result.
call_ ::
  (Req.Request (r a), FromJSON a) =>
  DiscordHandle ->
  r a ->
  Eurovision ()
call_ dh req = liftIO $ void $ restCall dh req

addReact :: DiscordHandle -> ReactionInfo -> Eurovision ()
addReact dh ReactionInfo {..} = undefined

removeReact :: DiscordHandle -> ReactionInfo -> Eurovision ()
removeReact dh ReactionInfo {..} = undefined
