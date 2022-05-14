{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
module Bot where

import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Monad (forM, forM_, unless, void)
import Control.Monad.Reader
import Countries
import qualified Data.ByteString as BS
import Data.Map (Map, (!))
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
import Discord.Internal.Rest.Channel (MessageDetailedOpts(..))

runBot :: Eurovision ()
runBot = do
  st <- ask
  getState >>= \s -> liftIO $ print s
  err <- do
    botToken <- liftIO $ getEnv "BOT_TOKEN"
    liftIO $
      runDiscord $
        def
          { discordToken = T.pack botToken,
            discordOnEvent = \e -> void $ runReaderT (eventHandler e) st
          }
  liftIO $ print err
  runBot

eventHandler :: Event -> EurovisionBot ()
eventHandler event = case event of
  MessageCreate m -> newMessage m
  MessageReactionAdd r -> addReact r
  MessageReactionRemove r -> removeReact r
  _ -> return ()

newMessage :: Message -> EurovisionBot ()
newMessage message@Message {..} = do
  dms <- liftEuro $ get dmChannels
  if
      -- Ignore bot messages
      | userIsBot messageAuthor ->
        return ()
      -- Start a new vote if we don't have one in progress
      | "!vote" `isPrefixOf` messageContent 
          && userId messageAuthor `Map.notMember` dms ->
        startVote message
      | messageChannelId `elem` Map.elems dms ->
        -- Assume it's a score update and act appropriately
        case parseScore messageContent of
          Left err ->
            call_ $
              Req.CreateMessage
                messageChannelId
                (T.pack err)
          Right score -> do
            recordScore (userId messageAuthor) score
            reactFlag (userId messageAuthor) messageId (countryCode score)
            updateBallotVis (userId messageAuthor)
      | otherwise -> return ()

-- Add a flag react to a message
reactFlag :: UserId -> MessageId -> CountryCode -> EurovisionBot ()
reactFlag uid mid code = do
  dmChannelM <- liftEuro $ get $ (!? uid) . dmChannels
  case dmChannelM of
    Nothing -> return ()
    Just dmChannel ->
      call_ $ Req.CreateReaction (dmChannel, mid) (getEmoji code)

-- Given a score, add it to the ballot.
recordScore :: UserId -> Score -> EurovisionBot ()
recordScore uid score = do
  ballots <- liftEuro $ get ballots
  let ballot' = updateBallot (ballots !@ uid) score
      ballots' = Map.insert uid ballot' ballots
  liftEuro $ modifyState (\state -> state {ballots = ballots'})

-- Update the rendering of a user's ballot.
updateBallotVis :: UserId -> EurovisionBot ()
updateBallotVis uid = do
  ballot <- liftEuro $ get $ (!@ uid) . ballots
  channelIdM <- liftEuro $ get $ (!? uid) . dmChannels
  messageIdM <- liftEuro $ get $ (!? uid) . scoreMessages
  case (,) <$> channelIdM <*> messageIdM of
    Nothing -> return ()
    Just ballotMsg -> do
      call_ $
        Req.EditMessage
          ballotMsg
          def {messageDetailedContent = showBallot ballot }

-- Get someone ready to vote.
startVote :: Message -> EurovisionBot ()
startVote msg = do
  call $ Req.CreateReaction (messageChannelId msg, messageId msg) "white_check_mark"
  let uid = userId $ messageAuthor msg
  dmChannelE <- call $ Req.CreateDM uid
  case dmChannelE of
    Left err -> liftIO $ print err
    Right dmChannel -> do
      -- Delete all messages in the channel
      -- messagesE <-
      --   call $
      --     Req.GetChannelMessages
      --       (channelId dmChannel)
      --       (100, Req.LatestMessages)
      -- case messagesE of
      --   Left err -> liftIO $ print err
      --   Right messages ->
      --     forM_ messages $ \message ->
      --       call $
      --         Req.DeleteMessage
      --           (channelId dmChannel, messageId message)
      -- Send a friendly image
      imgBytes <- liftIO $ BS.readFile "EurovisionBot-logo.png"
      res <-
        call $
          Req.CreateMessageDetailed
            (channelId dmChannel)
            def
              { messageDetailedFile = Just ("EurovisionBot-logo.png", imgBytes) }
      call $
        Req.CreateMessage
          (channelId dmChannel)
          $ T.unlines
            [ "Send me a message with a 3 letter country code and the points to award (eg. `GBR 12`).",
              "If you assign the same amount of points more than once, I'll move other countries down to make space."
            ]
      -- Send the ballot message
      ballotMsgE <-
        call $
          Req.CreateMessage
            (channelId dmChannel)
            $ showBallot mempty
      case ballotMsgE of
        Left err -> liftIO $ print err
        Right ballotMsg -> do
          liftEuro $ modifyState
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
  r a ->
  EurovisionBot (Either RestCallErrorCode a)
call req = liftDiscord $ restCall req

-- Call an API endpoint, discarding the result.
call_ ::
  (Req.Request (r a), FromJSON a) =>
  r a ->
  EurovisionBot ()
call_ req = liftDiscord $ void $ restCall req

liftDiscord :: forall a . DiscordHandler a -> EurovisionBot a
liftDiscord d = do
  x <- lift ask
  liftIO $ runReaderT d x

liftEuro :: forall a . Eurovision a -> EurovisionBot a
liftEuro e = do
  x <- ask
  liftIO $ runReaderT e x

addReact :: ReactionInfo -> EurovisionBot ()
addReact ReactionInfo {..} = undefined

removeReact :: ReactionInfo -> EurovisionBot ()
removeReact ReactionInfo {..} = undefined
