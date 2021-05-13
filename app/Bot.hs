module Bot where

import Control.Monad.Reader
import Countries
import qualified Data.ByteString as BS
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

  curr <- get
  liftIO $ print curr

  err <- do
    botToken <- liftIO $ getEnv "BOT_TOKEN"
    liftIO $ runDiscord $
      def
        { discordToken = T.pack botToken,
          discordOnEvent = \e -> runReaderT (eventHandler e) st
        }
  liftIO $ print err
  runBot

eventHandler :: Event -> EuroDiscord ()
eventHandler event = case event of
  MessageCreate m -> newMessage m
  MessageReactionAdd r -> addReact r
  MessageReactionRemove r -> removeReact r
  _ -> return ()

newMessage :: Message -> EuroDiscord ()
newMessage message@Message {..} = do
  dms <- gets dmChannels
  if  -- Ignore bot messages
      | userIsBot messageAuthor ->
        return ()
      -- Start a new vote if we don't have one in progress
      | messageText == "!vote" && userId messageAuthor `Map.notMember` dms ->
        startVote message
      | messageChannel `elem` Map.elems dms ->
        -- Assume it's a score update and act appropriately
        case parseScore messageText of
          Left _ ->
            -- call_ $
              -- Req.CreateMessage
              --   messageChannel
              --   (T.pack err)
            call_ $
              Req.CreateReaction (messageChannel, messageId) "question"
          Right score -> do
            recordScore (userId messageAuthor) score
            reactFlag (userId messageAuthor) messageId (countryCode score)
            updateBallotVis (userId messageAuthor)
      | otherwise -> return ()

-- Add a flag react to a message
reactFlag :: UserId -> MessageId -> CountryCode -> EuroDiscord ()
reactFlag uid mid code = do
  dmChannelM <- gets $ (!? uid) . dmChannels
  case dmChannelM of
    Nothing -> return ()
    Just dmChannel ->
      call_ $ Req.CreateReaction (dmChannel, mid) (getEmoji code)

-- Given a score, add it to the ballot.
recordScore :: UserId -> Score -> EuroDiscord ()
recordScore uid score = do
  ballots <- gets ballots
  let ballot' = updateBallot (ballots !@ uid) score
      ballots' = Map.insert uid ballot' ballots
  modify (\state -> state {ballots = ballots'})

-- Update the rendering of a user's ballot.
updateBallotVis :: UserId -> EuroDiscord ()
updateBallotVis uid = do
  ballot <- gets $ (!@ uid) . ballots
  channelIdM <- gets $ (!? uid) . dmChannels
  messageIdM <- gets $ (!? uid) . scoreMessages
  case (,) <$> channelIdM <*> messageIdM of
    Nothing -> return ()
    Just ballotMsg -> do
      call_ $
        Req.EditMessage
          ballotMsg
          (showBallot ballot)
          Nothing

-- Get someone ready to vote.
startVote :: Message -> EuroDiscord ()
startVote msg = do
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

      -- Send a horizontal bar
      call_ $ 
        Req.CreateMessage 
          (channelId dmChannel)
          $ T.replicate 80 "="

      -- Send a friendly image
      imgBytes <- liftIO $ BS.readFile "eurovision-2021.png"
      call_ $
          Req.CreateMessageUploadFile
            (channelId dmChannel)
            "euro-2020.png"
            imgBytes
      call_
        $ Req.CreateMessage
          (channelId dmChannel)
        $ T.unlines
          [ "Send me a message with a 3 letter country code and the points to award (eg. `GBR 12`).",
            "If you assign the same amount of points more than once, I'll move other countries down to make space."
          ]

      -- Send the ballot message
      ballotMsgE <-
        call
          $ Req.CreateMessage
            (channelId dmChannel)
          $ showBallot mempty
      case ballotMsgE of
        Left err -> liftIO $ print err
        Right ballotMsg -> do
          modify
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
  EuroDiscord (Either RestCallErrorCode a)
call = lift . restCall

-- Call an API endpoint, discarding the result.
call_ ::
  (Req.Request (r a), FromJSON a) =>
  r a ->
  EuroDiscord ()
call_ req = void $ lift $ restCall req

addReact :: ReactionInfo -> EuroDiscord ()
addReact _ = pure ()

removeReact :: ReactionInfo -> EuroDiscord ()
removeReact _ = pure ()
