module Websockets where

import Data.Text (Text)
import Network.WebSockets.Client

sendCommand :: Text -> IO ()
sendCommand comm = withConnection "ws://localhost:4444" $ \connection -> do
  sendTextData connection comm
  _ <- receiveData connection :: IO Text
  sendClose connection ("Goodbye" :: Text)

setScene :: Text -> IO ()
setScene scene =
  sendCommand $
    "{ \"request-type\":\"SetCurrentScene\", \"scene-name\":\""
      <> scene
      <> "\", \"message-id\":\"\" }"
