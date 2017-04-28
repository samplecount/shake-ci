-- Copyright 2017 Samplecount S.L.
--
-- All rights reserved.

{-# LANGUAGE OverloadedStrings #-}

module Development.Shake.CI.Slack (
    Message
  , Author(..)
  , author_
  , message_
  , message
  , slack
  , notify
  , jenkins
) where

import Data.Aeson
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as T
import Development.Shake
import Development.Shake.CI.Env (getEnv', withEnvIO)
import Network.Wreq

data Message = Message (Maybe Text) [Attachment]
                deriving (Show)

instance ToJSON Message where
  toJSON (Message text attachments) = object $ catMaybes [
      ("text" .=) <$> text, Just ("attachments" .= attachments)
    ]

data Author = Author {
    authorName :: Text
  , authorLink :: Maybe Text
  , authorIcon :: Maybe Text
  } deriving (Show)

author_ :: Text -> Author
author_ n = Author n Nothing Nothing

data Attachment = Attachment {
    color :: Maybe Text
  , author :: Maybe Author
  , title :: Text
  , text :: Text
  } deriving (Show)

attachment :: Text -> Text -> Attachment
attachment = Attachment Nothing Nothing

instance ToJSON Attachment where
  toJSON x = object $ catMaybes [
      ("color" .=) <$> color x
    , ("author_name" .=) <$> (authorName <$> author x)
    , ("author_link" .=) <$> (authorLink =<< author x)
    , ("author_icon" .=) <$> (authorIcon =<< author x)
    , Just ("title" .= title x)
    , Just ("text" .= text x)
    ]
  
message_ :: Text -> Message
message_ = flip Message [] . Just

message :: Maybe Text -> [Attachment] -> Message
message = Message

link :: Text -> Text -> Text
link url desc = T.concat ["<", url, "|", desc, ">"]

-- | Post a text message to a slack channel.
--
-- SLACK_URL environment variable needs to be defined.
slack :: Message -> Action ()
slack = liftIO . slackIO

slackIO :: Message -> IO ()
slackIO msg = withEnvIO "SLACK_URL" $ \url -> do
  _ <- post url (toJSON msg)
  return ()

data Notifications = Notifications {
    start :: Message
  , success :: Message
  , failure :: Message
  } deriving (Show)

notify :: Action Notifications -> Action a -> Action a
notify notifications action = do
  ns <- notifications
  actionOnException (do { liftIO (slackIO (start ns)) ; result <- action ; liftIO (slackIO (success ns)) ; return result })
                    (slackIO (failure ns))

jenkins :: Maybe String -> Action Notifications
jenkins tag = do
  jenkinsURL <- T.pack <$> getEnv' "JENKINS_URL"
  jobName <- T.pack <$> getEnv' "JOB_NAME"
  buildURL <- T.pack <$> getEnv' "BUILD_URL"
  buildNumber <- T.pack <$> getEnv' "BUILD_NUMBER"
  let msg t = attachment (link buildURL jobName)
                         (T.concat [t, " (", link (T.concat [buildURL, "/console"]) (T.concat ["#", buildNumber]), ")"])
      withTag x = T.concat $ [x] ++ maybe [] (\t -> [" ", T.pack t]) tag
  return Notifications {
      start = message Nothing [msg $ withTag "Start"]
    , success = message Nothing [msg $ withTag "Success"]
    , failure = message Nothing [msg $ withTag "Failure "]
    }
