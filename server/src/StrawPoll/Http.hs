{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module StrawPoll.Http
  ( Env (..),
    application,
    start,
  )
where

import Data.Function ((&))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import Data.Time (getCurrentTime)
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import StrawPoll.Poll (Answer, Id (..), Poll, SavePoll, SaveVote, VoteResult (..), createPoll, vote)
import Yesod.Core

data Env = Env
  { envSavePoll :: SavePoll IO,
    envSaveVote :: SaveVote IO,
    envFindPoll :: Id Poll -> IO (Maybe Poll)
  }

instance Yesod Env

mkYesod
  "Env"
  [parseRoutes|
    /polls PollsR POST
    /polls/#Integer PollR GET
    /polls/#Integer/votes/#Integer VoteR POST
  |]

postPollsR :: Handler Value
postPollsR = do
  poll <- requireCheckJsonBody
  currentTime <- liftIO getCurrentTime
  savePoll <- fmap envSavePoll getYesod
  result <- liftIO $ createPoll savePoll currentTime poll
  case result of
    Left errors ->
      sendStatusJSON HTTP.badRequest400 errors
    Right createdPoll ->
      sendStatusJSON HTTP.created201 createdPoll

getPollR :: Integer -> Handler Value
getPollR (Id @Poll -> pollId) = do
  findPoll <- fmap envFindPoll getYesod
  liftIO (findPoll pollId) >>= \case
    Nothing -> sendStatusJSON HTTP.notFound404 ("" :: Text)
    Just poll -> returnJson poll

postVoteR :: Integer -> Integer -> Handler Value
postVoteR (Id @Poll -> pollId) (Id @Answer -> answerId) = do
  env <- getYesod
  let (findPoll, saveVote) = (envFindPoll env, envSaveVote env)
  liftIO (findPoll pollId) >>= \case
    Nothing ->
      sendStatusJSON @_ @Text HTTP.notFound404 "Cannot find requested poll."
    Just poll -> do
      currentTime <- liftIO getCurrentTime
      voteResult <- liftIO $ vote saveVote currentTime poll answerId
      case voteResult of
        AnswerNotFound ->
          sendStatusJSON @_ @Text HTTP.notFound404 "Cannot find requested answer."
        PollIsExpired ->
          sendStatusJSON @_ @Text HTTP.badRequest400 "Poll is expired."
        VoteAccepted updatedPoll ->
          returnJson updatedPoll

application :: Env -> IO Wai.Application
application env = toWaiAppPlain env

start :: Env -> Warp.Port -> IO ()
start env port =
  let settings =
        let printMessage =
              let portText = Text.pack $ show port
                  message = "HTTP server started on port " <> portText
               in TextIO.putStrLn message
         in Warp.defaultSettings
              & Warp.setPort port
              & Warp.setBeforeMainLoop printMessage
   in application env >>= Warp.runSettings settings
