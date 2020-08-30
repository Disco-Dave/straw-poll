module StrawPoll.Http.Handlers where

import Data.Text (Text)
import Data.Time (getCurrentTime)
import qualified Network.HTTP.Types as HTTP
import StrawPoll.Http.Env (Env (..), Handler)
import StrawPoll.Poll (Answer, Id (..), Poll, VoteResult (..), createPoll, vote)
import Yesod.Core

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
