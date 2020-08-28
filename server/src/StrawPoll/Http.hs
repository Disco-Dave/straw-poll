module StrawPoll.Http
  ( Env (..),
    application,
    start,
  )
where

import Control.Monad.IO.Class (MonadIO (..))
import qualified Data.Aeson as Aeson
import Data.Function ((&))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import Data.Time (getCurrentTime)
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import Servant
import StrawPoll.Poll

data Env = Env
  { envSavePoll :: SavePoll IO,
    envSaveVote :: SaveVote IO,
    envFindPoll :: Id Poll -> IO (Maybe Poll)
  }

type Api =
  "polls"
    :> ( Capture "pollId" Integer :> Get '[JSON] Poll
           :<|> ReqBody '[JSON] CreatePollRequest :> Verb 'POST 201 '[JSON] Poll
           :<|> Capture "pollId" Integer :> "votes" :> Capture "answerId" Integer :> Get '[JSON] Poll
       )

throwJsonError :: Aeson.ToJSON body => ServerError -> body -> Handler a
throwJsonError serverError body =
  throwError $
    serverError
      { errHeaders = ("Content-Type", "application/json") : errHeaders serverError,
        errBody = Aeson.encode body
      }

handleFindPoll :: Env -> Integer -> Handler Poll
handleFindPoll Env {..} (Id -> pollId) =
  liftIO (envFindPoll pollId) >>= \case
    Nothing -> throwError err404
    Just poll -> pure poll

handleCreatePoll :: Env -> CreatePollRequest -> Handler Poll
handleCreatePoll Env {..} createRequest = do
  currentTime <- liftIO getCurrentTime
  result <- liftIO $ createPoll envSavePoll currentTime createRequest
  case result of
    Right poll -> pure poll
    Left errors -> throwJsonError err400 errors

handleVoting :: Env -> Integer -> Integer -> Handler Poll
handleVoting Env {..} (Id -> pollId) (Id -> answerId) =
  liftIO (envFindPoll pollId) >>= \case
    Nothing -> throwJsonError @Text err404 "Cannot find requested poll."
    Just poll -> do
      currentTime <- liftIO getCurrentTime
      result <- liftIO $ vote envSaveVote currentTime poll answerId
      case result of
        AnswerNotFound ->
          throwJsonError @Text err404 "Cannot find requested answer."
        PollIsExpired ->
          throwJsonError @Text err400 "Poll is expired."
        VoteAccepted updatedPoll ->
          pure updatedPoll

server :: Env -> Server Api
server env =
  handleFindPoll env
    :<|> handleCreatePoll env
    :<|> handleVoting env

application :: Env -> Wai.Application
application env = serve (Proxy @Api) (server env)

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
   in Warp.runSettings settings $ application env
