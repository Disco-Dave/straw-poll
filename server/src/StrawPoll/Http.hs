module StrawPoll.Http
  ( Env (..),
    application,
    start,
  )
where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString as ByteString
import Data.Function ((&))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import Data.Time (getCurrentTime)
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Middleware.Cors as Cors
import StrawPoll.Poll (Answer, Id (..), Poll, SavePoll, SaveVote, VoteResult (..), createPoll, vote)
import Text.Read (readMaybe)

data Env = Env
  { envSavePoll :: SavePoll IO,
    envSaveVote :: SaveVote IO,
    envFindPoll :: Id Poll -> IO (Maybe Poll)
  }

jsonResponse :: Aeson.ToJSON body => HTTP.Status -> body -> Wai.Response
jsonResponse status body =
  Wai.responseLBS
    status
    [("Content-Type", "application/json")]
    (Aeson.encode body)

parseId :: Text -> Maybe (Id entity)
parseId = fmap Id . readMaybe . Text.unpack

withJsonBody ::
  Aeson.FromJSON a =>
  Wai.Request ->
  (Wai.Response -> IO Wai.ResponseReceived) ->
  (a -> IO Wai.ResponseReceived) ->
  IO Wai.ResponseReceived
withJsonBody request send handleBody
  | not hasJsonHeader =
    send $ Wai.responseLBS HTTP.badRequest400 [] mempty
  | otherwise = do
    rawBody <- Wai.strictRequestBody request
    case Aeson.eitherDecode rawBody of
      Right body -> handleBody body
      Left err -> send $ jsonResponse HTTP.badRequest400 err
  where
    isJson = ByteString.isInfixOf "application/json"
    isJsonHeader (headerName, headerValue) =
      headerName == "Content-Type" && isJson headerValue
    hasJsonHeader = any isJsonHeader (Wai.requestHeaders request)

type Handler = Env -> Wai.Application

postPollsHandler :: Handler
postPollsHandler Env {..} request send =
  withJsonBody request send $ \poll -> do
    currentTime <- getCurrentTime
    result <- createPoll envSavePoll currentTime poll
    send $ case result of
      Left errors ->
        jsonResponse HTTP.badRequest400 errors
      Right createdPoll ->
        jsonResponse HTTP.created201 createdPoll

postPollsVotesHandler :: Id Poll -> Id Answer -> Handler
postPollsVotesHandler pollId answerId Env {..} _ send =
  envFindPoll pollId >>= \case
    Nothing ->
      send $ jsonResponse @Text HTTP.notFound404 "Cannot find requested poll."
    Just poll -> do
      currentTime <- getCurrentTime
      voteResult <- vote envSaveVote currentTime poll answerId
      send $ case voteResult of
        AnswerNotFound ->
          jsonResponse @Text HTTP.notFound404 "Cannot find requested answer."
        PollIsExpired ->
          jsonResponse @Text HTTP.badRequest400 "Poll is expired."
        VoteAccepted updatedPoll ->
          jsonResponse HTTP.ok200 updatedPoll

getPollsHandler :: Id Poll -> Handler
getPollsHandler pollId Env {..} _ send = do
  maybePoll <- envFindPoll pollId
  send $ case maybePoll of
    Nothing ->
      Wai.responseLBS HTTP.notFound404 [] mempty
    Just poll ->
      jsonResponse HTTP.ok200 poll

application :: Env -> Wai.Application
application env =
  let addCors =
        Cors.simpleCorsResourcePolicy
          { Cors.corsRequestHeaders = ["Content-Type"]
          }
          & Cors.cors . const . Just
      waiApp request send =
        let method = Wai.requestMethod request
            pathInfo = Wai.pathInfo request
            route = case (method, pathInfo) of
              ("POST", ["polls"]) ->
                postPollsHandler
              ("POST", ["polls", parseId -> Just pollId, "votes", parseId -> Just answerId]) ->
                postPollsVotesHandler pollId answerId
              ("GET", ["polls", parseId -> Just pollId]) ->
                getPollsHandler pollId
              _ ->
                \_ _ _ -> send $ Wai.responseLBS HTTP.notFound404 [] mempty
         in route env request send
   in addCors waiApp

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
