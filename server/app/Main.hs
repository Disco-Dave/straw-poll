module Main (main) where

import Control.Exception (throwIO)
import Data.Pool (Pool, withResource)
import qualified Database.PostgreSQL.Simple as Postgres
import StrawPoll.Config (Config (..))
import qualified StrawPoll.Config as Config
import qualified StrawPoll.Database as Database
import qualified StrawPoll.Http as Http

getConfig :: IO Config
getConfig = do
  configResult <- Config.getConfig
  case configResult of
    Left err -> throwIO err
    Right config -> pure config

makeEnv :: Pool Postgres.Connection -> Http.Env
makeEnv pool =
  Http.Env
    { Http.envSavePoll = \unsavedPoll ->
        withResource pool $ \connection ->
          Database.savePoll connection unsavedPoll,
      Http.envFindPoll = \pollId ->
        withResource pool $ \connection ->
          Database.findPoll connection pollId,
      Http.envSaveVote = \pollId answerId ->
        withResource pool $ \connection ->
          Database.saveVote connection pollId answerId
    }

main :: IO ()
main = do
  Config {..} <- getConfig

  let connectInfo = 
        Postgres.ConnectInfo
          { connectHost = configPgHost,
            connectPort = configPgPort,
            connectUser = configPgUser,
            connectPassword = configPgPassword,
            connectDatabase = "straw_poll"
          }

  env <- makeEnv <$> Database.createPool connectInfo
  Http.start env configHttpPort
