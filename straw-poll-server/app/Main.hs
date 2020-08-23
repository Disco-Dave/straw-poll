module Main (main) where

import Control.Exception (throw)
import Data.Pool (Pool, withResource)
import Data.Time (getCurrentTime)
import qualified Database.PostgreSQL.Simple as Postgres
import StrawPoll.Config (Config (..))
import qualified StrawPoll.Config as Config
import qualified StrawPoll.Database as Database
import qualified StrawPoll.Http as Http

getConfig :: IO Config
getConfig = do
  configResult <- Config.getConfig
  case configResult of
    Left err -> throw err
    Right config -> pure config

makeEnv :: Pool Postgres.Connection -> Http.Env
makeEnv pool =
  Http.Env
    { Http.envGetCurrentTime =
        getCurrentTime,
      Http.envSavePoll = \unsavedPoll ->
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
  env <- makeEnv <$> Database.createPool configConnectionString
  Http.start env configHttpPort
