module StrawPoll.Config
  ( Config (..),
    ConfigErrorReason (..),
    ConfigError (..),
    ConfigErrors (..),
    getConfig,
  )
where

import Control.Exception (Exception)
import Data.Foldable (toList)
import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Word (Word16)
import Network.Wai.Handler.Warp (Port)
import StrawPoll.Validation (Validation (..))
import qualified StrawPoll.Validation as Validation
import System.Environment (lookupEnv)
import Text.Read (readMaybe)
import Type.Reflection (Typeable)

data ConfigVar value = ConfigVar
  { configVarName :: !String,
    configVarDescription :: !String,
    configVarConvert :: !(String -> Maybe value)
  }

data Config = Config
  { configHttpPort :: !Port,
    configPgHost :: !String,
    configPgPort :: !Word16,
    configPgUser :: !String,
    configPgPassword :: !String
  }
  deriving (Show, Eq)

data ConfigErrorReason
  = VariableNotSet
  | ConversionFailed
  deriving (Show, Eq, Typeable)

data ConfigError = ConfigError
  { configErrorName :: !String,
    configErrorDescription :: !String,
    configErrorReason :: !ConfigErrorReason
  }
  deriving (Show, Eq, Typeable)

newtype ConfigErrors = ConfigErrors (NonEmpty ConfigError)
  deriving (Eq, Typeable)

instance Semigroup ConfigErrors where
  ConfigErrors e1 <> ConfigErrors e2 =
    ConfigErrors $ e1 <> e2

instance Show ConfigErrors where
  show (ConfigErrors errors) =
    let displayError ConfigError {..} =
          let displayReason =
                case configErrorReason of
                  VariableNotSet -> "Variable not set"
                  ConversionFailed -> "Coversion failed"
           in "\t" <> configErrorName <> " - " <> displayReason <> " - " <> configErrorDescription
     in "Problems with the following environment variables: \n"
          <> (intercalate "\n" . fmap displayError $ toList errors)

instance Exception ConfigErrors

runConfigVar :: ConfigVar value -> IO (Validation ConfigErrors value)
runConfigVar ConfigVar {..} =
  let failConfigVar reason =
        ConfigErrors $
          ConfigError
            { configErrorName = configVarName,
              configErrorDescription = configVarDescription,
              configErrorReason = reason
            }
            :| []
   in do
        env <- lookupEnv configVarName
        pure $ case fmap configVarConvert env of
          Nothing -> Failure $ failConfigVar VariableNotSet
          Just Nothing -> Failure $ failConfigVar ConversionFailed
          Just (Just value) -> Success value

getConfig :: IO (Either ConfigErrors Config)
getConfig = do
  httpPort <-
    runConfigVar $
      ConfigVar
        { configVarName = "STRAW_POLL_HTTP_PORT",
          configVarDescription = "Port to run HTTP server on",
          configVarConvert = readMaybe
        }
  pgHost <-
    runConfigVar $
      ConfigVar
        { configVarName = "STRAW_POLL_PG_HOST",
          configVarDescription = "Postgres host for the application",
          configVarConvert = pure
        }
  pgPort <-
    runConfigVar $
      ConfigVar
        { configVarName = "STRAW_POLL_PG_PORT",
          configVarDescription = "Postgres port for the application",
          configVarConvert = readMaybe
        }
  pgUser <-
    runConfigVar $
      ConfigVar
        { configVarName = "STRAW_POLL_PG_USER",
          configVarDescription = "Postgres user for the application",
          configVarConvert = pure
        }
  pgPassword <-
    runConfigVar $
      ConfigVar
        { configVarName = "STRAW_POLL_PG_PASSWORD",
          configVarDescription = "Postgres password for the application",
          configVarConvert = pure
        }
  pure . Validation.toEither $
    Config
      <$> httpPort
      <*> pgHost
      <*> pgPort
      <*> pgUser
      <*> pgPassword
