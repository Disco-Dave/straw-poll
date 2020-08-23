module StrawPoll.Config
  ( Config (..),
    ConfigErrorReason (..),
    ConfigError (..),
    ConfigErrors (..),
    getConfig,
  )
where

import Control.Exception (Exception)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as Char8
import Data.Foldable (toList)
import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty (..))
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
  { configConnectionString :: !ByteString,
    configHttpPort :: !Port
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
  connectionString <-
    runConfigVar $
      ConfigVar
        { configVarName = "STRAW_POLL_PG",
          configVarDescription = "Libpq connection string for postgres database",
          configVarConvert = Just . Char8.pack
        }
  port <-
    runConfigVar $
      ConfigVar
        { configVarName = "STRAW_POLL_HTTP_PORT",
          configVarDescription = "Port to run the http server on",
          configVarConvert = readMaybe
        }
  pure . Validation.toEither $
    Config
      <$> connectionString
      <*> port
