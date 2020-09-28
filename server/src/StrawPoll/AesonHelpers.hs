module StrawPoll.AesonHelpers
  ( fieldLabelModifier,
    options,
  )
where

import qualified Data.Aeson as Aeson
import Data.List (stripPrefix)

fieldLabelModifier :: String -> String -> String
fieldLabelModifier prefix fieldLabel =
  case stripPrefix prefix fieldLabel of
    Just (l : rest) -> toLower l : rest
    _ -> fieldLabel

options :: String -> Aeson.Options
options prefix =
  Aeson.defaultOptions
    { Aeson.fieldLabelModifier = fieldLabelModifier prefix,
      Aeson.omitNothingFields = True,
      Aeson.unwrapUnaryRecords = True
    }
