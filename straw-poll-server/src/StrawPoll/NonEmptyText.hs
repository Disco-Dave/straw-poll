module StrawPoll.NonEmptyText
  ( NonEmptyText,
    make,
    toText,
  )
where

import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Text (Text)
import qualified Data.Text as Text

newtype NonEmptyText = NonEmptyText Text
  deriving (Eq, Ord)

make :: Text -> Maybe NonEmptyText
make text
  | Text.null text = Nothing
  | otherwise = Just $ NonEmptyText text

toText :: NonEmptyText -> Text
toText (NonEmptyText text) = text

instance Show NonEmptyText where
  show (NonEmptyText text) = show text

instance Semigroup NonEmptyText where
  (NonEmptyText t1) <> (NonEmptyText t2) =
    NonEmptyText $ t1 <> t2

instance ToJSON NonEmptyText where
  toJSON = toJSON . toText
  toEncoding = toEncoding . toText

instance FromJSON NonEmptyText where
  parseJSON value =
    fmap make (parseJSON value) >>= \case
      Nothing -> fail "Text must be non-empty."
      Just nonEmptyText -> pure nonEmptyText
