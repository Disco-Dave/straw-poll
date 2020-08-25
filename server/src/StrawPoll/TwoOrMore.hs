module StrawPoll.TwoOrMore
  ( TwoOrMore,
    make,
    toList,
  )
where

import Data.Aeson (ToJSON (..))

newtype TwoOrMore a = TwoOrMore {fromTwoOrMore :: [a]}
  deriving (Eq, Ord)

instance Show a => Show (TwoOrMore a) where
  show (TwoOrMore xs) = show xs

make :: a -> a -> [a] -> TwoOrMore a
make first second rest = TwoOrMore $ first : second : rest

toList :: TwoOrMore a -> [a]
toList = fromTwoOrMore

instance Functor TwoOrMore where
  fmap f = TwoOrMore . fmap f . fromTwoOrMore

instance Foldable TwoOrMore where
  foldMap f = foldMap f . fromTwoOrMore

instance Traversable TwoOrMore where
  traverse f = fmap TwoOrMore . traverse f . fromTwoOrMore

instance ToJSON a => ToJSON (TwoOrMore a) where
  toJSON = toJSON . fromTwoOrMore
  toEncoding = toEncoding . fromTwoOrMore
