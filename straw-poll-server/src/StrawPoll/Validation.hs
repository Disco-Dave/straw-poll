module StrawPoll.Validation
  ( Validation (..),
    toValidation,
    toEither,
  )
where

data Validation err success
  = Failure !err
  | Success !success

toValidation :: Either err success -> Validation err success
toValidation = \case
  Left e -> Failure e
  Right r -> Success r

toEither :: Validation err success -> Either err success
toEither = \case
  Failure e -> Left e
  Success r -> Right r

instance Functor (Validation err) where
  fmap f = \case
    Failure err -> Failure err
    Success success -> Success $ f success

instance Semigroup err => Applicative (Validation err) where
  pure = Success
  validationFunction <*> validationValue =
    case (validationFunction, validationValue) of
      (Failure err1, Failure err2) -> Failure $ err1 <> err2
      (Failure err, _) -> Failure err
      (_, Failure err) -> Failure err
      (Success f, Success v) -> Success $ f v
