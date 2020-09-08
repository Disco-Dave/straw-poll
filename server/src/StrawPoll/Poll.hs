module StrawPoll.Poll
  ( Id (..),
    Answer (..),
    Poll (..),
    CreatePollRequest (..),
    CreatePollRequestErrors (..),
    UnsavedPoll (..),
    SavePoll,
    createPoll,
    SaveVote,
    vote,
    VoteResult (..),
  )
where

import Data.Aeson (FromJSON (..), ToJSON (..), genericParseJSON, genericToEncoding, genericToJSON)
import Data.Functor (($>))
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import Numeric.Natural (Natural)
import StrawPoll.AesonHelpers (options)
import StrawPoll.NonEmptyText (NonEmptyText)
import qualified StrawPoll.NonEmptyText as NonEmptyText
import StrawPoll.TwoOrMore (TwoOrMore)
import qualified StrawPoll.TwoOrMore as TwoOrMore
import StrawPoll.Validation (Validation (..))
import qualified StrawPoll.Validation as Validation

newtype Id entity = Id {fromId :: Integer}
  deriving (Show, Eq, Generic)

instance ToJSON (Id entity) where
  toJSON = toJSON . fromId
  toEncoding = toEncoding . fromId

instance FromJSON (Id entity) where
  parseJSON = fmap Id . parseJSON

data Answer = Answer
  { answerId :: !(Id Answer),
    answerText :: !NonEmptyText,
    answerVotes :: !Natural
  }
  deriving (Show, Eq, Generic)

instance ToJSON Answer where
  toJSON = genericToJSON $ options "answer"
  toEncoding = genericToEncoding $ options "answer"

data Poll = Poll
  { pollId :: !(Id Poll),
    pollQuestion :: !NonEmptyText,
    pollAnswers :: !(TwoOrMore Answer),
    pollExpiration :: !(Maybe UTCTime)
  }
  deriving (Show, Generic, Eq)

instance ToJSON Poll where
  toJSON = genericToJSON $ options "poll"
  toEncoding = genericToEncoding $ options "poll"

data CreatePollRequest = CreatePollRequest
  { createPollRequestQuestion :: !Text,
    createPollRequestAnswers :: !([Text]),
    createPollRequestExpiration :: !(Maybe UTCTime)
  }
  deriving (Show, Generic, Eq)

instance FromJSON CreatePollRequest where
  parseJSON = genericParseJSON $ options "createPollRequest"

data CreatePollRequestErrors = CreatePollRequestErrors
  { createPollRequestErrorsQuestion :: !(Maybe Text),
    createPollRequestErrorsAnswers :: !(Maybe Text),
    createPollRequestErrorsExpiration :: !(Maybe Text)
  }
  deriving (Show, Eq, Generic)

instance Semigroup CreatePollRequestErrors where
  err1 <> err2 =
    CreatePollRequestErrors
      { createPollRequestErrorsQuestion =
          createPollRequestErrorsQuestion err1 <> createPollRequestErrorsQuestion err2,
        createPollRequestErrorsAnswers =
          createPollRequestErrorsAnswers err1 <> createPollRequestErrorsAnswers err2,
        createPollRequestErrorsExpiration =
          createPollRequestErrorsExpiration err1 <> createPollRequestErrorsExpiration err2
      }

instance Monoid CreatePollRequestErrors where
  mempty = CreatePollRequestErrors Nothing Nothing Nothing

instance ToJSON CreatePollRequestErrors where
  toJSON = genericToJSON $ options "createPollRequestErrors"
  toEncoding = genericToEncoding $ options "createPollRequestErrors"

data UnsavedPoll = UnsavedPoll
  { unsavedPollQuestion :: !NonEmptyText,
    unsavedPollAnswers :: !(TwoOrMore NonEmptyText),
    unsavedPollExpiration :: !(Maybe UTCTime)
  }
  deriving (Show, Eq)

validateCreatePollRequest :: UTCTime -> CreatePollRequest -> Either CreatePollRequestErrors UnsavedPoll
validateCreatePollRequest currentTime CreatePollRequest {..} =
  let question =
        case NonEmptyText.make $ Text.strip createPollRequestQuestion of
          Just nonEmptyQuestion ->
            Success nonEmptyQuestion
          Nothing ->
            let err = Just "Question may not be empty."
             in Failure mempty {createPollRequestErrorsQuestion = err}
      answers =
        let makeAnswer = NonEmptyText.make . Text.strip
         in case mapMaybe makeAnswer createPollRequestAnswers of
              (firstAnswer : secondAnswer : otherAnswers) ->
                Success $ TwoOrMore.make firstAnswer secondAnswer otherAnswers
              _ ->
                let err = Just "List of answers may not be less than two."
                 in Failure mempty {createPollRequestErrorsAnswers = err}
      expiration =
        case createPollRequestExpiration of
          Nothing -> Success Nothing
          Just expirationTime
            | expirationTime > currentTime ->
              Success $ Just expirationTime
            | otherwise ->
              let err = Just "Expiration may not be less than or equal to today."
               in Failure mempty {createPollRequestErrorsExpiration = err}
   in Validation.toEither $ UnsavedPoll <$> question <*> answers <*> expiration

type SavePoll m = UnsavedPoll -> m Poll

createPoll :: Applicative m => SavePoll m -> UTCTime -> CreatePollRequest -> m (Either CreatePollRequestErrors Poll)
createPoll savePoll currentTime pollRequest =
  case validateCreatePollRequest currentTime pollRequest of
    Left errors ->
      pure $ Left errors
    Right unsavedPoll ->
      Right <$> savePoll unsavedPoll

data VoteResult
  = PollIsExpired
  | AnswerNotFound
  | VoteAccepted !Poll
  deriving (Show, Eq)

type SaveVote m = Id Poll -> Id Answer -> m ()

vote :: Applicative m => SaveVote m -> UTCTime -> Poll -> Id Answer -> m VoteResult
vote saveVote currentTime poll@Poll {..} answer
  | answerIsNotFound = pure AnswerNotFound
  | isExpired = pure PollIsExpired
  | otherwise =
    let increaseCount a@Answer {answerId, answerVotes}
          | answerId == answer = a {answerVotes = succ answerVotes}
          | otherwise = a
        newPoll = poll {pollAnswers = fmap increaseCount pollAnswers}
     in saveVote pollId answer $> VoteAccepted newPoll
  where
    answerIsNotFound =
      let isRequestedAnswer Answer {answerId} =
            answerId == answer
       in not $ any isRequestedAnswer pollAnswers
    isExpired =
      case pollExpiration of
        Just expiration
          | expiration <= currentTime -> True
        _ -> False
