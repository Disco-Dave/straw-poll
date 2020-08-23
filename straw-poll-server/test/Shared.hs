module Shared where

import Control.Monad.ST (runST)
import Data.STRef (modifySTRef', newSTRef, readSTRef)
import Data.Text (Text)
import Data.Traversable (for)
import StrawPoll.NonEmptyText (NonEmptyText)
import qualified StrawPoll.NonEmptyText as NonEmptyText
import StrawPoll.Poll
  ( Answer (..),
    Id (..),
    Poll (..),
    UnsavedPoll (..),
  )

unsafeNonEmptyText :: Text -> NonEmptyText
unsafeNonEmptyText text =
  case NonEmptyText.make text of
    Just a -> a
    Nothing -> error "Make sure you only pass non-empty text to me."

savePoll :: UnsavedPoll -> Poll
savePoll UnsavedPoll {..} =
  Poll
    { pollId = Id 11,
      pollQuestion = unsavedPollQuestion,
      pollExpiration = unsavedPollExpiration,
      pollAnswers = runST $ do
        idRef <- newSTRef 1
        for unsavedPollAnswers $ \answerText -> do
          answerId <- Id <$> readSTRef idRef
          modifySTRef' idRef succ
          pure $
            Answer
              { answerId = answerId,
                answerText = answerText,
                answerVotes = 0
              }
    }
