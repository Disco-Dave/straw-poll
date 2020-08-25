module StrawPoll.PollSpec (spec) where

import Data.Functor.Identity (Identity (..))
import Data.Time (UTCTime, addUTCTime, getCurrentTime, nominalDay, utcToLocalZonedTime)
import Shared (savePoll, unsafeNonEmptyText)
import StrawPoll.Poll
  ( Answer (..),
    CreatePollRequest (..),
    CreatePollRequestErrors (..),
    Id (..),
    Poll (..),
    VoteResult (..),
  )
import qualified StrawPoll.Poll as Poll
import qualified StrawPoll.TwoOrMore as TwoOrMore
import Test.Hspec (Spec, describe, it, runIO, shouldBe)

createPoll :: UTCTime -> CreatePollRequest -> Either CreatePollRequestErrors Poll
createPoll currentTime =
  runIdentity . Poll.createPoll (Identity . savePoll) currentTime

vote :: UTCTime -> Poll -> Id Answer -> VoteResult
vote currentTime poll =
  runIdentity . Poll.vote (\_ _ -> pure ()) currentTime poll

spec :: Spec
spec = do
  currentTime <- runIO getCurrentTime
  describe "createPoll" $ do
    it "does not allow empty question" $ do
      let request =
            CreatePollRequest
              { createPollRequestQuestion = "",
                createPollRequestExpiration = Nothing,
                createPollRequestAnswers = ["Yes", "No"]
              }
          expectedResponse =
            Left $
              CreatePollRequestErrors
                { createPollRequestErrorsQuestion = Just "Question may not be empty.",
                  createPollRequestErrorsAnswers = Nothing,
                  createPollRequestErrorsExpiration = Nothing
                }
       in createPoll currentTime request `shouldBe` expectedResponse

    it "does not allow a question of all whitespace" $
      let request =
            CreatePollRequest
              { createPollRequestQuestion = "     ",
                createPollRequestExpiration = Nothing,
                createPollRequestAnswers = ["Yes", "No"]
              }
          expectedResponse =
            Left $
              CreatePollRequestErrors
                { createPollRequestErrorsQuestion = Just "Question may not be empty.",
                  createPollRequestErrorsAnswers = Nothing,
                  createPollRequestErrorsExpiration = Nothing
                }
       in createPoll currentTime request `shouldBe` expectedResponse

    it "filters out empty answers" $
      let request =
            CreatePollRequest
              { createPollRequestQuestion = "Example",
                createPollRequestExpiration = Nothing,
                createPollRequestAnswers = ["", "Yes", "", "No", ""]
              }
          expectedResponse =
            Right
              Poll
                { pollId = Id 11,
                  pollQuestion = unsafeNonEmptyText "Example",
                  pollExpiration = Nothing,
                  pollAnswers =
                    TwoOrMore.make
                      (Answer (Id 1) (unsafeNonEmptyText "Yes") 0)
                      (Answer (Id 2) (unsafeNonEmptyText "No") 0)
                      []
                }
       in createPoll currentTime request `shouldBe` expectedResponse

    it "filters out answers that are all whitespace" $
      let request =
            CreatePollRequest
              { createPollRequestQuestion = "Example",
                createPollRequestExpiration = Nothing,
                createPollRequestAnswers = ["   ", "Yes", "\t", "No", "   \n "]
              }
          expectedResponse =
            Right
              Poll
                { pollId = Id 11,
                  pollQuestion = unsafeNonEmptyText "Example",
                  pollExpiration = Nothing,
                  pollAnswers =
                    TwoOrMore.make
                      (Answer (Id 1) (unsafeNonEmptyText "Yes") 0)
                      (Answer (Id 2) (unsafeNonEmptyText "No") 0)
                      []
                }
       in createPoll currentTime request `shouldBe` expectedResponse

    it "does not allow a list of empty of answers" $
      let request =
            CreatePollRequest
              { createPollRequestQuestion = "Some example",
                createPollRequestExpiration = Nothing,
                createPollRequestAnswers = []
              }
          expectedResponse =
            Left $
              CreatePollRequestErrors
                { createPollRequestErrorsQuestion = Nothing,
                  createPollRequestErrorsAnswers = Just "List of answers may not be less than two.",
                  createPollRequestErrorsExpiration = Nothing
                }
       in createPoll currentTime request `shouldBe` expectedResponse

    it "does not allow an empty list of answers" $
      let request =
            CreatePollRequest
              { createPollRequestQuestion = "Some example",
                createPollRequestExpiration = Nothing,
                createPollRequestAnswers = ["", "", ""]
              }
          expectedResponse =
            Left $
              CreatePollRequestErrors
                { createPollRequestErrorsQuestion = Nothing,
                  createPollRequestErrorsAnswers = Just "List of answers may not be less than two.",
                  createPollRequestErrorsExpiration = Nothing
                }
       in createPoll currentTime request `shouldBe` expectedResponse

    it "does not allow a list with only one answer" $
      let request =
            CreatePollRequest
              { createPollRequestQuestion = "Some example",
                createPollRequestExpiration = Nothing,
                createPollRequestAnswers = ["Yes"]
              }
          expectedResponse =
            Left $
              CreatePollRequestErrors
                { createPollRequestErrorsQuestion = Nothing,
                  createPollRequestErrorsAnswers = Just "List of answers may not be less than two.",
                  createPollRequestErrorsExpiration = Nothing
                }
       in createPoll currentTime request `shouldBe` expectedResponse

    it "does not allow expiration less than today" $ do
      expiration <- utcToLocalZonedTime $ addUTCTime (-1 * nominalDay) currentTime
      let request =
            CreatePollRequest
              { createPollRequestQuestion = "Some example",
                createPollRequestExpiration = Just expiration,
                createPollRequestAnswers = ["Yes", "No"]
              }
          expectedResponse =
            Left $
              CreatePollRequestErrors
                { createPollRequestErrorsQuestion = Nothing,
                  createPollRequestErrorsAnswers = Nothing,
                  createPollRequestErrorsExpiration = Just "Expiration may not be less than or equal to today."
                }
       in createPoll currentTime request `shouldBe` expectedResponse

    it "does not allow expiration equal to today" $ do
      expiration <- utcToLocalZonedTime currentTime
      let request =
            CreatePollRequest
              { createPollRequestQuestion = "Some example",
                createPollRequestExpiration = Just expiration,
                createPollRequestAnswers = ["Yes", "No"]
              }
          expectedResponse =
            Left $
              CreatePollRequestErrors
                { createPollRequestErrorsQuestion = Nothing,
                  createPollRequestErrorsAnswers = Nothing,
                  createPollRequestErrorsExpiration = Just "Expiration may not be less than or equal to today."
                }
       in createPoll currentTime request `shouldBe` expectedResponse

    it "trims leading and trailing whitespace on questions" $
      let request =
            CreatePollRequest
              { createPollRequestQuestion = "   Example  ",
                createPollRequestExpiration = Nothing,
                createPollRequestAnswers = ["Yes", "No"]
              }
          expectedResponse =
            Right
              Poll
                { pollId = Id 11,
                  pollQuestion = unsafeNonEmptyText "Example",
                  pollExpiration = Nothing,
                  pollAnswers =
                    TwoOrMore.make
                      (Answer (Id 1) (unsafeNonEmptyText "Yes") 0)
                      (Answer (Id 2) (unsafeNonEmptyText "No") 0)
                      []
                }
       in createPoll currentTime request `shouldBe` expectedResponse

    it "trims leading and trailing whitespace on answer" $
      let request =
            CreatePollRequest
              { createPollRequestQuestion = "Example",
                createPollRequestExpiration = Nothing,
                createPollRequestAnswers = ["Yes\t", "  No   "]
              }
          expectedResponse =
            Right
              Poll
                { pollId = Id 11,
                  pollQuestion = unsafeNonEmptyText "Example",
                  pollExpiration = Nothing,
                  pollAnswers =
                    TwoOrMore.make
                      (Answer (Id 1) (unsafeNonEmptyText "Yes") 0)
                      (Answer (Id 2) (unsafeNonEmptyText "No") 0)
                      []
                }
       in createPoll currentTime request `shouldBe` expectedResponse

    it "all errors are reported" $ do
      expiration <- utcToLocalZonedTime currentTime
      let request =
            CreatePollRequest
              { createPollRequestQuestion = "",
                createPollRequestExpiration = Just expiration,
                createPollRequestAnswers = ["Yes"]
              }
          expectedResponse =
            Left $
              CreatePollRequestErrors
                { createPollRequestErrorsQuestion = Just "Question may not be empty.",
                  createPollRequestErrorsAnswers = Just "List of answers may not be less than two.",
                  createPollRequestErrorsExpiration = Just "Expiration may not be less than or equal to today."
                }
       in createPoll currentTime request `shouldBe` expectedResponse

    it "allows a poll to be made when all rules are satisfied" $ do
      expiration <- utcToLocalZonedTime $ addUTCTime nominalDay currentTime
      let request =
            CreatePollRequest
              { createPollRequestQuestion = "Is this even a useful test?",
                createPollRequestExpiration = Just expiration,
                createPollRequestAnswers = ["Yes\t", "  No   ", "Maybe", "Who cares?"]
              }
          expectedResponse =
            Right
              Poll
                { pollId = Id 11,
                  pollQuestion = unsafeNonEmptyText "Is this even a useful test?",
                  pollExpiration = Just expiration,
                  pollAnswers =
                    TwoOrMore.make
                      (Answer (Id 1) (unsafeNonEmptyText "Yes") 0)
                      (Answer (Id 2) (unsafeNonEmptyText "No") 0)
                      [ (Answer (Id 3) (unsafeNonEmptyText "Maybe") 0),
                        (Answer (Id 4) (unsafeNonEmptyText "Who cares?") 0)
                      ]
                }
       in createPoll currentTime request `shouldBe` expectedResponse

  describe "vote" $ do
    it "does not allow voting for an answer that does not exist for a given poll" $ do
      expiration <- utcToLocalZonedTime $ addUTCTime nominalDay currentTime
      let poll =
            Poll
              { pollId = Id 11,
                pollQuestion = unsafeNonEmptyText "Is this even a useful test?",
                pollExpiration = Just expiration,
                pollAnswers =
                  TwoOrMore.make
                    (Answer (Id 1) (unsafeNonEmptyText "Yes") 0)
                    (Answer (Id 2) (unsafeNonEmptyText "No") 0)
                    [ (Answer (Id 3) (unsafeNonEmptyText "Maybe") 0),
                      (Answer (Id 4) (unsafeNonEmptyText "Who cares?") 0)
                    ]
              }
       in vote currentTime poll (Id 6) `shouldBe` AnswerNotFound

    it "does not allow voting when the poll is expired" $ do
      expiration <- utcToLocalZonedTime $ addUTCTime (-1 * nominalDay) currentTime
      let poll =
            Poll
              { pollId = Id 11,
                pollQuestion = unsafeNonEmptyText "Is this even a useful test?",
                pollExpiration = Just expiration,
                pollAnswers =
                  TwoOrMore.make
                    (Answer (Id 1) (unsafeNonEmptyText "Yes") 0)
                    (Answer (Id 2) (unsafeNonEmptyText "No") 0)
                    [ (Answer (Id 3) (unsafeNonEmptyText "Maybe") 0),
                      (Answer (Id 4) (unsafeNonEmptyText "Who cares?") 0)
                    ]
              }
       in vote currentTime poll (Id 2) `shouldBe` PollIsExpired

    it "allows voting when the poll is not expired" $ do
      expiration <- utcToLocalZonedTime $ addUTCTime nominalDay currentTime
      let poll =
            Poll
              { pollId = Id 11,
                pollQuestion = unsafeNonEmptyText "Is this even a useful test?",
                pollExpiration = Just expiration,
                pollAnswers =
                  TwoOrMore.make
                    (Answer (Id 1) (unsafeNonEmptyText "Yes") 0)
                    (Answer (Id 2) (unsafeNonEmptyText "No") 0)
                    [ (Answer (Id 3) (unsafeNonEmptyText "Maybe") 0),
                      (Answer (Id 4) (unsafeNonEmptyText "Who cares?") 0)
                    ]
              }
          expectedPoll =
            Poll
              { pollId = Id 11,
                pollQuestion = unsafeNonEmptyText "Is this even a useful test?",
                pollExpiration = Just expiration,
                pollAnswers =
                  TwoOrMore.make
                    (Answer (Id 1) (unsafeNonEmptyText "Yes") 0)
                    (Answer (Id 2) (unsafeNonEmptyText "No") 1)
                    [ (Answer (Id 3) (unsafeNonEmptyText "Maybe") 0),
                      (Answer (Id 4) (unsafeNonEmptyText "Who cares?") 0)
                    ]
              }
       in vote currentTime poll (Id 2) `shouldBe` VoteAccepted expectedPoll

    it "increments the count correctly for the answer the user requested to vote for" $ do
      let poll =
            Poll
              { pollId = Id 11,
                pollQuestion = unsafeNonEmptyText "Is this even a useful test?",
                pollExpiration = Nothing,
                pollAnswers =
                  TwoOrMore.make
                    (Answer (Id 1) (unsafeNonEmptyText "Yes") 0)
                    (Answer (Id 2) (unsafeNonEmptyText "No") 0)
                    [ (Answer (Id 3) (unsafeNonEmptyText "Maybe") 0),
                      (Answer (Id 4) (unsafeNonEmptyText "Who cares?") 0)
                    ]
              }
          expectedPoll =
            Poll
              { pollId = Id 11,
                pollQuestion = unsafeNonEmptyText "Is this even a useful test?",
                pollExpiration = Nothing,
                pollAnswers =
                  TwoOrMore.make
                    (Answer (Id 1) (unsafeNonEmptyText "Yes") 0)
                    (Answer (Id 2) (unsafeNonEmptyText "No") 1)
                    [ (Answer (Id 3) (unsafeNonEmptyText "Maybe") 0),
                      (Answer (Id 4) (unsafeNonEmptyText "Who cares?") 0)
                    ]
              }
       in vote currentTime poll (Id 2) `shouldBe` VoteAccepted expectedPoll
