{-# LANGUAGE QuasiQuotes #-}

module StrawPoll.HttpSpec (spec) where

import Data.Time
import Shared (savePoll, unsafeNonEmptyText)
import StrawPoll.Http (Env (..), application)
import StrawPoll.Poll (Answer (..), Id (..), Poll (..))
import qualified StrawPoll.TwoOrMore as TwoOrMore
import Test.Hspec
import qualified Test.Hspec.Wai as HspecWai
import qualified Test.Hspec.Wai.Internal as HspecWaiInternal
import Test.Hspec.Wai.JSON (json)

httpEnv :: Env
httpEnv =
  Env
    { envSavePoll = pure . savePoll,
      envSaveVote = \_ _ -> pure (),
      envFindPoll = \_ -> pure Nothing
    }

withApplication :: Env -> HspecWai.WaiSession () a -> IO a
withApplication env =
  HspecWaiInternal.withApplication $ application env

spec :: Spec
spec = do
  it "responds with 404 for unknown path" $
    withApplication httpEnv $
      HspecWai.get "/unknown" `HspecWai.shouldRespondWith` 404

  describe "POST /polls" $ do
    it "responds with 400 if body is empty" $
      withApplication httpEnv $
        HspecWai.post "/polls" mempty `HspecWai.shouldRespondWith` 400

    it "responds with 400 if body is not valid CreatePollRequest json" $
      withApplication httpEnv $
        HspecWai.post "/polls" [json| "some garbage" |] `HspecWai.shouldRespondWith` 400

    it "responds with 400 if there are errors" $
      withApplication httpEnv $
        let request =
              [json| 
                { 
                  "question": "Should you answer yes or no?",
                  "expiration": "2000-08-23T15:14:35.536505Z",
                  "answers": [
                    "Yes",
                    "No",
                    "Not sure"
                  ]
                } |]
            response =
              [json|  
                {
                  "expiration": "Expiration may not be less than or equal to today."
                } |]
         in HspecWai.post "/polls" request `HspecWai.shouldRespondWith` response {HspecWai.matchStatus = 400}

    it "responds with 201 and created poll in response body if successful" $
      withApplication httpEnv $
        let request =
              [json| 
                { 
                  "question": "Should you answer yes or no?",
                  "answers": [
                    "Yes",
                    "No",
                    "Not sure"
                  ]
                } |]
            response =
              [json|  
                {
                  "id": 11,
                  "question": "Should you answer yes or no?",
                  "answers": [
                    {
                      "id": 1,
                      "text": "Yes",
                      "votes": 0
                    },
                    {
                      "id": 2,
                      "text": "No",
                      "votes": 0
                    },
                    {
                      "id": 3,
                      "text": "Not sure",
                      "votes": 0
                    }
                  ]
                } |]
         in HspecWai.post "/polls" request `HspecWai.shouldRespondWith` response {HspecWai.matchStatus = 201}

  describe "POST /polls/:pollId/votes/:answerId" $ do
    it "responds with 404 if poll id is invalid" $
      withApplication httpEnv $
        HspecWai.post "/polls/abc/votes/1" mempty `HspecWai.shouldRespondWith` 404

    it "responds with 404 if answer id is invalid" $
      withApplication httpEnv $
        HspecWai.post "/polls/1/votes/abc" mempty `HspecWai.shouldRespondWith` 404

    it "responds with 404 if poll is not found" $
      withApplication httpEnv $
        HspecWai.post "/polls/1/votes/1" mempty `HspecWai.shouldRespondWith` [json| "Cannot find requested poll." |] {HspecWai.matchStatus = 404}

    it "responds with 404 if answer is not found" $
      let poll =
            Poll
              { pollId = Id 1,
                pollQuestion = unsafeNonEmptyText "Is this an example question?",
                pollExpiration = Nothing,
                pollAnswers =
                  TwoOrMore.make
                    (Answer (Id 1) (unsafeNonEmptyText "Yes") 0)
                    (Answer (Id 2) (unsafeNonEmptyText "No") 0)
                    []
              }
          localEnv = httpEnv {envFindPoll = \_ -> pure $ Just poll}
       in withApplication localEnv $
            HspecWai.post "/polls/1/votes/3" mempty `HspecWai.shouldRespondWith` [json| "Cannot find requested answer." |] {HspecWai.matchStatus = 404}

    it "responds with 400 if poll is expired" $ do
      expiration <- utcToLocalZonedTime =<< addUTCTime (-1 * nominalDay) <$> getCurrentTime
      let poll =
            Poll
              { pollId = Id 1,
                pollQuestion = unsafeNonEmptyText "Is this an example question?",
                pollExpiration = Just expiration,
                pollAnswers =
                  TwoOrMore.make
                    (Answer (Id 1) (unsafeNonEmptyText "Yes") 0)
                    (Answer (Id 2) (unsafeNonEmptyText "No") 0)
                    []
              }
          localEnv = httpEnv {envFindPoll = \_ -> pure $ Just poll}
       in withApplication localEnv $
            HspecWai.post "/polls/1/votes/1" mempty `HspecWai.shouldRespondWith` [json| "Poll is expired." |] {HspecWai.matchStatus = 400}

    it "responds with 200 and updated poll if vote was accepted" $
      let poll =
            Poll
              { pollId = Id 1,
                pollQuestion = unsafeNonEmptyText "Is this an example question?",
                pollExpiration = Nothing,
                pollAnswers =
                  TwoOrMore.make
                    (Answer (Id 1) (unsafeNonEmptyText "Yes") 0)
                    (Answer (Id 2) (unsafeNonEmptyText "No") 0)
                    []
              }
          localEnv = httpEnv {envFindPoll = \_ -> pure $ Just poll}
          response =
            [json|  
              {
                "id": 1,
                "question": "Is this an example question?",
                "answers": [
                  {
                    "id": 1,
                    "text": "Yes",
                    "votes": 1
                  },
                  {
                    "id": 2,
                    "text": "No",
                    "votes": 0
                  }
                ]
              } |]
       in withApplication localEnv $
            HspecWai.post "/polls/1/votes/1" mempty `HspecWai.shouldRespondWith` response {HspecWai.matchStatus = 200}

  describe "GET /polls/:pollId" $ do
    it "responds with 404 if pollId is invalid" $
      withApplication httpEnv $
        HspecWai.get "/polls/abc" `HspecWai.shouldRespondWith` 404

    it "responds with 404 if poll is not found" $
      withApplication httpEnv $
        HspecWai.get "/polls/1" `HspecWai.shouldRespondWith` 404

    it "responds with 200 if poll is found and poll in body" $
      let poll =
            Poll
              { pollId = Id 1,
                pollQuestion = unsafeNonEmptyText "Is this an example question?",
                pollExpiration = Nothing,
                pollAnswers =
                  TwoOrMore.make
                    (Answer (Id 1) (unsafeNonEmptyText "Yes") 0)
                    (Answer (Id 2) (unsafeNonEmptyText "No") 0)
                    []
              }
          localEnv = httpEnv {envFindPoll = \_ -> pure $ Just poll}
          response =
            [json|  
              {
                "id": 1,
                "question": "Is this an example question?",
                "answers": [
                  {
                    "id": 1,
                    "text": "Yes",
                    "votes": 0
                  },
                  {
                    "id": 2,
                    "text": "No",
                    "votes": 0
                  }
                ]
              } |]
       in withApplication localEnv $
            HspecWai.get "/polls/1" `HspecWai.shouldRespondWith` response {HspecWai.matchStatus = 200}
