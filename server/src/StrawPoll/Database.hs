{-# LANGUAGE QuasiQuotes #-}

module StrawPoll.Database
  ( createPool,
    savePoll,
    saveVote,
    findPoll,
  )
where

import Data.Function ((&))
import Data.Functor (void, (<&>))
import qualified Data.Map.Strict as Map
import Data.Pool (Pool)
import qualified Data.Pool as Pool
import Data.Traversable (for)
import qualified Database.PostgreSQL.Simple as Postgres
import Database.PostgreSQL.Simple.SqlQQ (sql)
import qualified StrawPoll.NonEmptyText as NonEmptyText
import StrawPoll.Poll (Answer (..), Id (..), Poll (..), UnsavedPoll (..))
import qualified StrawPoll.TwoOrMore as TwoOrMore

createPool :: Postgres.ConnectInfo -> IO (Pool Postgres.Connection)
createPool connectInfo =
  let create = Postgres.connect connectInfo
      destroy = Postgres.close
      idleTime = 5
      stripeCount = 5
      resourcePerStripe = 10
   in Pool.createPool create destroy stripeCount idleTime resourcePerStripe

savePoll :: Postgres.Connection -> UnsavedPoll -> IO Poll
savePoll connection UnsavedPoll {..} = Postgres.withTransaction connection $ do
  [Id . Postgres.fromOnly -> pollId] <-
    let insertPollSql =
          [sql|
            INSERT INTO public.polls (question, expiration) 
            VALUES (?, ?)
            RETURNING id; |]
        insertPollParams =
          (NonEmptyText.toText unsavedPollQuestion, unsavedPollExpiration)
     in Postgres.query connection insertPollSql insertPollParams

  _ <-
    let insertAnswersSql =
          [sql|
            INSERT INTO public.answers (poll_id, answer)
            VALUES (?, ?); |]
        insertAnswersParams =
          fmap (\a -> (fromId pollId, NonEmptyText.toText a)) unsavedPollAnswers
            & TwoOrMore.toList
     in Postgres.executeMany connection insertAnswersSql insertAnswersParams

  answerResults <-
    let selectAnswersSql =
          [sql| 
            SELECT a.id, a.answer
            FROM public.answers AS a 
            WHERE a.poll_id = ?
            ORDER BY a.id; |]
     in Postgres.query connection selectAnswersSql [fromId pollId]

  (firstAnswer : secondAnswer : otherAnswers) <-
    let answerMap =
          Map.fromList . TwoOrMore.toList $ (unsavedPollAnswers <&> \a -> (NonEmptyText.toText a, a))
     in pure $
          answerResults <&> \(answerId, answerText) ->
            Answer
              { answerId = Id answerId,
                answerText = answerMap Map.! answerText,
                answerVotes = 0
              }
  pure $
    Poll
      { pollId = pollId,
        pollQuestion = unsavedPollQuestion,
        pollExpiration = unsavedPollExpiration,
        pollAnswers = TwoOrMore.make firstAnswer secondAnswer otherAnswers
      }

findPoll :: Postgres.Connection -> Id Poll -> IO (Maybe Poll)
findPoll connection pollId = do
  answersQueryResult <-
    let query =
          [sql| 
            SELECT a.id, a.answer, COUNT(v.*)
            FROM public.answers AS a 
              LEFT OUTER JOIN public.votes AS v
                ON v.answer_id = a.id
                AND v.poll_id = a.poll_id
            WHERE a.poll_id = ?
            GROUP BY a.id, a.answer
            ORDER BY a.id; |]
     in Postgres.query connection query [fromId pollId]
  pollQueryResult <-
    let query =
          [sql| 
            SELECT p.question, p.expiration 
            FROM public.polls AS p 
            WHERE p.id = ?; |]
     in Postgres.query connection query [fromId pollId]
  pure $ do
    [(question, pollExpiration)] <- pure pollQueryResult
    pollQuestion <- NonEmptyText.make question
    (firstAnswer : secondAnswer : otherAnswers) <- for answersQueryResult $
      \(answerId, answerText, answerVotes) ->
        Answer (Id answerId)
          <$> NonEmptyText.make answerText
          <*> pure (fromIntegral @Integer answerVotes)
    let pollAnswers = TwoOrMore.make firstAnswer secondAnswer otherAnswers
    pure Poll {..}

saveVote :: Postgres.Connection -> Id Poll -> Id Answer -> IO ()
saveVote connection pollId answerId =
  let query =
        [sql|
          INSERT INTO public.votes (poll_id, answer_id)
          VALUES (?, ?); |]
      params = (fromId pollId, fromId answerId)
   in void $ Postgres.execute connection query params
