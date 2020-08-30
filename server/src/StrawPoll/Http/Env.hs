{-# LANGUAGE TemplateHaskell #-}

module StrawPoll.Http.Env where

import Control.Monad.Reader (local)
import StrawPoll.Poll (Id (..), Poll, SavePoll, SaveVote)
import Yesod.Core
import Yesod.Core.Types (HandlerData (..))

data Env = Env
  { envSavePoll :: SavePoll IO,
    envSaveVote :: SaveVote IO,
    envFindPoll :: Id Poll -> IO (Maybe Poll)
  }

instance Yesod Env where
  makeSessionBackend _ = pure Nothing
  errorHandler err =
    let useJsonRep handlerData@HandlerData {handlerRequest} =
          handlerData
            { handlerRequest =
                handlerRequest {reqAccept = ["application/json"]}
            }
     in local useJsonRep $ defaultErrorHandler err

mkYesodData "Env" $(parseRoutesFile "src/StrawPoll/Http/Routes")
