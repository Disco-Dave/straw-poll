{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module StrawPoll.Http
  ( Env (..),
    application,
    start,
  )
where

import Data.Function ((&))
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import StrawPoll.Http.Env
import StrawPoll.Http.Handlers
import Yesod.Core

mkYesodDispatch "Env" $(parseRoutesFile "src/StrawPoll/Http/Routes")

application :: Env -> IO Wai.Application
application env = toWaiAppPlain env

start :: Env -> Warp.Port -> IO ()
start env port =
  let settings =
        let printMessage =
              let portText = Text.pack $ show port
                  message = "HTTP server started on port " <> portText
               in TextIO.putStrLn message
         in Warp.defaultSettings
              & Warp.setPort port
              & Warp.setBeforeMainLoop printMessage
   in application env >>= Warp.runSettings settings
