module IRC 
  ( IRC
  , silent, send
  , ctcp
  , reply, tryReply
  , request
  ) where

import ClassyPrelude

import qualified Conduit
import Conduit ((.|))
import qualified Text.HTML.DOM as DOM
import qualified Network.HTTP.Conduit as HTTP
import qualified Data.Text as Text
import qualified Text.XML.Cursor as XML

import qualified Context
import Context (Context)
import qualified Env
import Env (ENV)

type IRC = ReaderT Context ENV

silent :: Text -> Text -> IRC ()
silent command message = lift $ Env.silent command message

send :: Text -> Text -> IRC ()
send command message = lift $ Env.send command message

respond :: Text -> IRC ()
respond message = do
    ctx <- ask
    case Text.head $ Context.channel ctx of
      '#' -> send "PRIVMSG" $ Context.channel ctx ++ " " ++ message
      _   -> send "NOTICE"  $ Context.nick ctx ++ " " ++ message

ctcp :: Text -> Text -> IRC ()
ctcp command message = respond $ "\SOH" ++ command ++ " " ++ message ++ "\SOH"

reply :: Text -> IRC ()
reply s = do
    nick <- asks Context.nick
    respond $ nick ++ ": " ++ s

tryReply :: Maybe Text -> IRC ()
tryReply = reply . fromMaybe "I'm sorry, I couldn't find anything."

request :: Text -> IRC XML.Cursor
request url = XML.fromDocument <$> do
    web <- lift $ asks Env.web
    req <- liftIO . HTTP.parseRequest $ unpack url
    Conduit.runResourceT $ do
        response <- HTTP.http req web
        Conduit.runConduit $ HTTP.responseBody response .| DOM.sinkDoc
