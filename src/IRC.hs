module IRC 
  ( IRC
  , silent, send
  , ctcp
  , reply, tryReply
  , request, json
  , bold, italic
  ) where

import ClassyPrelude

import qualified Data.Aeson as Aeson
import qualified Data.CaseInsensitive as CI
import qualified Conduit
import Conduit ((.|))
import qualified Text.HTML.DOM as DOM
import qualified Network.HTTP.Conduit as HTTP
import qualified Network.HTTP.Simple as HTTP
import qualified Text.XML.Cursor as XML

import qualified Context
import Context (Context(..))
import qualified Env
import Env (ENV)

type IRC = ReaderT Context ENV

silent :: Text -> Text -> IRC ()
silent command message = lift $ Env.silent command message

send :: Text -> Text -> IRC ()
send command message = lift $ Env.send command message

respond :: Text -> IRC ()
respond message = do
    target <- asks Context.target
    send "PRIVMSG" $ target ++ " " ++ message

ctcp :: Text -> Text -> IRC ()
ctcp command message = respond $ "\SOH" ++ command ++ " " ++ message ++ "\SOH"

reply :: Text -> IRC ()
reply s = do
    nick <- CI.original <$> asks Context.nick
    respond $ nick ++ ": " ++ s

tryReply :: Maybe Text -> IRC ()
tryReply = reply . fromMaybe "I'm sorry, I couldn't find anything."

json :: Text -> IRC Aeson.Value
json url = do
    web <- lift $ asks Env.web
    req <- liftIO . HTTP.parseRequest $ unpack url
    res <- HTTP.httpJSON $ HTTP.setRequestManager web req
    return $ HTTP.getResponseBody res

request :: Text -> IRC XML.Cursor
request url = XML.fromDocument <$> do
    web <- lift $ asks Env.web
    req <- liftIO . HTTP.parseRequest $ unpack url
    Conduit.runResourceT $ do
        res <- HTTP.http req web
        Conduit.runConduit $ HTTP.responseBody res .| DOM.sinkDoc

bold :: Text -> Text
bold = ("\x02" ++) . (++ "\x02")

italic :: Text -> Text
italic = ("\x1d" ++ ) . (++ "\x1d")
