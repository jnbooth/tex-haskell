module IRC 
  ( Socketed(..)
  , Bot(..), BotIO
  , Context(..), IRC
  , silent, send, ctcp, reply, tryReply
  , get
  ) where

import ClassyPrelude hiding (handle)

import qualified Conduit
import qualified Text.HTML.DOM as DOM
import qualified Data.Maybe as Maybe
import qualified Network.HTTP.Conduit as HTTP
import qualified System.IO as IO
import qualified Control.Monad.Trans.Reader as Reader
import qualified STMContainers.Map as Stm
import qualified Database.Persist.Postgresql as Sql
import qualified Data.Text as Text
import qualified Text.XML.Cursor as XML

import Conduit ((.|))
import Data.Pool (Pool)

import Persist

class Socketed m where
  getSocket :: m -> Handle

data Bot = Bot 
    { botSocket :: !Handle
    , botWeb    :: !HTTP.Manager
    , botPool   :: !(Pool Sql.SqlBackend)
    , botNick   :: !Text
    , botOwner  :: !(Maybe Text)
    , botUsers  :: !(Stm.Map Text User)
    }
instance Socketed Bot where 
  getSocket = botSocket

type BotIO = Reader.ReaderT Bot IO

data Context = Context
    { ctxBot     :: !Bot
    , ctxChannel :: !Text
    , ctxUser    :: !User
    }
instance Socketed Context where 
  getSocket = getSocket . ctxBot

type IRC = Reader.ReaderT Context BotIO

silent :: ∀ a m. (MonadIO m, Socketed a) => Text -> Text -> Reader.ReaderT a m ()
silent command message = do
    socket <- Reader.asks getSocket
    liftIO . IO.hPutStrLn socket . unpack $ command ++ " " ++ message

send :: ∀ a m. (MonadIO m, Socketed a) => Text -> Text -> Reader.ReaderT a m ()
send command message = do
    silent command message
    putStrLn $ "\x1b[32m> " ++ command ++ " " ++ message ++ "\x1b[0m"

respond :: Text -> Text -> IRC ()
respond command message = do
    Context{..} <- Reader.ask
    let User{..} = ctxUser
    case Text.head ctxChannel of
        '#' -> send "PRIVMSG" $ ctxChannel ++ " " ++ message
        _   -> send command $ userNick ++ " " ++ message

ctcp :: Text -> Text -> IRC ()
ctcp command message = 
    respond "PRIVMSG" $ "\SOH" ++ command ++ " " ++ message ++ "\SOH"

reply :: Text -> IRC ()
reply s = do
    userNick <- Reader.asks $ userNick . ctxUser
    respond "NOTICE" $ userNick ++ ": " ++ s

tryReply :: Maybe Text -> IRC ()
tryReply = reply . Maybe.fromMaybe "I'm sorry, I couldn't find anything."

get :: Text -> IRC XML.Cursor
get url = XML.fromDocument <$> do
    botWeb <- Reader.asks $ botWeb . ctxBot
    request <- liftIO . HTTP.parseRequest $ unpack url
    Conduit.runResourceT $ do
        response <- HTTP.http request botWeb
        Conduit.runConduit $ HTTP.responseBody response .| DOM.sinkDoc
