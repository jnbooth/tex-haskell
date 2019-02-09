module Env (Env(..), new, ENV, getEnv, silent, send) where

import ClassyPrelude

import qualified Data.Text.Encoding as Encoding
import qualified Network.HTTP.Conduit as HTTP
import qualified System.IO as IO
import qualified Network.Socket as Net
import Network.Socket (Socket)
import qualified Control.Monad.Trans.Reader as Reader
import qualified Database.PostgreSQL.Simple as SQL
import qualified System.Environment as SysEnv

data Env = Env
    { handle :: !Handle
    , web    :: !HTTP.Manager
    , sql    :: !SQL.Connection

    , nick   :: !Text
    , owner  :: !Text
    }

type ENV = ReaderT Env IO

getEnv :: ∀ m. MonadIO m => String -> m Text
getEnv = (pack <$>) . liftIO . SysEnv.getEnv

connect :: String -> Int -> IO Socket
connect url port = Net.withSocketsDo $ do
    addr:_ <- Net.getAddrInfo (Just hints) (Just url) (Just $ show port)
    socket <- Net.socket 
              (Net.addrFamily addr) 
              (Net.addrSocketType addr) 
              (Net.addrProtocol addr)
    Net.connect socket (Net.addrAddress addr)
    return socket
  where
    hints = Net.defaultHints { Net.addrSocketType = Net.Stream }

new :: IO Env
new = do
    server <- getEnv "IRC_SERVER"
    socket <- connect (unpack server) 6660
    handle <- Net.socketToHandle socket ReadWriteMode
    IO.hSetBuffering handle IO.NoBuffering
    web    <- HTTP.newManager HTTP.tlsManagerSettings
    dbUrl  <- getEnv "DATABASE_URL"
    sql    <- SQL.connectPostgreSQL $ Encoding.encodeUtf8 dbUrl
    nick   <- getEnv "IRC_NICK"
    owner  <- getEnv "OWNER"
    return Env{..}

silent :: Text -> Text -> ENV ()
silent command message = do
    handle <- Reader.asks Env.handle
    liftIO . IO.hPutStrLn handle . unpack $ command ++ " " ++ message

send :: Text -> Text -> ENV ()
send command message = do
    silent command message
    putStrLn $ "\x1b[32m> " ++ command ++ " " ++ message ++ "\x1b[0m"
