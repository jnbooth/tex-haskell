module Env (Env(..), new, ENV, getEnv, silent, send) where

import ClassyPrelude hiding (handle)

import qualified Network.HTTP.Conduit as HTTP
import qualified System.IO as IO
import qualified Network.Socket as Net
import Network.Socket (Socket)
import qualified Database.PostgreSQL.Simple as SQL
import qualified System.Environment as System

data Env = Env
    { handle :: !Handle
    , out    :: !(Chan (Text, Bool))
    , web    :: !HTTP.Manager
    , sql    :: !SQL.Connection

    , nick   :: !Text
    , owner  :: !Text
    }

type ENV = ReaderT Env IO

getEnv :: ∀ m. MonadIO m => String -> m Text
getEnv = (pack <$>) . liftIO . System.getEnv

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
    out    <- newChan
    web    <- HTTP.newManager HTTP.tlsManagerSettings
    dbUrl  <- getEnv "DATABASE_URL"
    sql    <- SQL.connectPostgreSQL $ encodeUtf8 dbUrl
    nick   <- getEnv "IRC_NICK"
    owner  <- getEnv "OWNER"
    return Env{..}

output :: Bool -> Text -> Text -> ENV ()
output vis command message = do
    out <- asks Env.out
    writeChan out (command ++ " " ++ message, vis)

silent :: Text -> Text -> ENV ()
silent = output False
send :: Text -> Text -> ENV ()
send = output True
