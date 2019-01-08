module App (main) where

import ClassyPrelude hiding (handle)

import qualified Control.Monad.Parallel as Concurrent
import qualified System.Environment as Env
import qualified Control.Exception as Exception
import qualified Network.HTTP.Conduit as HTTP
import qualified System.IO as IO
import qualified Data.Maybe as Maybe
import qualified Network
import qualified Control.Monad.Trans.Reader as Reader
import qualified Database.Persist.Postgresql as Sql
import qualified STMContainers.Map as Stm
import qualified Data.Text as Text

import Data.Pool (Pool)
import Text.Read (read)

import qualified Commands
import qualified Util
import qualified IRC
import IRC (Bot(..), Context(..))
import Persist

main :: Pool Sql.SqlBackend -> IO ()
main pool = 
    Exception.bracket (connect Nothing pool) (IO.hClose . botSocket) $ 
    Reader.runReaderT run

connect :: Maybe Handle -> Pool Sql.SqlBackend -> IO IRC.Bot
connect mSocket botPool = do
    botNick   <- pack <$> Env.getEnv "IRC_NICK"
    botOwner  <- (toLower . pack <$>) <$> Env.lookupEnv "OWNER"
    server    <- Env.getEnv "IRC_SERVER"
    port      <- read <$> Env.getEnv "IRC_PORT"
    users     <- db (Util.withKey userNick)
    botUsers  <- Stm.newIO
    atomically $ mapM_ (toStm botUsers) users
    botSocket <- case mSocket of
        Just socket -> return socket
        Nothing     -> Network.connectTo server $ Network.PortNumber port
    botWeb    <- HTTP.newManager HTTP.tlsManagerSettings
    IO.hSetBuffering botSocket IO.NoBuffering
    return IRC.Bot{..}
  where
    toStm m (k, v) = Stm.insert v k m
    db :: ∀ a b. Persist.Val a => (a -> b) -> IO [b]
    db f =
        (f . Sql.entityVal <$>) 
        <$> Sql.runSqlPersistMPool (Sql.selectList [] []) botPool

run :: IRC.BotIO ()
run = do
    botNick <- Reader.asks botNick
    IRC.silent "NICK" botNick
    IRC.silent "USER" $ botNick ++ " * * " ++ botNick
    forever listen

listen :: IRC.BotIO ()
listen = do
    bot@Bot{..} <- Reader.ask
    response <- liftIO $ pack <$> IO.hGetLine botSocket
    if | "PING :" `isPrefixOf` response ->
          IRC.silent "PONG" $ drop 5 response
       | loggedOn botNick `isPrefixOf` response -> do
          password <- Util.getEnv "IRC_PASSWORD"
          IRC.silent "PRIVMSG" $ "NickServ IDENTIFY " ++ password
          autojoin <- Util.getEnv "AUTOJOIN"
          for_ (Text.split (== ',') autojoin) $ IRC.silent "JOIN" . ("#" ++)
       | otherwise -> do
          let (usr, a)  = Util.breakOn '!' $ drop 1 response
              (cmd, b)  = Util.breakOn ' ' . drop 1 $ dropWhile (/= ' ') a
              (chan, c) = Util.breakOn ' ' b
              msg       = drop 1 c
              cmds      = getCommands msg
          if | cmd == "PRIVMSG" && not (null cmds) -> do
              putStrLn $ "\x1b[37m> " ++ response ++ "\x1b[0m"
              muser <- atomically $ Stm.lookup usr botUsers
              Reader.runReaderT (Concurrent.mapM_ Commands.eval cmds) $ Context
                  { ctxBot     = bot
                  , ctxChannel = chan
                  , ctxUser    = Maybe.fromMaybe (User usr 0 Nothing) muser
                  }
             | otherwise -> putStrLn response
  where
    loggedOn nick = ":" ++ nick ++ " MODE " ++ nick

getCommands :: Text -> [[Text]]
getCommands "" = []
getCommands s
  | '\SOH' `elem` s = [] -- Ignore CTCP
  | firstChar == '.' || firstChar == '!' = [words $ drop 1 s]
  | otherwise =
    (words <$>) .
    filter (not . null) .
    catMaybes .
    (splitBracket <$>) .
    drop 1 $
    Text.splitOn "[" s
  where
    firstChar = Text.head s
    splitBracket x = flip take x <$> Text.findIndex (== ']') x
