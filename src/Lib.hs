module Lib (main) where

import ClassyPrelude

import qualified Control.Concurrent.Forkable as Concurrent
import qualified Configuration.Dotenv as Dotenv
import qualified Control.Exception as Exception
import qualified Data.HashMap.Strict as HashMap
import Data.HashMap.Strict (HashMap)
import qualified System.IO as IO
import qualified Data.Text.Read as Read
import qualified Control.Monad.Trans.Reader as Reader
import Control.Monad.Trans.Reader (ReaderT)
import qualified Data.Text as Text

import qualified Command
import qualified Commands
import qualified Context
import Context (Context(..))
import qualified Env
import Env (ENV, Env)
import qualified IRC
import IRC (IRC)
import qualified Util

main :: IO ()
main = do
    void $ Dotenv.loadFile Dotenv.defaultConfig
    Exception.bracket Env.new (IO.hClose . Env.handle) $ 
        Reader.runReaderT run

run :: ENV ()
run = do
    nick <- Reader.asks Env.nick
    Env.silent "NICK" nick
    Env.silent "USER" $ nick ++ " * * " ++ nick
    forever listen

listen :: ENV ()
listen = do
    env <- Reader.ask
    let botNick = Env.nick env
    response <- (pack <$>) . liftIO . IO.hGetLine $ Env.handle env
    if | "PING :" `isPrefixOf` response ->
          Env.silent "PONG" $ drop 5 response
       | loggedOn botNick `isPrefixOf` response -> do
          password <- Env.getEnv "IRC_PASSWORD"
          Env.silent "PRIVMSG" $ "NickServ IDENTIFY " ++ password
          autojoin <- Env.getEnv "AUTOJOIN"
          for_ (Text.split (== ',') autojoin) $ Env.silent "JOIN" . ("#" ++)
       | otherwise -> do
          let (nick, a) = Util.breakOn ' ' $ drop 1 response
              (cmd, b)  = Util.breakOn ' ' $ drop 1 a
              (chan, c) = Util.breakOn ' ' b
              msg       = drop 1 c
              cmds      = getCommands msg
          if | cmd == "PRIVMSG" && not (null cmds) -> do
                  putStrLn $ "\x1b[37m> " ++ response ++ "\x1b[0m"
                  ctx <- liftIO $ Context.new env chan nick
                  Reader.runReaderT (forkEval cmds) ctx
             | otherwise -> putStrLn response
  where
    loggedOn nick = ":" ++ nick ++ " MODE " ++ nick
    forkEval = traverse_ $ Concurrent.forkIO . eval

getCommands :: Text -> [Text]
getCommands "" = []
getCommands s
  | '\SOH' `elem` s = [] -- Ignore CTCP
  | firstChar == '.' || firstChar == '!' = [drop 1 s]
  | otherwise = filter (not . null) . 
                catMaybes .
                (rightBracket <$>) .
                drop 1 $
                leftBracket s
  where
    firstChar      = Text.head s
    leftBracket    = Text.splitOn "["
    rightBracket x = flip take x <$> Text.findIndex (== ']') x

eval :: Text -> IRC ()
eval s = case HashMap.lookup cmd Commands.commands of
    Nothing -> return ()
    Just f  -> do
        res <- f query
        case res of
            Nothing -> return ()
        IRC.tryReply res
  where
    (cmd, query) = Util.breakOn ' ' s
