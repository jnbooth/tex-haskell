module Lib (main) where

import ClassyPrelude

import qualified Control.Concurrent.Forkable as Concurrent
import qualified Configuration.Dotenv as Dotenv
import qualified System.IO as IO
import qualified Data.Text as Text

import qualified Command
import qualified Commands
import qualified Context
import qualified Env
import Env (ENV)
import qualified IRC
import IRC (IRC)
import qualified Util

main :: IO ()
main = do
    void $ Dotenv.loadFile Dotenv.defaultConfig
    bracket Env.new (IO.hClose . Env.handle) $ runReaderT run

run :: ENV ()
run = do
    nick <- asks Env.nick
    Env.silent "NICK" nick
    Env.silent "USER" $ nick ++ " * * " ++ nick
    forever listen

listen :: ENV ()
listen = do
    env <- ask
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
          let (nick, a)   = Util.breakOn ' ' $ drop 1 response
              (cmd, b)    = Util.breakOn ' ' a
              (chan, msg) = Util.breakOn ' ' b
              cmds        = getCommands $ drop 1 msg
          if | cmd == "PRIVMSG" && not (null cmds) -> do
                  putStrLn $ "\x1b[37m> " ++ response ++ "\x1b[0m"
                  ctx <- liftIO $ Context.new chan nick
                  runReaderT (traverse_ (Concurrent.forkIO . eval) cmds) ctx
             | otherwise -> putStrLn response
  where
    loggedOn nick = ":" ++ nick ++ " MODE " ++ nick

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
eval s = case lookup command Commands.commands of
    Nothing  -> return ()
    Just cmd -> IRC.reply . fromMaybe (Command.usage cmd) =<<
                Command.run cmd query
  where
    (command, query) = Util.breakOn ' ' s
