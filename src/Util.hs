module Util where

import ClassyPrelude
import qualified System.Environment as Env
import qualified Data.Text as Text

withKey :: ∀ a b. (a -> b) -> a -> (b, a)
withKey f x = (f x, x)

-- TODO benchmark `Text.breakOn . singleton` vs. `Text.break . (==)`
breakOn :: Char -> Text -> (Text, Text)
breakOn x = second (drop 1) . Text.breakOn (singleton x)

getEnv :: ∀ m. MonadIO m => Text -> m Text
getEnv = (pack <$>) . liftIO . Env.getEnv . unpack
lookupEnv :: ∀ m. MonadIO m => Text -> m (Maybe Text)
lookupEnv = ((pack <$>) <$>) . liftIO . Env.lookupEnv . unpack
