module Parse (TextWord(..), run, noArgs) where

import ClassyPrelude

import qualified Data.Text.Read as Read
import qualified Data.Text as Text

import qualified Util

-- | A `Text` which is only a single word
newtype TextWord = TextWord Text

class Arg a where
    fromText :: Text -> Maybe (a, Text)
instance Arg Int where
    fromText = Util.eitherToMaybe . Read.signed Read.decimal
instance Arg Word where
    fromText = Util.eitherToMaybe . Read.decimal
instance Arg Bool where
    fromText s = case x of
        "true"  -> Just (True, xs)
        "True"  -> Just (True, xs)
        "false" -> Just (False, xs)
        "False" -> Just (False, xs)
        _       -> Nothing
      where (x, xs) = Text.break (== ' ') s
instance Arg TextWord where
    fromText = Just . first TextWord . Text.break (== ' ')
instance Arg Text where
    fromText = Just . (, "")

parse :: ∀ a. Arg a => Text -> Maybe (a, Text)
parse "" = Nothing
parse s  = (second (dropWhile (== ' ')) <$>) $ fromText s

class Monad m => Run m r f where
    run :: f -> Text -> m (Maybe r)

-- With no arguments
noArgs :: ∀ m r. Monad m => r -> Text -> m (Maybe r)
noArgs x "" = return $ Just x
noArgs _ _  = return Nothing

-- With one argument
instance {-# OVERLAPPING #-} (Arg a, Monad m) => Run m r (a -> m r) where
    run f (parse -> Just (x, "")) = Just <$> f x
    run _ _                       = return Nothing

-- With more than one argument
instance (Arg a, Monad m, Run m r f) => Run m r (a -> f) where
    run _ (parse -> Just (_::a, "")) = return Nothing
    run f (parse -> Just (x, xs))    = run (f x) xs
    run _ _                          = return Nothing
