module Util where

import ClassyPrelude

import qualified Data.Text as Text

eitherToMaybe :: ∀ a b. Either a b -> Maybe b
eitherToMaybe = either (const Nothing) Just

breakOn :: Char -> Text -> (Text, Text)
breakOn x = second (dropWhile (== x)) . Text.break (== x)
