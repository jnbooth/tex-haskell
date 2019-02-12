module Util where

import ClassyPrelude

import qualified Data.Text as Text

atLeast :: (MonoFoldable o, Integral i) => o -> i -> Bool
atLeast xs len = GT == compareLength xs len
atMost  :: (MonoFoldable o, Integral i) => o -> i -> Bool
atMost  xs len = LT == compareLength xs len

eitherToMaybe :: ∀ a b. Either a b -> Maybe b
eitherToMaybe = either (const Nothing) Just

breakOn :: Char -> Text -> (Text, Text)
breakOn x = second (dropWhile (== x)) . Text.break (== x)

omitParens :: Text -> Text
omitParens = (\(_, _, s) -> s) . foldl' acc (0, ' ', "")
  where
    acc (i, prev, s) '(' = (i + 1,         prev, s)
    acc (i, prev, s) ')' = (max 0 $ i - 1, prev, s)
    acc (i, ' ',  s) ',' = (i,             ' ',  s)
    acc (i, ' ',  s) ' ' = (i,             ' ',  s)
    acc (0, _,    s) c   = (0,             c,    Text.snoc s c)
    acc x            _   = x
