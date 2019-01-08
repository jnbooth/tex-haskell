{-# LANGUAGE TemplateHaskell #-}
module Local (Local(..)) where

import ClassyPrelude
import qualified Text.Read as Read
import Data.Aeson
import Web.PathPieces
import Web.Internal.HttpApiData
import Database.Persist.TH

import qualified Util

data Local = Local
    { channel :: !Text
    , obj     :: !Text
    }
    deriving (Eq, Ord)

instance Show Local where
  show Local{..} = unpack $ channel ++ " " ++ obj

instance Read Local where
  readPrec = uncurry Local . Util.breakOn ' ' . pack <$> Read.look

instance ToJSON Local where 
  toJSON = toJSON . show
instance FromJSON Local where 
  parseJSON = (Read.read <$>) . parseJSON
instance PathPiece Local where
  fromPathPiece = (Read.read <$>) . fromPathPiece
  toPathPiece = toPathPiece . show
instance ToHttpApiData Local where
  toQueryParam = toQueryParam . show
instance FromHttpApiData Local where
  parseQueryParam = (Read.read <$>) . parseQueryParam

derivePersistField "Local"
