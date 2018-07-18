{-# LANGUAGE OverloadedStrings #-}

module Json
  ( processTweets
  ) where

import Control.Applicative
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Text.Lazy as T

newtype Status = Status
  { getStatus :: T.Text
  } deriving (Show)

newtype Statuses = Statuses
  { getStatuses :: [Status]
  } deriving (Show)

instance FromJSON Statuses where
  parseJSON (Object v) = Statuses <$> v .: "statuses"
  parseJSON _ = empty

instance FromJSON Status where
  parseJSON (Object v) = Status <$> v .: "text"
  parseJSON _ = empty

instance ToJSON Status where
  toJSON = String . T.toStrict . getStatus

parseStatuses :: B.ByteString -> Maybe Statuses
parseStatuses = decode

displayBytes :: Maybe B.ByteString -> B.ByteString
displayBytes (Just b) = b
displayBytes Nothing = ""

-- pull the text statuses out of a search results json
processTweets :: B.ByteString -> B.ByteString
processTweets v = displayBytes $ values
  where
    v' = fmap getStatuses . parseStatuses $ v
    values = fmap encode v'
