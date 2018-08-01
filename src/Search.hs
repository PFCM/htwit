{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Search
  ( SearchParams(..)
  , getStatusStream
  , makeSearchRequest
  )
where

import           Conduit
import           Control.Lens
import           Data.Aeson                               ( Value
                                                          , encode
                                                          )
import           Data.Aeson.Lens
import qualified Data.ByteString.Lazy          as B
import qualified Data.Text                     as T
import           Web.Twitter.Conduit
import           Web.Twitter.Types

data SearchParams = SearchParams
  { searchTerm :: T.Text
  , searchCount :: Int
  , searchLang :: T.Text
  }

extendedTweetsQueryItem :: APIQueryItem
extendedTweetsQueryItem = ("tweet_mode", PVString "extended")

makeSearchRequest
  :: SearchParams -> APIRequest SearchTweets (SearchResult [Status])
makeSearchRequest SearchParams {..} =
  request & params <>~ [extendedTweetsQueryItem]
  where request = searchTweets searchTerm & lang ?~ searchLang & count ?~ 100

getStatusStream
  :: Monad m
  => Int
  -> SearchResult (ConduitT () Value m ())
  -> ConduitT () B.ByteString m ()
getStatusStream num result = searchStream .| takeC num .| mapC
  (\v -> encode $ v ^. key "full_text" . _String)
  where searchStream = searchResultStatuses result
