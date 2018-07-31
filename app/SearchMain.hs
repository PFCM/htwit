{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module SearchMain where

import Auth
import Conduit
import Control.Applicative
import Control.Concurrent (threadDelay)

import Control.Lens
import Data.Aeson (encode)
import Data.Aeson.Lens
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy as B
import Data.Semigroup ((<>))
import qualified Data.Text as T
import Options.Applicative
import Search
import Web.Authenticate.OAuth as OA
import Web.Twitter.Conduit hiding (search)
import Web.Twitter.Types

-- the cmd line opts all at once
data Options = Options
  { auth :: Maybe OAuth
  , rcfile :: String
  , search :: SearchParams
  }

searchParamParser :: Parser SearchParams
searchParamParser =
  SearchParams <$>
  strOption
    (long "search_term" <> short 'q' <>
     help "search terms, space separated including twitter search keywords etc." <>
     metavar "SEARCH") <*>
  option
    auto
    (long "search_count" <> help "number of statuses per page" <> showDefault <>
     short 'c' <>
     value 100 <>
     metavar "INT") <*>
  strOption
    (long "lang" <> help "language code to limit results to" <> value "en" <>
     showDefault <>
     metavar "LANG")

rcPathParser :: Parser String
rcPathParser =
  strOption
    (long "credential_file" <> help "where the credentials are stored" <>
     value ".creds" <>
     showDefault)

makeTwitterOAuth :: String -> String -> OAuth
makeTwitterOAuth key secret =
  twitterOAuth
    { oauthConsumerKey = S8.pack key
    , oauthConsumerSecret = S8.pack secret
    , oauthCallback = Nothing
    }

oauthCreds :: Parser (Maybe OAuth)
oauthCreds =
  optional $
  makeTwitterOAuth <$>
  strOption
    (long "consumer_key" <>
     help "Consumer Key for the twitter app to authenticate as") <*>
  strOption
    (long "consumer_secret" <> help "Consumer Secret for the twitter app")

descriptionHeader :: InfoMod a
descriptionHeader =
  progDesc "the twitter search/tweets api if all you care about is the text" <>
  header "search-tweets: searches tweets."

optionParser :: Parser Options
optionParser = Options <$> oauthCreds <*> rcPathParser <*> searchParamParser

cmdOpts :: ParserInfo Options
cmdOpts = info (helper <*> optionParser) (fullDesc <> descriptionHeader)

printStatusStream ::
     Int -> ConduitT () B.ByteString IO () -> ConduitT () Void IO ()
printStatusStream delay inputStream =
  inputStream .| intersperseC "," .|
  mapM_C (\x -> B.putStr x >> threadDelay delay)

main :: IO ()
main = do
  opts <- execParser cmdOpts
  let creds = auth opts
  mgr <- newManager tlsManagerSettings
  twinfo <- getAuth creds mgr (rcfile opts)
  let searchParams = search opts
  result <- sourceWithSearchResult' twinfo mgr (makeSearchRequest searchParams)
  let statusStream = getStatusStream (searchCount searchParams) result
  putStr "["
  runConduit . printStatusStream 100 $ statusStream
  putStrLn "]"
