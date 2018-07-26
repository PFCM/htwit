{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module FilterMain where

import Auth
import Conduit
import Control.Applicative
import Control.Lens ((^.), (^?))
import Control.Monad.Trans.Resource ()
import Data.Aeson (Value, encode)
import Data.Aeson.Lens
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy as B
import qualified Data.Conduit.List as CL
import Data.Maybe
import Data.Semigroup ((<>))
import qualified Data.Text as T
import Debug.Trace
import Options.Applicative
import Web.Twitter.Conduit
import Web.Twitter.Types

data FilterOptions = FilterOptions
  { auth :: Maybe OAuth
  , rcfile :: String
  , keywords :: [T.Text]
  } deriving (Show)

keywordsParser :: Parser [T.Text]
keywordsParser =
  some $ argument str (metavar "KEYWORDS" <> help "keywords to track")

descriptionHeader :: InfoMod a
descriptionHeader =
  progDesc "The twitter statuses/filter api if you have very specific desires" <>
  header "statuses-filter: filters statuses."

rcPathParser :: Parser String
rcPathParser =
  strOption
    (long "credential_file" <> help "where credentials live" <> value ".creds" <>
     showDefault)

makeTwitterOAuth :: String -> String -> OAuth
makeTwitterOAuth key secret =
  twitterOAuth
  { oauthConsumerKey = S8.pack key
  , oauthConsumerSecret = S8.pack secret
  , oauthCallback = Nothing
  }

authParser :: Parser (Maybe OAuth)
authParser =
  optional $ makeTwitterOAuth <$>
  strOption (long "consumer_key" <> help "consumer key for twitter api") <*>
  strOption (long "consumer_secret" <> help "consumer secret for twitter api")

optionParser :: Parser FilterOptions
optionParser = FilterOptions <$> authParser <*> rcPathParser <*> keywordsParser

cmdOpts :: ParserInfo FilterOptions
cmdOpts = info (helper <*> optionParser) (descriptionHeader <> fullDesc)

makeFilterRequest :: [T.Text] -> APIRequest StatusesFilter StreamingAPI
makeFilterRequest = statusesFilter . pure . Track

truncated :: Value -> Maybe Bool
truncated v = v ^? key "truncated" . _Bool

extendedText :: Value -> Maybe B.ByteString
extendedText v =
  case trace ("truncated: " ++ (show . truncated $v)) fullText of
    "\"\"" -> Nothing
    x -> Just x
  where
    fullText = encode $ v ^. key "extended_tweet" . key "full_text" . _String

normalText :: Value -> B.ByteString
normalText v = encode $ v ^. key "text" . _String

fullStatusText :: Value -> Maybe B.ByteString
fullStatusText v = extendedText v <|> Just (normalText v)

isRetweet :: Value -> Bool
isRetweet v = isJust rt
  where
    rt = v ^? key "retweeted_status"

main :: IO ()
main = do
  opts <- execParser cmdOpts
  let creds = auth opts
  mgr <- newManager tlsManagerSettings
  twinfo <- getAuth creds mgr (rcfile opts)
  let req = makeFilterRequest . keywords $ opts
  runResourceT $ do
    response <-
      stream' twinfo mgr req :: (ResourceT IO) (ConduitT () Value (ResourceT IO) ())
    runConduit $ response .| filterC isRetweet .| takeC 10 .|
      mapC fullStatusText .|
      CL.catMaybes .|
      mapM_C (lift . B.putStrLn)
