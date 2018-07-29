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
import Options.Applicative
import Web.Twitter.Conduit
import Web.Twitter.Types

data FilterOptions = FilterOptions
  { auth :: Maybe OAuth
  , rcfile :: String
  , keywords :: [T.Text]
  , numTweets :: Int
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

numTweetsParser :: Parser Int
numTweetsParser =
  option
    auto
    (long "num" <> short 'n' <> metavar "INT" <>
     help "how many tweets to grab before exiting" <>
     value 100)

optionParser :: Parser FilterOptions
optionParser =
  FilterOptions <$> authParser <*> rcPathParser <*> keywordsParser <*>
  numTweetsParser

cmdOpts :: ParserInfo FilterOptions
cmdOpts = info (helper <*> optionParser) (descriptionHeader <> fullDesc)

makeFilterRequest :: [T.Text] -> APIRequest StatusesFilter StreamingAPI
makeFilterRequest = statusesFilter . pure . Track

truncated :: Value -> Maybe Bool
truncated v = v ^? key "truncated" . _Bool

statusLanguage :: Value -> T.Text
statusLanguage v = v ^. key "lang" . _String

extendedText :: Value -> B.ByteString
extendedText v = encode $ v ^. key "extended_tweet" . key "full_text" . _String

normalText :: Value -> B.ByteString
normalText v = encode $ v ^. key "text" . _String

fullStatusText :: Value -> B.ByteString
fullStatusText v =
  case extendedText v of
    "\"\"" -> normalText v
    x -> x

isRetweet :: Value -> Bool
isRetweet v = isJust rt
  where
    rt = v ^? key "retweeted_status"

isLang :: T.Text -> Value -> Bool
isLang l v = statusLanguage v == l

zipC ::
     Monad m => ConduitT a b m r -> ConduitT () c m r -> ConduitT a (b, c) m r
zipC a b = do
  aVal <- await a
  bVal <- await b
  yield (aVal, bVal)

-- countAndPrint :: Monad m => ConduitT a (IO a) m ()
-- countAndPrint c = let
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
    runConduit $ response .| filterC (not . isRetweet) .| filterC (isLang "en") .|
      takeC (numTweets opts) .|
      mapC fullStatusText .|
      mapM_C (lift . B.putStrLn)
