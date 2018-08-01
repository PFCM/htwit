{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module FilterMain where

import Auth
import Conduit
import Control.Applicative
import Control.Concurrent.MVar
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
import Data.Time.Clock
import Options.Applicative
import System.IO
import Web.Twitter.Conduit
import Web.Twitter.Types

data FilterOptions = FilterOptions
  { auth :: Maybe OAuth
  , rcfile :: String
  , keywords :: [T.Text]
  , numTweets :: Int
  , maxSecs :: Int
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
     value 10000)

maxSecsParser :: Parser Int
maxSecsParser =
  option
    auto
    (long "max_secs" <> short 'm' <> metavar "INT" <>
     help "how many seconds to run for" <>
     value 600)

optionParser :: Parser FilterOptions
optionParser =
  FilterOptions <$> authParser <*> rcPathParser <*> keywordsParser <*>
  numTweetsParser <*>
  maxSecsParser

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

formatCount :: Int -> String
formatCount c = "\r" ++ show c ++ "  tweets"

printCount :: MVar Int -> a -> IO ()
printCount count _ = do
  c <- takeMVar count
  let newC = c + 1
  hPutStr stderr . formatCount $ newC
  putMVar count newC
  return ()

-- note hardcoded language, disallowing of retweets
filterTweetStream :: Monad m => ConduitT () Value m () -> ConduitT () Value m ()
filterTweetStream c = c .| filterC (not . isRetweet) .| filterC (isLang "en")

isInTime :: NominalDiffTime -> UTCTime -> IO Bool
isInTime len start = (<= len) . (`diffUTCTime` start) <$> getCurrentTime

takeWhileMC :: Monad m => (a -> m Bool) -> ConduitT a a m ()
takeWhileMC f = go
  where
    go = do
      mx <- await
      case mx of
        Nothing -> return ()
        Just x -> do
          pred <- lift . f $ x
          case pred of
            False -> return ()
            True -> do
              yield x
              go

-- myMapMC :: Monad m => (i -> m o) -> ConduitT i o m ()
-- myMapMC f = go
--   where
--     go = do
--       mx <- await
--       case mx of
--         Nothing -> return ()
--         Just x -> do
--           val <- lift . f $ x
--           yield val
--           go
main :: IO ()
main = do
  opts <- execParser cmdOpts
  let creds = auth opts
  count <- newMVar 0
  mgr <- newManager tlsManagerSettings
  twinfo <- getAuth creds mgr (rcfile opts)
  let req = makeFilterRequest . keywords $ opts
  let duration = realToFrac . maxSecs $ opts
  putStr "["
  startTime <- getCurrentTime
  runResourceT $ do
    response <-
      stream' twinfo mgr req :: (ResourceT IO) (ConduitT () Value (ResourceT IO) ())
    let rc = filterTweetStream response
    runConduit $ rc .| takeC (numTweets opts) .|
      takeWhileMC (\_ -> lift $ isInTime duration startTime) .|
      mapC fullStatusText .|
      iterMC (lift . printCount count) .|
      intersperseC "," .|
      mapM_C (lift . B.putStr)
  putStrLn "]"
  hPutStrLn stderr ""
