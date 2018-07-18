{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Auth
import Control.Applicative
import Control.Exception
import Control.Lens
import Control.Monad.IO.Class
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy as B
import Data.Conduit
import qualified Data.Conduit.List as CL
import Data.Semigroup ((<>))
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Json
import Options.Applicative
import System.Directory (doesFileExist)
import Web.Twitter.Conduit hiding (search)
import Web.Twitter.Types

-- import Web.Twitter.Types.Lens
data NoCredentialException
  = CredentialFileError
  | AuthError
  deriving (Show)

instance Exception NoCredentialException

-- the cmd line opts all at once
data Options = Options
  { auth :: Maybe OAuth
  , rcfile :: String
  , search :: SearchParams
  }

data SearchParams = SearchParams
  { searchTerm :: T.Text
  , searchCount :: Integer
  , searchLang :: T.Text
  }

searchParamParser :: Parser SearchParams
searchParamParser =
  SearchParams <$>
  strOption
    (long "search_term" <> short 'q' <>
     help
       "search terms, space separated ro including twitter search keywords etc." <>
     metavar "SEARCH") <*>
  option
    auto
    (long "search_count" <> help "number of statuses per page" <> showDefault <>
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

getAuth :: Maybe OAuth -> Manager -> FilePath -> IO TWInfo
getAuth (Just auth) mgr rcpath = do
  twinfo <- authorise auth mgr
  putStrLn $ "Authentication successful, caching results in: " ++ rcpath
  let creds = twCredential . twToken $ twinfo
  writeCredentialFile rcpath auth creds
  return twinfo
getAuth Nothing _ rcpath = tryReadFile rcpath

-- irritatingly partial
tryReadFile :: FilePath -> IO TWInfo
tryReadFile path = do
  creds <- readCredentialFile path
  case creds of
    Just vals -> return vals
    Nothing -> throw CredentialFileError

-- main :: IO ()
-- main = B.interact processTweets
main :: IO ()
main = do
  opts <- execParser cmdOpts
  let creds = auth opts
  mgr <- newManager tlsManagerSettings
  twinfo <- getAuth creds mgr (rcfile opts)
  putStrLn "attempting search"
  let searchParams = search opts
  sourceWithMaxId
    twinfo
    mgr
    (searchTweets (searchTerm searchParams) & lang ?~ (searchLang searchParams) &
     count ?~ (searchCount searchParams)) $=
    CL.isolate 10 $$
    CL.mapM_ $ \res ->
    liftIO $ do
      T.putStrLn . T.intercalate "\n" . fmap statusText $
        searchResultStatuses res
