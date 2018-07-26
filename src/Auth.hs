{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveAnyClass #-}

module Auth where

import Control.Exception
import qualified Data.ByteString.Char8 as S8
import System.IO (hFlush, stdout)
import Web.Authenticate.OAuth as OA
import Web.Twitter.Conduit hiding (map)

data NoCredentialException
  = CredentialFileError
  | AuthError
  deriving (Show, Exception)

-- instance Exception NoCredentialException
-- irritatingly partial
tryReadFile :: FilePath -> IO TWInfo
tryReadFile path = do
  creds <- readCredentialFile path
  case creds of
    Just vals -> return vals
    Nothing -> throw CredentialFileError

getAuth :: Maybe OAuth -> Manager -> FilePath -> IO TWInfo
getAuth (Just auth) mgr rcpath = do
  twinfo <- authorise auth mgr
  putStrLn $ "Authentication successful, caching results in: " ++ rcpath
  let creds = twCredential . twToken $ twinfo
  writeCredentialFile rcpath auth creds
  return twinfo
getAuth Nothing _ rcpath = tryReadFile rcpath

-- try read the file containing credentials and stuff them into the right type
readCredentialFile :: FilePath -> IO (Maybe TWInfo)
readCredentialFile path = makeTWInfo . S8.lines <$> S8.readFile path
  where
    makeTWInfo [a, b, c, d] =
      Just $ setCredential (makeOAuth a b) (makeCred c d) def
    makeTWInfo _ = Nothing
    makeOAuth k s = twitterOAuth {oauthConsumerKey = k, oauthConsumerSecret = s}
    makeCred t s = Credential [("oauth_token", t), ("oauth_token_secret", s)]

-- write credentials to file
writeCredentialFile :: FilePath -> OAuth -> Credential -> IO ()
writeCredentialFile path oauth =
  S8.writeFile path . packCreds oauth . unCredential
  where
    packCreds auth creds =
      S8.unlines $
      [oauthConsumerKey auth, oauthConsumerSecret auth] ++
      (take 2 . map snd $ creds)

-- should check to see if rc file exists, otherwise prompt but for now just prompt
authorise :: OAuth -> Manager -> IO TWInfo
authorise oauth mgr = do
  cred <- OA.getTemporaryCredential oauth mgr
  let url = OA.authorizeUrl oauth cred
  pin <- getPIN url
  realcred <- OA.getAccessToken oauth (OA.insert "oauth_verifier" pin cred) mgr
  return $ setCredential oauth realcred def
  where
    getPIN url = do
      putStrLn $ "visit: " ++ url
      putStr "> enter pin: "
      hFlush stdout
      S8.getLine
