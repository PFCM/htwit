module Main where

import qualified Data.ByteString.Lazy as B
import Lib

main :: IO ()
main = B.interact processTweets
