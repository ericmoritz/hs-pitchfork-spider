{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad
import Data.Aeson
import Data.Sort
import Network.HTTP.Simple
import PitchforkSpider.Data (Review (..))

data Results = Results [Review]
  deriving (Show)

instance FromJSON Results where
  parseJSON = withObject "Results" $ \o -> do
    items <- o .: "results" >>= (.: "list")
    reviews <- mapM parseJSON items
    return $ Results reviews

main :: IO ()
main = do
  let year = 2020 -- TODO get the real year
  request <- parseRequest "GET https://pitchfork.com/api/v2/search/?types=reviews&hierarchy=sections%2Freviews%2Falbums%2Cchannels%2Freviews%2Falbums&sort=publishdate%20desc%2Cposition%20asc&size=100&start=1"
  response <- httpBS request
  reviews <- case eitherDecodeStrict (getResponseBody response) of
    Left x -> fail x
    Right (Results x) -> return x
  forM_ (sort $ filter (\x -> rating x > 6.5 && releaseYear x == year) reviews) $ \x -> do
    print x
