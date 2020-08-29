-- | The data types for Pitchfork search api
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}
module PitchforkSpider.Data
  ( runTests
  )
where

import           RIO
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.ByteString.Lazy          as LBS
import           Data.Scientific
import           Data.String.QQ
import           Data.Text
import           GHC.Generics
import           Test.Hspec
import           Data.Maybe
import           Control.Applicative


data Review = Review
  { reviewId :: Text
  , url :: Text
  , title :: Text
  , rating :: Scientific
  } deriving (Show, Eq, Generic)

data SearchResults = SearchResults [Review]


instance FromJSON Review where
  parseJSON = withObject "Review" $ \o -> do
    reviewId    <- o .: "id"
    title       <- o .: "socialTitle"
    url0        <- o .: "url"
    mbTombstone <- o .:? "tombstone" :: Parser (Maybe Value)
    -- I'm sure there is a nicer way to do this
    mbRating    <-
      (case mbTombstone of
        Nothing        -> return Nothing
        Just tombstone -> parseRatingFromTombstoneAlbum tombstone
      )
    let rating = maybe 0.0 id mbRating
    let url    = "https://pitchfork.com" <> url0
    return Review { .. }


parseRatingFromTombstoneAlbum :: Value -> Parser (Maybe Scientific)
parseRatingFromTombstoneAlbum = withObject "Rating" $ \o -> do
  mbRating <- o .:? "rating"
  return $ do
    ratingStr <- mbRating
    readMaybe ratingStr


testReviewParser :: IO ()
testReviewParser = hspec $ do
  describe "Parse Review" $ do
    it "should work in the happy case" $ do
      review `shouldBe` Right Review
        { reviewId = "5f43d52a8ae6324b59c6a321"
        , url      =
          "https://pitchfork.com/reviews/albums/the-front-bottoms-in-sickness-and-in-flames/"
        , title    = "The Front Bottoms: In Sickness & In Flames"
        , rating   = 0.0
        }
 where
  album :: Value
  album = object ["rating" .= String "5.4"]

  fixture :: Value
  fixture = object
    [ "id" .= String "5f43d52a8ae6324b59c6a321"
    , "url"
      .= String "/reviews/albums/the-front-bottoms-in-sickness-and-in-flames/"
    , "socialTitle" .= String "The Front Bottoms: In Sickness & In Flames"
    , "tombstone" .= object ["albums" .= [album]]
    ]
  review :: Either String Review
  review = parseEither parseJSON fixture



testParseRatingFromTombstoneAlbum :: IO ()
testParseRatingFromTombstoneAlbum = hspec $ do
  describe "parseRatingFromTombstoneAlbum" $ do
    it "should extract return the value if it is a valid numeric string" $ do
      let result =
            parseEither parseRatingFromTombstoneAlbum fixtureWithValidRating
      result `shouldBe` Right (Just 5.4)

    it "should extract return Nothing if the rating is invalid" $ do
      let result =
            parseEither parseRatingFromTombstoneAlbum fixtureWithBadRating
      result `shouldBe` Right Nothing

    it "should extract return Nothing if the rating key is missing" $ do
      let result =
            parseEither parseRatingFromTombstoneAlbum fixtureWithoutRating
      result `shouldBe` Right Nothing
 where
  fixtureWithValidRating :: Value
  fixtureWithValidRating = object ["rating" .= String "5.4"]

  fixtureWithBadRating :: Value
  fixtureWithBadRating = object ["rating" .= String "not a number"]

  fixtureWithoutRating :: Value
  fixtureWithoutRating = object []


runTests :: IO ()
runTests = do
  testReviewParser
  testParseRatingFromTombstoneAlbum
