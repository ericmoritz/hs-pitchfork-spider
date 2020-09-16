{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | The data types for Pitchfork search api
module PitchforkSpider.Data
  ( runTests,
    Review (..),
  )
where

import Control.Applicative
import Data.Aeson
import Data.Aeson.Types
import Data.Maybe
import Data.Scientific
import Data.Text
import Data.Vector ((!?))
import GHC.Generics
import RIO
import Test.Hspec

data Review
  = Review
      { reviewId :: Text,
        url :: Text,
        title :: Text,
        rating :: Scientific
      }
  deriving (Show, Eq, Generic)

instance Ord Review where
  compare x y = compare (rating x) (rating y)

instance FromJSON Review where
  parseJSON = withObject "Review" $ \o -> do
    reviewId <- o .: "id"
    title <- o .: "socialTitle"
    url <- ("https://pitchfork.com" <>) <$> (o .: "url")
    albums <- o .: "tombstone" >>= (.: "albums")
    ratings <- mapM parseRatingFromTombstoneAlbum albums
    rating <- case ratings !? 0 of
      Nothing -> fail "not rating found"
      Just x -> return x
    return Review {..}

parseRatingFromTombstoneAlbum :: Value -> Parser Scientific
parseRatingFromTombstoneAlbum = withObject "album" $ \o -> do
  ratingStr <- o .: "rating" >>= (.: "rating")
  -- TODO: read is probably no the best thing here
  case readMaybe ratingStr of
    Nothing -> fail "could not read rating"
    Just x -> return x

testReviewParser :: IO ()
testReviewParser = hspec $ do
  describe "Parse Review" $ do
    it "should work in the happy case" $ do
      review
        `shouldBe` Right
          Review
            { reviewId = "5f43d52a8ae6324b59c6a321",
              url =
                "https://pitchfork.com/reviews/albums/the-front-bottoms-in-sickness-and-in-flames/",
              title = "The Front Bottoms: In Sickness & In Flames",
              rating = 5.4
            }
  where
    album :: Value
    album = object ["rating" .= object ["rating" .= String "5.4"]]
    fixture :: Value
    fixture =
      object
        [ "id" .= String "5f43d52a8ae6324b59c6a321",
          "url"
            .= String "/reviews/albums/the-front-bottoms-in-sickness-and-in-flames/",
          "socialTitle" .= String "The Front Bottoms: In Sickness & In Flames",
          "tombstone" .= object ["albums" .= [album]]
        ]
    review :: Either String Review
    review = parseEither parseJSON fixture

testParseRatingFromTombstoneAlbum :: IO ()
testParseRatingFromTombstoneAlbum = hspec $ do
  describe "parseRatingFromTombstoneAlbum" $ do
    it "should extract return the value if it is a valid numeric string" $ do
      let result =
            parseEither parseRatingFromTombstoneAlbum fixtureWithValidRating
      result `shouldBe` Right 5.4
    it "should extract return Nothing if the rating is invalid" $ do
      let result =
            parseEither parseRatingFromTombstoneAlbum fixtureWithBadRating
      result `shouldBe` Left "Error in $: could not read rating"
    it "should extract return Nothing if the rating key is missing" $ do
      let result =
            parseEither parseRatingFromTombstoneAlbum fixtureWithoutRating
      result `shouldBe` Left "Error in $: key \"rating\" not found"
  where
    fixtureWithValidRating :: Value
    fixtureWithValidRating = object ["rating" .= object ["rating" .= String "5.4"]]
    fixtureWithBadRating :: Value
    fixtureWithBadRating = object ["rating" .= object ["rating" .= String "not a number"]]
    fixtureWithoutRating :: Value
    fixtureWithoutRating = object []

runTests :: IO ()
runTests = do
  testReviewParser
  testParseRatingFromTombstoneAlbum
