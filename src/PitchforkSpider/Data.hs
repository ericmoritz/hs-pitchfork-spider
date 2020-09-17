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
        releaseYear :: Integer,
        rating :: Scientific
      }
  deriving (Show, Eq, Generic)

instance Ord Review where
  compare x y = compare (rating x) (rating y)

instance FromJSON Review where
  parseJSON = withObject "Review" $ \o -> do
    tombstone <- parseTombstone o
    Review
      <$> reviewId o
      <*> url o
      <*> title o
      <*> pure (tombReleaseYear tombstone)
      <*> pure (tombRating tombstone)
    where
      reviewId = (.: "id")
      url o = ("https://pitchfork.com" <>) <$> (o .: "url")
      title = (.: "socialTitle")
      parseTombstone o = do
        albums <- o .: "tombstone" >>= (.: "albums")
        tombstones <- mapM parseJSON albums
        return $ maybe (TombstoneAlbum 0 0) id (tombstones !? 0)

data TombstoneAlbum
  = TombstoneAlbum
      { tombReleaseYear :: Integer,
        tombRating :: Scientific
      }
  deriving (Show, Eq)

instance FromJSON TombstoneAlbum where
  parseJSON = withObject "tombstone" $ \o ->
    TombstoneAlbum
      <$> (parseYear o <|> return 0)
      <*> (parseRating o <|> return 0)
    where
      parseYear = (.: "album") >=> (.: "release_year")
      parseRating o = do
        ratingStr <- o .: "rating" >>= (.: "rating")
        case readMaybe ratingStr of
          Nothing -> fail $ "could not read rating: " ++ ratingStr
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
              releaseYear = 0,
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

testParseTombstoneAlbum :: IO ()
testParseTombstoneAlbum = hspec $ do
  describe "parseRatingFromTombstoneAlbum" $ do
    it "should extract return the value if it is a valid numeric string" $ do
      let fixture = object ["rating" .= object ["rating" .= String "5.4"], "album" .= object ["release_year" .= Number 2020]]
      parseEither parseJSON fixture `shouldBe` Right (TombstoneAlbum 2020 5.4)
    it "should set the rating to 0 if it can't be read" $ do
      let fixture = object ["rating" .= object ["rating" .= String "not a number"], "album" .= object ["release_year" .= Number 2020]]
      parseEither parseJSON fixture `shouldBe` Right (TombstoneAlbum 2020 0)
    it "should set the rating to 0 if it is not found in inner object" $ do
      let fixture = object ["rating" .= object [], "album" .= object ["release_year" .= Number 2020]]
      parseEither parseJSON fixture `shouldBe` Right (TombstoneAlbum 2020 0)
    it "should set the rating to 0 if it is not found in root object" $ do
      let fixture = object ["album" .= object ["release_year" .= Number 2020]]
      parseEither parseJSON fixture `shouldBe` Right (TombstoneAlbum 2020 0)
    it "should set release year to 0 if release_year is missing on the album object" $ do
      let fixture = object ["rating" .= object ["rating" .= String "5.4"], "album" .= object []]
      parseEither parseJSON fixture `shouldBe` Right (TombstoneAlbum 0 5.4)
    it "should set release year to 0 if the album key is missing" $ do
      let fixture = object ["rating" .= object ["rating" .= String "5.4"]]
      parseEither parseJSON fixture `shouldBe` Right (TombstoneAlbum 0 5.4)
  where

runTests :: IO ()
runTests = do
  testReviewParser
  testParseTombstoneAlbum
