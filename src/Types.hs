{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Types where

import Data.Aeson
import GHC.Generics
import Data.Text (Text)

data User = User
  { userId :: Int
  , username :: Text
  , email :: Text
  , role :: Text
  } deriving (Generic, Show)

data Beer = Beer
  { beerId :: Int
  , beerName :: Text
  , brewery :: Text
  , categoryId :: Maybe Int
  , alcoholPercentage :: Maybe Double
  , price :: Double
  , stockQuantity :: Int
  , description :: Maybe Text
  } deriving (Generic, Show)

data Order = Order
  { orderId :: Int
  , orderUserId :: Int
  , totalAmount :: Double
  , status :: Text
  } deriving (Generic, Show)

data NewBeer = NewBeer
  { newBeerName :: Text
  , newBrewery :: Text
  , newCategoryId :: Maybe Int
  , newAlcoholPercentage :: Maybe Double
  , newPrice :: Double
  , newStockQuantity :: Int
  , newDescription :: Maybe Text
  } deriving (Generic, Show)

-- JSON instances
instance ToJSON User where
  toJSON (User uid uname uemail urole) = object
    [ "id" .= uid
    , "username" .= uname
    , "email" .= uemail
    , "role" .= urole
    ]

instance ToJSON Beer where
  toJSON (Beer bid bname bbrewery bcatId balc bprice bstock bdesc) = object
    [ "id" .= bid
    , "name" .= bname
    , "brewery" .= bbrewery
    , "categoryId" .= bcatId
    , "alcoholPercentage" .= balc
    , "price" .= bprice
    , "stockQuantity" .= bstock
    , "description" .= bdesc
    ]

instance ToJSON Order where
  toJSON (Order oid ouid ototal ostatus) = object
    [ "id" .= oid
    , "userId" .= ouid
    , "totalAmount" .= ototal
    , "status" .= ostatus
    ]

instance FromJSON NewBeer where
  parseJSON = withObject "NewBeer" $ \o -> NewBeer
    <$> o .: "name"
    <*> o .: "brewery"
    <*> o .:? "categoryId"
    <*> o .:? "alcoholPercentage"
    <*> o .: "price"
    <*> o .: "stockQuantity"
    <*> o .:? "description"
