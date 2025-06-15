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

data AuthUser = AuthUser
  { authUserId :: Int
  , authUsername :: Text
  , authRole :: Text
  } deriving (Generic, Show)

data Beer = Beer
  { beerId :: Int
  , beerName :: Text
  , brewery :: Text
  , beerCategoryId :: Maybe Int  -- Changed from categoryId to beerCategoryId
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

data Category = Category
  { categoryId :: Int
  , categoryName :: Text
  , categoryDescription :: Maybe Text
  } deriving (Generic, Show)

data NewBeer = NewBeer
  { newBeerName :: Text
  , newBrewery :: Text
  , newBeerCategoryId :: Maybe Int  -- Changed from newCategoryId to newBeerCategoryId
  , newAlcoholPercentage :: Maybe Double
  , newPrice :: Double
  , newStockQuantity :: Int
  , newDescription :: Maybe Text
  } deriving (Generic, Show)

data NewUser = NewUser
  { newUsername :: Text
  , newEmail :: Text
  , newPassword :: Text
  , newRole :: Maybe Text
  } deriving (Generic, Show)

data LoginRequest = LoginRequest
  { loginEmail :: Text
  , loginPassword :: Text
  } deriving (Generic, Show)

data LoginResponse = LoginResponse
  { token :: Text
  , user :: User
  } deriving (Generic, Show)

data NewCategory = NewCategory
  { newCategoryName :: Text
  , newCategoryDescription :: Maybe Text
  } deriving (Generic, Show)

data OrderStatusUpdate = OrderStatusUpdate
  { newStatus :: Text
  } deriving (Generic, Show)

-- JSON instances
instance ToJSON User where
  toJSON (User uid uname uemail urole) = object
    [ "id" .= uid
    , "username" .= uname
    , "email" .= uemail
    , "role" .= urole
    ]

instance ToJSON AuthUser where
  toJSON (AuthUser uid uname urole) = object
    [ "id" .= uid
    , "username" .= uname
    , "role" .= urole
    ]

instance ToJSON Beer where
  toJSON (Beer bid bname bbrewery bcatId balc bprice bstock bdesc) = object
    [ "id" .= bid
    , "name" .= bname
    , "brewery" .= bbrewery
    , "categoryId" .= bcatId  -- Keep JSON field name as categoryId for API compatibility
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

instance ToJSON Category where
  toJSON (Category cid cname cdesc) = object
    [ "id" .= cid
    , "name" .= cname
    , "description" .= cdesc
    ]

instance ToJSON LoginResponse where
  toJSON (LoginResponse tok usr) = object
    [ "token" .= tok
    , "user" .= usr
    ]

instance FromJSON NewBeer where
  parseJSON = withObject "NewBeer" $ \o -> NewBeer
    <$> o .: "name"
    <*> o .: "brewery"
    <*> o .:? "categoryId"  -- Keep JSON field name as categoryId for API compatibility
    <*> o .:? "alcoholPercentage"
    <*> o .: "price"
    <*> o .: "stockQuantity"
    <*> o .:? "description"

instance FromJSON NewUser where
  parseJSON = withObject "NewUser" $ \o -> NewUser
    <$> o .: "username"
    <*> o .: "email"
    <*> o .: "password"
    <*> o .:? "role"

instance FromJSON LoginRequest where
  parseJSON = withObject "LoginRequest" $ \o -> LoginRequest
    <$> o .: "email"
    <*> o .: "password"

instance FromJSON NewCategory where
  parseJSON = withObject "NewCategory" $ \o -> NewCategory
    <$> o .: "name"
    <*> o .:? "description"

instance FromJSON OrderStatusUpdate where
  parseJSON = withObject "OrderStatusUpdate" $ \o -> OrderStatusUpdate
    <$> o .: "status"  -- Fixed: Use <$> instead of <*> for the first field
