{-# LANGUAGE OverloadedStrings #-}

module Main where

import ConnectDB
import BeerShopDB
import Types
import Control.Monad.IO.Class (liftIO)
import Control.Exception (SomeException)
import Data.Aeson (decode, object, (.=), FromJSON(..), withObject, (.:))
import Data.Text.Lazy (Text, toStrict)
import Database.MySQL.Simple
import Web.Scotty hiding (status)
import qualified Web.Scotty as S (status)
import Network.HTTP.Types.Status
import qualified Data.Text as T

getParamWithDefault :: Text -> Text -> ActionM Text
getParamWithDefault paramName defaultValue = do
  result <- param paramName `rescue` (\(_ :: SomeException) -> return defaultValue)
  return result

data OrderRequest = OrderRequest
  { reqUserId :: Int
  , reqTotal :: Double
  } deriving Show

instance FromJSON OrderRequest where
  parseJSON = withObject "OrderRequest" $ \o -> OrderRequest
    <$> o .: "userId"
    <*> o .: "total"

main :: IO ()
main = do
  conn <- connect connInfo
  putStrLn "Beer Shop API started on port 3000"
  
  scotty 3000 $ do
    
    -- GET /api/beers - Get all beers with pagination, sorting
    get "/api/beers" $ do
      limitParam <- getParamWithDefault "limit" "10"
      offsetParam <- getParamWithDefault "offset" "0"
      sortParam <- getParamWithDefault "sort" "id"
      
      let limit = read (T.unpack $ toStrict limitParam) :: Int
          offset = read (T.unpack $ toStrict offsetParam) :: Int
          sortBy = if (toStrict sortParam) `elem` ["name", "price", "alcohol"] 
                   then Just (toStrict sortParam) 
                   else Nothing
      
      beers <- liftIO $ getBeers conn (Just limit) (Just offset) sortBy
      json beers

    get "/api/beers/:id" $ do
      beerId <- param "id"
      maybeBeer <- liftIO $ getBeerById conn beerId
      case maybeBeer of
        Nothing -> do
          S.status notFound404
          json $ object ["error" .= ("Beer not found" :: String)]
        Just beer -> json beer

    -- Add new beer
    post "/api/beers" $ do
      bodyBytes <- body
      case decode bodyBytes of
        Nothing -> do
          S.status badRequest400
          json $ object ["error" .= ("Invalid JSON" :: String)]
        Just newBeer -> do
          beerId <- liftIO $ addBeer conn newBeer
          S.status created201
          json $ object ["id" .= beerId, "message" .= ("Beer created" :: String)]

    -- Update beer
    put "/api/beers/:id" $ do
      beerId <- param "id"
      bodyBytes <- body
      case decode bodyBytes of
        Nothing -> do
          S.status badRequest400
          json $ object ["error" .= ("Invalid JSON" :: String)]
        Just newBeer -> do
          success <- liftIO $ updateBeer conn beerId newBeer
          if success
            then json $ object ["message" .= ("Beer updated" :: String)]
            else do
              S.status notFound404
              json $ object ["error" .= ("Beer not found" :: String)]

    -- DELETE /api/beers/:id - Delete beer
    delete "/api/beers/:id" $ do
      beerId <- param "id"
      success <- liftIO $ deleteBeer conn beerId
      if success
        then json $ object ["message" .= ("Beer deleted" :: String)]
        else do
          S.status notFound404
          json $ object ["error" .= ("Beer not found" :: String)]

    -- Get all users
    get "/api/users" $ do
      users <- liftIO $ getUsers conn
      json users

    -- Get specific user
    get "/api/users/:id" $ do
      userId <- param "id"
      maybeUser <- liftIO $ getUserById conn userId
      case maybeUser of
        Nothing -> do
          S.status notFound404
          json $ object ["error" .= ("User not found" :: String)]
        Just user -> json user

    --  Get user orders
    get "/api/users/:id/orders" $ do
      userId <- param "id"
      orders <- liftIO $ getUserOrders conn userId
      json orders

    -- Create new order
    post "/api/orders" $ do
      bodyBytes <- body
      case decode bodyBytes of
        Nothing -> do
          S.status badRequest400
          json $ object ["error" .= ("Invalid JSON" :: String)]
        Just orderReq -> do
          orderId <- liftIO $ createOrder conn (reqUserId orderReq) (reqTotal orderReq)
          S.status created201
          json $ object ["id" .= orderId, "message" .= ("Order created" :: String)]

    -- Health check
    get "/api/health" $ do
      json $ object ["status" .= ("OK" :: String), "service" .= ("Beer Shop API" :: String)]

    notFound $ do
      S.status notFound404
      json $ object ["error" .= ("Endpoint not found" :: String)]
