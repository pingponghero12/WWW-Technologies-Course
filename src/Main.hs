{-# LANGUAGE OverloadedStrings #-}

module Main where

import Auth
import BeerShopDB
import ConnectDB
import Control.Exception (SomeException)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON), decode, encode, object, withObject, (.:), (.=))
import Data.Text qualified as T
import Data.Text.Lazy (Text, fromStrict, toStrict)
import Data.Text.Lazy qualified as TL
import Database.MySQL.Simple
import Network.HTTP.Types (status200)
import Network.HTTP.Types.Status
import Network.Wai (Middleware, mapResponseHeaders, requestMethod, responseLBS)
import Types
import Web.Scotty hiding (status)
import Web.Scotty qualified as S (status)

-- Helper type for order requests
data OrderRequest = OrderRequest
  { reqUserId :: Int,
    reqTotal :: Double
  }
  deriving (Show)

instance FromJSON OrderRequest where
  parseJSON = withObject "OrderRequest" $ \o ->
    OrderRequest
      <$> o .: "userId"
      <*> o .: "total"

-- Helper functions
getParamWithDefault :: Text -> Text -> ActionM Text
getParamWithDefault paramName defaultValue = do
  result <- param paramName `rescue` (\(_ :: SomeException) -> return defaultValue)
  return result

getAuthUser :: ActionM (Maybe AuthUser)
getAuthUser = do
  authHeader <- header "Authorization" `rescue` (\(_ :: SomeException) -> return Nothing)
  case authHeader of
    Nothing -> return Nothing
    Just headerVal -> do
      let token = T.drop 7 $ TL.toStrict headerVal -- Remove "Bearer " prefix
      return $ verifyToken token

requireAuth :: ActionM AuthUser
requireAuth = do
  maybeUser <- getAuthUser
  case maybeUser of
    Nothing -> do
      S.status unauthorized401
      json $ object ["error" .= ("Authentication required" :: String)]
      finish
    Just user -> return user

requireAdmin :: ActionM AuthUser
requireAdmin = do
  user <- requireAuth
  if isAdmin user
    then return user
    else do
      S.status forbidden403
      json $ object ["error" .= ("Admin access required" :: String)]
      finish

parseIntParam :: Text -> ActionM (Maybe Int)
parseIntParam paramName = do
  paramVal <- getParamWithDefault paramName ""
  let paramStr = T.unpack $ toStrict paramVal
  return $
    if null paramStr
      then Nothing
      else case reads paramStr of
        [(x, "")] -> Just x
        _ -> Nothing

parseDoubleParam :: Text -> ActionM (Maybe Double)
parseDoubleParam paramName = do
  paramVal <- getParamWithDefault paramName ""
  let paramStr = T.unpack $ toStrict paramVal
  return $
    if null paramStr
      then Nothing
      else case reads paramStr of
        [(x, "")] -> Just x
        _ -> Nothing

-- CORS Middleware
corsMiddleware :: Middleware
corsMiddleware app req respond = do
  let headers =
        [ ("Access-Control-Allow-Origin", "*"),
          ("Access-Control-Allow-Methods", "GET, POST, PUT, DELETE, PATCH, OPTIONS"),
          ("Access-Control-Allow-Headers", "Content-Type, Authorization")
        ]

  if requestMethod req == "OPTIONS"
    then respond $ responseLBS status200 headers ""
    else app req $ \res -> respond $ mapResponseHeaders (++ headers) res

main :: IO ()
main = do
  conn <- connect connInfo
  putStrLn "Beer Shop API started on port 3000"

  scotty 3000 $ do
    -- Apply CORS middleware
    middleware corsMiddleware

    -- Authentication endpoints
    post "/api/auth/register" $ do
      bodyBytes <- body
      case decode bodyBytes of
        Nothing -> do
          S.status badRequest400
          json $ object ["error" .= ("Invalid JSON" :: String)]
        Just newUser -> do
          result <- liftIO $ createUser conn newUser
          case result of
            Left err -> do
              S.status badRequest400
              json $ object ["error" .= err]
            Right userId -> do
              S.status created201
              json $ object ["id" .= userId, "message" .= ("User created successfully" :: String)]

    post "/api/auth/login" $ do
      bodyBytes <- body
      case decode bodyBytes of
        Nothing -> do
          S.status badRequest400
          json $ object ["error" .= ("Invalid JSON" :: String)]
        Just (LoginRequest email password) -> do
          maybeUser <- liftIO $ authenticateUser conn email password
          case maybeUser of
            Nothing -> do
              S.status unauthorized401
              json $ object ["error" .= ("Invalid credentials" :: String)]
            Just user -> do
              token <- liftIO $ generateToken user
              json $ LoginResponse token user

    -- Beer endpoints
    get "/api/beers" $ do
      limitParam <- parseIntParam "limit"
      offsetParam <- parseIntParam "offset"
      sortParam <- getParamWithDefault "sort" "id"
      categoryParam <- parseIntParam "category"
      minPriceParam <- parseDoubleParam "minPrice"
      maxPriceParam <- parseDoubleParam "maxPrice"
      minAlcoholParam <- parseDoubleParam "minAlcohol"
      maxAlcoholParam <- parseDoubleParam "maxAlcohol"

      let limit = case limitParam of Nothing -> Just 20; Just x -> Just x
          offset = case offsetParam of Nothing -> Just 0; Just x -> Just x
          sortBy =
            let s = toStrict sortParam
             in if s `elem` ["name", "price", "price_desc", "alcohol", "alcohol_asc"]
                  then Just s
                  else Nothing

      beers <-
        liftIO $
          getBeers
            conn
            limit
            offset
            sortBy
            categoryParam
            minPriceParam
            maxPriceParam
            minAlcoholParam
            maxAlcoholParam
      json beers

    get "/api/beers/:id" $ do
      beerId <- param "id"
      maybeBeer <- liftIO $ getBeerById conn beerId
      case maybeBeer of
        Nothing -> do
          S.status notFound404
          json $ object ["error" .= ("Beer not found" :: String)]
        Just beer -> json beer

    post "/api/beers" $ do
      _ <- requireAdmin
      bodyBytes <- body
      case decode bodyBytes of
        Nothing -> do
          S.status badRequest400
          json $ object ["error" .= ("Invalid JSON" :: String)]
        Just newBeer -> do
          beerId <- liftIO $ addBeer conn newBeer
          S.status created201
          json $ object ["id" .= beerId, "message" .= ("Beer created" :: String)]

    put "/api/beers/:id" $ do
      _ <- requireAdmin
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

    delete "/api/beers/:id" $ do
      _ <- requireAdmin
      beerId <- param "id"
      success <- liftIO $ deleteBeer conn beerId
      if success
        then json $ object ["message" .= ("Beer deleted" :: String)]
        else do
          S.status notFound404
          json $ object ["error" .= ("Beer not found" :: String)]

    -- Category endpoints
    get "/api/categories" $ do
      categories <- liftIO $ getCategories conn
      json categories

    get "/api/categories/:id" $ do
      categoryId <- param "id"
      maybeCategory <- liftIO $ getCategoryById conn categoryId
      case maybeCategory of
        Nothing -> do
          S.status notFound404
          json $ object ["error" .= ("Category not found" :: String)]
        Just category -> json category

    post "/api/categories" $ do
      _ <- requireAdmin
      bodyBytes <- body
      case decode bodyBytes of
        Nothing -> do
          S.status badRequest400
          json $ object ["error" .= ("Invalid JSON" :: String)]
        Just newCategory -> do
          categoryId <- liftIO $ addCategory conn newCategory
          S.status created201
          json $ object ["id" .= categoryId, "message" .= ("Category created" :: String)]

    put "/api/categories/:id" $ do
      _ <- requireAdmin
      categoryId <- param "id"
      bodyBytes <- body
      case decode bodyBytes of
        Nothing -> do
          S.status badRequest400
          json $ object ["error" .= ("Invalid JSON" :: String)]
        Just newCategory -> do
          success <- liftIO $ updateCategory conn categoryId newCategory
          if success
            then json $ object ["message" .= ("Category updated" :: String)]
            else do
              S.status notFound404
              json $ object ["error" .= ("Category not found" :: String)]

    delete "/api/categories/:id" $ do
      _ <- requireAdmin
      categoryId <- param "id"
      success <- liftIO $ deleteCategory conn categoryId
      if success
        then json $ object ["message" .= ("Category deleted" :: String)]
        else do
          S.status notFound404
          json $ object ["error" .= ("Category not found" :: String)]

    -- User endpoints
    get "/api/users" $ do
      _ <- requireAdmin
      users <- liftIO $ getUsers conn
      json users

    get "/api/users/:id" $ do
      authUser <- requireAuth
      userId <- param "id"
      if canAccessUserResource authUser userId
        then do
          maybeUser <- liftIO $ getUserById conn userId
          case maybeUser of
            Nothing -> do
              S.status notFound404
              json $ object ["error" .= ("User not found" :: String)]
            Just user -> json user
        else do
          S.status forbidden403
          json $ object ["error" .= ("Access denied" :: String)]

    post "/api/users" $ do
      _ <- requireAdmin
      bodyBytes <- body
      case decode bodyBytes of
        Nothing -> do
          S.status badRequest400
          json $ object ["error" .= ("Invalid JSON" :: String)]
        Just newUser -> do
          result <- liftIO $ createUser conn newUser
          case result of
            Left err -> do
              S.status badRequest400
              json $ object ["error" .= err]
            Right userId -> do
              S.status created201
              json $ object ["id" .= userId, "message" .= ("User created" :: String)]

    put "/api/users/:id" $ do
      _ <- requireAdmin
      userId <- param "id"
      bodyBytes <- body
      case decode bodyBytes of
        Nothing -> do
          S.status badRequest400
          json $ object ["error" .= ("Invalid JSON" :: String)]
        Just newUser -> do
          success <- liftIO $ updateUser conn userId newUser
          if success
            then json $ object ["message" .= ("User updated" :: String)]
            else do
              S.status notFound404
              json $ object ["error" .= ("User not found" :: String)]

    delete "/api/users/:id" $ do
      _ <- requireAdmin
      userId <- param "id"
      success <- liftIO $ deleteUser conn userId
      if success
        then json $ object ["message" .= ("User deleted" :: String)]
        else do
          S.status notFound404
          json $ object ["error" .= ("User not found" :: String)]

    -- Order endpoints
    get "/api/orders" $ do
      authUser <- requireAuth
      if isAdmin authUser
        then do
          orders <- liftIO $ getAllOrders conn
          json orders
        else do
          S.status forbidden403
          json $ object ["error" .= ("Admin access required" :: String)]

    get "/api/users/:id/orders" $ do
      authUser <- requireAuth
      userId <- param "id"
      if canAccessUserResource authUser userId
        then do
          orders <- liftIO $ getUserOrders conn userId
          json orders
        else do
          S.status forbidden403
          json $ object ["error" .= ("Access denied" :: String)]

    post "/api/orders" $ do
      authUser <- requireAuth
      bodyBytes <- body
      case decode bodyBytes of
        Nothing -> do
          S.status badRequest400
          json $ object ["error" .= ("Invalid JSON" :: String)]
        Just orderReq -> do
          let userId = reqUserId orderReq
          if canAccessUserResource authUser userId
            then do
              orderId <- liftIO $ createOrder conn userId (reqTotal orderReq)
              S.status created201
              json $ object ["id" .= orderId, "message" .= ("Order created" :: String)]
            else do
              S.status forbidden403
              json $ object ["error" .= ("Access denied" :: String)]

    patch "/api/orders/:id/status" $ do
      _ <- requireAdmin
      orderId <- param "id"
      bodyBytes <- body
      case decode bodyBytes of
        Nothing -> do
          S.status badRequest400
          json $ object ["error" .= ("Invalid JSON" :: String)]
        Just orderStatusUpdate -> do
          let status = newStatus orderStatusUpdate -- Use the record field from Types.hs
          success <- liftIO $ updateOrderStatus conn orderId status
          if success
            then json $ object ["message" .= ("Order status updated" :: String)]
            else do
              S.status notFound404
              json $ object ["error" .= ("Order not found" :: String)]

    -- Health check
    get "/api/health" $ do
      json $ object ["status" .= ("OK" :: String), "service" .= ("Beer Shop API" :: String)]

    notFound $ do
      S.status notFound404
      json $ object ["error" .= ("Endpoint not found" :: String)]
