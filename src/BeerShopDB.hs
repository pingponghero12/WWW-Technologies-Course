{-# LANGUAGE OverloadedStrings #-}

module BeerShopDB where

import Database.MySQL.Simple
import Database.MySQL.Simple.QueryResults
import Database.MySQL.Simple.QueryParams
import Types
import Data.Text (Text)
import qualified Data.Text as T
import Data.String (fromString)

-- User functions
getUsers :: Connection -> IO [User]
getUsers conn = do
  rows <- query_ conn "SELECT id, username, email, role FROM users ORDER BY username"
  return [User uid uname uemail urole | (uid, uname, uemail, urole) <- rows]

getUserById :: Connection -> Int -> IO (Maybe User)
getUserById conn uid = do
  rows <- query conn "SELECT id, username, email, role FROM users WHERE id = ?" (Only uid)
  case rows of
    [(userId, username, email, role)] -> return $ Just (User userId username email role)
    _ -> return Nothing

-- Beer functions
getBeers :: Connection -> Maybe Int -> Maybe Int -> Maybe Text -> IO [Beer]
getBeers conn mlimit moffset msort = do
  let baseQuery = "SELECT id, name, brewery, category_id, alcohol_percentage, price, stock_quantity, description FROM beers"
  let orderClause = case msort of
        Just "name" -> " ORDER BY name"
        Just "price" -> " ORDER BY price"
        Just "alcohol" -> " ORDER BY alcohol_percentage DESC"
        _ -> " ORDER BY id"
  let limitClause = case (mlimit, moffset) of
        (Just l, Just o) -> " LIMIT " ++ show l ++ " OFFSET " ++ show o
        (Just l, Nothing) -> " LIMIT " ++ show l
        _ -> ""
  let fullQuery = fromString $ baseQuery ++ orderClause ++ limitClause
  
  rows <- query_ conn fullQuery
  return [Beer bid bname brewery catId alc price stock desc | 
          (bid, bname, brewery, catId, alc, price, stock, desc) <- rows]

getBeerById :: Connection -> Int -> IO (Maybe Beer)
getBeerById conn bid = do
  rows <- query conn "SELECT id, name, brewery, category_id, alcohol_percentage, price, stock_quantity, description FROM beers WHERE id = ?" (Only bid)
  case rows of
    [(beerId, name, brewery, catId, alc, price, stock, desc)] -> 
      return $ Just (Beer beerId name brewery catId alc price stock desc)
    _ -> return Nothing

addBeer :: Connection -> NewBeer -> IO Int
addBeer conn (NewBeer name brewery catId alc price stock desc) = do
  result <- execute conn 
    "INSERT INTO beers (name, brewery, category_id, alcohol_percentage, price, stock_quantity, description) VALUES (?, ?, ?, ?, ?, ?, ?)"
    (name, brewery, catId, alc, price, stock, desc)
  return $ fromIntegral result

updateBeer :: Connection -> Int -> NewBeer -> IO Bool
updateBeer conn bid (NewBeer name brewery catId alc price stock desc) = do
  result <- execute conn
    "UPDATE beers SET name = ?, brewery = ?, category_id = ?, alcohol_percentage = ?, price = ?, stock_quantity = ?, description = ? WHERE id = ?"
    (name, brewery, catId, alc, price, stock, desc, bid)
  return (result > 0)

deleteBeer :: Connection -> Int -> IO Bool
deleteBeer conn bid = do
  result <- execute conn "DELETE FROM beers WHERE id = ?" (Only bid)
  return (result > 0)

-- Order functions
getUserOrders :: Connection -> Int -> IO [Order]
getUserOrders conn uid = do
  rows <- query conn "SELECT id, user_id, total_amount, status FROM orders WHERE user_id = ? ORDER BY created_at DESC" (Only uid)
  return [Order oid userId total status | (oid, userId, total, status) <- rows]

createOrder :: Connection -> Int -> Double -> IO Int
createOrder conn uid total = do
  result <- execute conn "INSERT INTO orders (user_id, total_amount) VALUES (?, ?)" (uid, total)
  return $ fromIntegral result

-- Simple auth (in real app, use proper password hashing)
verifyUser :: Connection -> Text -> Text -> IO (Maybe User)
verifyUser conn email password = do
  rows <- query conn "SELECT id, username, email, role FROM users WHERE email = ? AND password_hash = ?" (email, password)
  case rows of
    [(uid, uname, uemail, urole)] -> return $ Just (User uid uname uemail urole)
    _ -> return Nothing
