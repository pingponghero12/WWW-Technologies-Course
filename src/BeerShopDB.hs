{-# LANGUAGE OverloadedStrings #-}

module BeerShopDB where

import Database.MySQL.Simple
import Database.MySQL.Simple.QueryResults
import Database.MySQL.Simple.QueryParams
import Types
import Auth
import Data.Text (Text)
import qualified Data.Text as T
import Data.String (fromString)
import Control.Monad (when)

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

getUserByEmail :: Connection -> Text -> IO (Maybe (User, Text))
getUserByEmail conn email = do
  rows <- query conn "SELECT id, username, email, role, password_hash FROM users WHERE email = ?" (Only email)
  case rows of
    [(uid, uname, uemail, urole, phash)] -> return $ Just (User uid uname uemail urole, phash)
    _ -> return Nothing

createUser :: Connection -> NewUser -> IO (Either String Int)
createUser conn (NewUser uname email password mrole) = do
  -- Check if user already exists
  existing <- getUserByEmail conn email
  case existing of
    Just _ -> return $ Left "User with this email already exists"
    Nothing -> do
      mhashedPw <- hashPassword password
      case mhashedPw of
        Nothing -> return $ Left "Failed to hash password"
        Just hashedPw -> do
          let userRole = case mrole of
                Just r | r `elem` ["admin", "customer"] -> r
                _ -> "customer"
          result <- execute conn 
            "INSERT INTO users (username, email, password_hash, role) VALUES (?, ?, ?, ?)"
            (uname, email, hashedPw, userRole)
          return $ Right $ fromIntegral result

updateUser :: Connection -> Int -> NewUser -> IO Bool
updateUser conn uid (NewUser uname email password mrole) = do
  mhashedPw <- hashPassword password
  case mhashedPw of
    Nothing -> return False
    Just hashedPw -> do
      let userRole = case mrole of
            Just r | r `elem` ["admin", "customer"] -> r
            _ -> "customer"
      result <- execute conn
        "UPDATE users SET username = ?, email = ?, password_hash = ?, role = ? WHERE id = ?"
        (uname, email, hashedPw, userRole, uid)
      return (result > 0)

deleteUser :: Connection -> Int -> IO Bool
deleteUser conn uid = do
  result <- execute conn "DELETE FROM users WHERE id = ?" (Only uid)
  return (result > 0)

-- Authentication with transition from plain text to bcrypt
authenticateUser :: Connection -> Text -> Text -> IO (Maybe User)
authenticateUser conn email password = do
  maybeUser <- getUserByEmail conn email
  case maybeUser of
    Nothing -> return Nothing
    Just (user, hash) -> do
      -- Check if it's a bcrypt hash or plain text
      let isValidPassword = if T.length hash > 20 && T.isPrefixOf "$2" hash
                           then verifyPassword password hash  -- bcrypt hash
                           else password == hash              -- plain text
      
      if isValidPassword
        then do
          -- If it was plain text, update to bcrypt hash
          when (not (T.isPrefixOf "$2" hash)) $ do
            mhashedPw <- hashPassword password
            case mhashedPw of
              Just newHash -> do
                _ <- execute conn "UPDATE users SET password_hash = ? WHERE email = ?" (newHash, email)
                return ()
              Nothing -> return ()
          return $ Just user
        else return Nothing

-- Beer functions with enhanced filtering
getBeers :: Connection -> Maybe Int -> Maybe Int -> Maybe Text -> Maybe Int -> Maybe Double -> Maybe Double -> Maybe Double -> Maybe Double -> IO [Beer]
getBeers conn mlimit moffset msort mcategory mminPrice mmaxPrice mminAlc mmaxAlc = do
  let baseQuery = "SELECT id, name, brewery, category_id, alcohol_percentage, price, stock_quantity, description FROM beers"
  
  -- Build WHERE clause
  let whereConditions = filter (not . T.null) [
        case mcategory of
          Just cat -> "category_id = " <> T.pack (show cat)
          Nothing -> "",
        case mminPrice of
          Just minP -> "price >= " <> T.pack (show minP)
          Nothing -> "",
        case mmaxPrice of
          Just maxP -> "price <= " <> T.pack (show maxP)
          Nothing -> "",
        case mminAlc of
          Just minA -> "alcohol_percentage >= " <> T.pack (show minA)
          Nothing -> "",
        case mmaxAlc of
          Just maxA -> "alcohol_percentage <= " <> T.pack (show maxA)
          Nothing -> ""
        ]
  
  let whereClause = if null whereConditions 
        then ""
        else " WHERE " <> T.intercalate " AND " whereConditions
  
  let orderClause = case msort of
        Just "name" -> " ORDER BY name"
        Just "price" -> " ORDER BY price"
        Just "price_desc" -> " ORDER BY price DESC"
        Just "alcohol" -> " ORDER BY alcohol_percentage DESC"
        Just "alcohol_asc" -> " ORDER BY alcohol_percentage ASC"
        _ -> " ORDER BY id"
  
  let limitClause = case (mlimit, moffset) of
        (Just l, Just o) -> " LIMIT " ++ show l ++ " OFFSET " ++ show o
        (Just l, Nothing) -> " LIMIT " ++ show l
        _ -> ""
  
  let fullQuery = fromString $ T.unpack $ T.concat [baseQuery, whereClause, T.pack orderClause, T.pack limitClause]
  
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

-- Category functions
getCategories :: Connection -> IO [Category]
getCategories conn = do
  rows <- query_ conn "SELECT id, name, description FROM categories ORDER BY name"
  return [Category cid cname cdesc | (cid, cname, cdesc) <- rows]

getCategoryById :: Connection -> Int -> IO (Maybe Category)
getCategoryById conn cid = do
  rows <- query conn "SELECT id, name, description FROM categories WHERE id = ?" (Only cid)
  case rows of
    [(categoryId, name, desc)] -> return $ Just (Category categoryId name desc)
    _ -> return Nothing

addCategory :: Connection -> NewCategory -> IO Int
addCategory conn (NewCategory name desc) = do
  result <- execute conn 
    "INSERT INTO categories (name, description) VALUES (?, ?)"
    (name, desc)
  return $ fromIntegral result

updateCategory :: Connection -> Int -> NewCategory -> IO Bool
updateCategory conn cid (NewCategory name desc) = do
  result <- execute conn
    "UPDATE categories SET name = ?, description = ? WHERE id = ?"
    (name, desc, cid)
  return (result > 0)

deleteCategory :: Connection -> Int -> IO Bool
deleteCategory conn cid = do
  result <- execute conn "DELETE FROM categories WHERE id = ?" (Only cid)
  return (result > 0)

-- Order functions
getUserOrders :: Connection -> Int -> IO [Order]
getUserOrders conn uid = do
  rows <- query conn "SELECT id, user_id, total_amount, status FROM orders WHERE user_id = ? ORDER BY created_at DESC" (Only uid)
  return [Order oid userId total status | (oid, userId, total, status) <- rows]

getAllOrders :: Connection -> IO [Order]
getAllOrders conn = do
  rows <- query_ conn "SELECT id, user_id, total_amount, status FROM orders ORDER BY created_at DESC"
  return [Order oid userId total status | (oid, userId, total, status) <- rows]

getOrderById :: Connection -> Int -> IO (Maybe Order)
getOrderById conn oid = do
  rows <- query conn "SELECT id, user_id, total_amount, status FROM orders WHERE id = ?" (Only oid)
  case rows of
    [(orderId, userId, total, status)] -> return $ Just (Order orderId userId total status)
    _ -> return Nothing

createOrder :: Connection -> Int -> Double -> IO Int
createOrder conn uid total = do
  result <- execute conn "INSERT INTO orders (user_id, total_amount) VALUES (?, ?)" (uid, total)
  return $ fromIntegral result

updateOrderStatus :: Connection -> Int -> Text -> IO Bool
updateOrderStatus conn oid newStatus = do
  when (newStatus `notElem` ["pending", "confirmed", "shipped", "delivered", "cancelled"]) $
    error "Invalid status"
  result <- execute conn "UPDATE orders SET status = ? WHERE id = ?" (newStatus, oid)
  return (result > 0)
