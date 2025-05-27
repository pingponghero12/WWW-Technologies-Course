{-# LANGUAGE OverloadedStrings #-}

module ConnectDB where

import Database.MySQL.Simple

connInfo :: ConnectInfo
connInfo = defaultConnectInfo
  { connectHost = "127.0.0.1"  -- Changed from "localhost" to force TCP
  , connectPort = 3306
  , connectUser = "root"
  , connectPassword = "1234"
  , connectDatabase = "beer_shop_db"  -- Make sure this matches your docker-compose.yml
  }
