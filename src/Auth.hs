{-# LANGUAGE OverloadedStrings #-}

module Auth where

import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Text (Text)
import Data.Time.Clock (UTCTime, getCurrentTime, addUTCTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Crypto.BCrypt
import Web.JWT as JWT
import qualified Data.Map as Map
import Types

-- JWT secret (in production, this should be in environment variable)
jwtSecret :: Text
jwtSecret = "your-secret-key-change-in-production"

-- Token expiration time (24 hours)
tokenExpirationTime :: NominalDiffTime
tokenExpirationTime = 24 * 60 * 60

-- Hash password using bcrypt
hashPassword :: Text -> IO (Maybe Text)
hashPassword password = do
  hashed <- hashPasswordUsingPolicy slowerBcryptHashingPolicy (TE.encodeUtf8 password)
  return $ fmap TE.decodeUtf8 hashed

-- Verify password against hash
verifyPassword :: Text -> Text -> Bool
verifyPassword password hash = 
  validatePassword (TE.encodeUtf8 hash) (TE.encodeUtf8 password)

-- Generate JWT token
generateToken :: User -> IO Text
generateToken user = do
  now <- getCurrentTime
  let expTime = addUTCTime tokenExpirationTime now
      claims = def { 
        sub = stringOrURI $ T.pack $ show (userId user),
        JWT.exp = numericDate $ utcTimeToPOSIXSeconds expTime,
        iat = numericDate $ utcTimeToPOSIXSeconds now,
        unregisteredClaims = Map.fromList [
          ("role", String $ role user),
          ("username", String $ username user)
        ]
      }
      token = encodeSigned HS256 (secret jwtSecret) claims
  return token

-- Verify JWT token and extract user info
verifyToken :: Text -> Maybe AuthUser
verifyToken token = do
  jwt <- decode token
  verified <- verify (secret jwtSecret) jwt
  if verified
    then do
      let claims = claims verified
      userIdStr <- stringOrURIToText <$> sub claims
      userId' <- readMaybe $ T.unpack userIdStr
      role' <- Map.lookup "role" (unregisteredClaims claims) >>= 
               (\case String r -> Just r; _ -> Nothing)
      username' <- Map.lookup "username" (unregisteredClaims claims) >>= 
                   (\case String u -> Just u; _ -> Nothing)
      return $ AuthUser userId' username' role'
    else Nothing
  where
    readMaybe s = case reads s of
      [(x, "")] -> Just x
      _ -> Nothing
    stringOrURIToText uri = T.pack $ show uri

-- Check if user has admin role
isAdmin :: AuthUser -> Bool
isAdmin user = authRole user == "admin"

-- Check if user can access resource (admin or owner)
canAccessUserResource :: AuthUser -> Int -> Bool
canAccessUserResource authUser targetUserId = 
  isAdmin authUser || authUserId authUser == targetUserId
