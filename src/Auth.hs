{-# LANGUAGE OverloadedStrings #-}

module Auth where

import Crypto.BCrypt qualified as BCrypt
import Data.Aeson qualified as Aeson
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time.Clock (NominalDiffTime, UTCTime, addUTCTime, getCurrentTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Types
import Web.JWT qualified as JWT

jwtSecret :: Text
jwtSecret = "your-secret-key-change-in-production"

tokenExpirationTime :: NominalDiffTime
tokenExpirationTime = 24 * 60 * 60

-- Fixed: Use BCrypt.hashPasswordUsingPolicy for salt rounds
hashPassword :: Text -> IO (Maybe Text)
hashPassword password = do
  let passwordBS = TE.encodeUtf8 password
      -- Use hashPasswordUsingPolicy with slowerBcryptHashingPolicy for 12 rounds
      policy = BCrypt.slowerBcryptHashingPolicy
  mhashed <- BCrypt.hashPasswordUsingPolicy policy passwordBS
  return $ fmap TE.decodeUtf8 mhashed

verifyPassword :: Text -> Text -> Bool
verifyPassword password hash =
  BCrypt.validatePassword (TE.encodeUtf8 password) (TE.encodeUtf8 hash)

generateToken :: User -> IO Text
generateToken user = do
  now <- getCurrentTime
  let expTime = addUTCTime tokenExpirationTime now
      cs =
        mempty
          { JWT.sub = JWT.stringOrURI $ T.pack (show (userId user)),
            JWT.exp = JWT.numericDate $ utcTimeToPOSIXSeconds expTime,
            JWT.iat = JWT.numericDate $ utcTimeToPOSIXSeconds now,
            JWT.unregisteredClaims =
              JWT.ClaimsMap $
                Map.fromList
                  [ ("role", Aeson.String $ role user),
                    ("username", Aeson.String $ username user)
                  ]
          }
      signer = JWT.hmacSecret jwtSecret
      token = JWT.encodeSigned signer mempty cs
  return token

verifyToken :: Text -> Maybe AuthUser
verifyToken token = do
  jwt <- JWT.decode token
  let signer = JWT.hmacSecret jwtSecret
      verifier = JWT.toVerify signer
  verifiedJWT <- JWT.verify verifier jwt
  let cs = JWT.claims verifiedJWT
      subField = JWT.sub cs
      userIdStr = case subField of
        Just uri -> JWT.stringOrURIToText uri
        Nothing -> ""
  userId' <- readMaybeInt (T.unpack userIdStr)
  let claimsMap = JWT.unClaimsMap $ JWT.unregisteredClaims cs
  role' <- case Map.lookup "role" claimsMap of
    Just (Aeson.String r) -> Just r
    _ -> Nothing
  username' <- case Map.lookup "username" claimsMap of
    Just (Aeson.String u) -> Just u
    _ -> Nothing
  return $ AuthUser userId' username' role'

readMaybeInt :: String -> Maybe Int
readMaybeInt s = case reads s of
  [(x, "")] -> Just x
  _ -> Nothing

isAdmin :: AuthUser -> Bool
isAdmin user = authRole user == "admin"

canAccessUserResource :: AuthUser -> Int -> Bool
canAccessUserResource authUser targetUserId =
  isAdmin authUser || authUserId authUser == targetUserId
