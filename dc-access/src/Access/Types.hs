-- | Collection of the types for the DataCenter Access. Some have
-- JSON representations, some have not.
{-# LANGUAGE OverloadedStrings #-}
module Access.Types
    ( Context (..)
    , Token (..)
    , Server
    , JSON
    , Response
    , successful
    , selection
    , json
    , responseError
    ) where

import Data.Aeson ( FromJSON (..)
                  , Value (..)
                  , (.:)
                  , eitherDecode
                  )
import Data.Aeson.Types (typeMismatch)
import Data.Either (isRight)
import Data.IORef (IORef)
import Data.Text.Encoding (encodeUtf8)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS

-- | Identifier of an API server.
type Server = String

type JSON = LBS.ByteString

type Response = Either String JSON

-- | Was a response successful?
successful :: Response -> Bool
successful = isRight

-- | Decode from JSON to the selected data structure.
selection :: FromJSON a => Response -> Either String a
selection = either (Left . const "Unsuccessful response") eitherDecode

-- | Get hand of the raw JSON string, if any.
json :: Response -> Either String JSON
json = either (Left . const "Unsuccessful response") Right

-- | Get hand of the responseError, is any.
responseError :: Response -> Maybe String
responseError = either Just (const Nothing)

-- | The runtime context for an ongoing session towards the API.
data Context
    = Context
      { server   :: !Server
        -- ^ API server.
      , tokenRef :: !(IORef Token)
        -- ^ Access tokens.
      }

-- | Access tokens towards the API.
data Token
    = Token
      { accessToken  :: !BS.ByteString
        -- ^ Access token.
      , refreshToken :: !BS.ByteString
        -- ^ Refresh token.
      }

-- | JSON decoding for Token.
instance FromJSON Token where
    parseJSON (Object o) =
        Token <$> (encodeUtf8 <$> o .: "access_token")
              <*> (encodeUtf8 <$> o .: "refresh_token")
    parseJSON invalid    = typeMismatch "Expecting Token object" invalid
