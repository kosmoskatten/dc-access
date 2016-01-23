-- | Collection of the types for the DataCenter Access. Some have
-- JSON representations, some have not.
{-# LANGUAGE OverloadedStrings #-}
module Access.Types
    ( Context (..)
    , Token (..)
    , Server
    ) where

import Data.Aeson
import Data.Aeson.Types
import Data.ByteString (ByteString)
import Data.IORef (IORef)
import Data.Text.Encoding (encodeUtf8)

type Server = String

data Context
    = Context
      { server   :: !Server
      , tokenRef :: !(IORef Token)
      }

data Token
    = Token
      { accessToken  :: !ByteString
      , refreshToken :: !ByteString
      }

instance FromJSON Token where
    parseJSON (Object o) =
        Token <$> (encodeUtf8 <$> o .: "access_token")
              <*> (encodeUtf8 <$> o .: "refresh_token")
    parseJSON invalid    = typeMismatch "Expecting Token object" invalid
