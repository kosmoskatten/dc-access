-- | Collection of the types for the DataCenter Access. Some have
-- JSON representations, some have not.
module Access.Types
    ( Context (..)
    , Token (..)
    , Server
    ) where

import Data.IORef (IORef)

type Server = String

data Context
    = Context
      { server   :: !Server
      , tokenRef :: !(IORef Token)
      }

data Token = Token
