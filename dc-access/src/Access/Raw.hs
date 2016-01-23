module Access.Raw
    ( aquireToken
    ) where

import Control.Monad.Trans (MonadIO)
import Network.Wreq (Options)
import Network.Wreq.Session (Session)

import Access.Types (Token, Server)

aquireToken :: MonadIO m => Session -> Server -> Options
            -> m (Either String Token)
aquireToken = undefined
