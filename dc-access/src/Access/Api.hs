module Access.Api
    ( listVPodCollection
    ) where

import Control.Monad.Trans (liftIO)

import Access.Monad (Access, context)
import Access.Types (Response)

import qualified Access.Rest as Rest

listVPodCollection :: Access Response
listVPodCollection = do
    c <- context
    liftIO $ Rest.listVPodCollection c
