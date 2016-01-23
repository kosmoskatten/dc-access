module Access.Raw
    ( aquireToken
    ) where

import Control.Lens ((^.))
import Data.Aeson (eitherDecode)
import Network.HTTP.Client (HttpException (..))
import Network.Wreq (Options, responseBody)
import Network.Wreq.Session (Session)

import qualified Control.Exception as E
import qualified Network.Wreq.Session as Session

import Access.EndPoint (EndPoint (..), (</>))
import Access.Types (Token (..), Server)

aquireToken :: Session -> Server -> Options -> IO (Either String Token)
aquireToken session host options =
    aquireToken' `E.catch` handler
    where
      aquireToken' :: IO (Either String Token)
      aquireToken' = do
          let url = host </> TokenService_Token
          resp <- Session.getWith options session url
          return $ eitherDecode (resp ^. responseBody)

      handler :: HttpException -> IO (Either String Token)
      handler = return . Left . show
