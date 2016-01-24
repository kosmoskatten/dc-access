-- | Low level functions to access the REST Api.
{-# LANGUAGE RecordWildCards #-}
module Access.Rest
    ( aquireToken
    , listVPodCollection
    ) where

import Control.Lens ((^.), (&), (?~))
import Data.Aeson (eitherDecode)
import Data.IORef (readIORef)
import Network.HTTP.Client (HttpException (..))
import Network.Wreq ( Options
                    , auth
                    , defaults
                    , oauth2Bearer
                    , responseBody
                    )
import Network.Wreq.Session (Session)

import qualified Data.ByteString.Lazy as LBS
import qualified Control.Exception as E
import qualified Network.HTTP.Client as C
import qualified Network.Wreq.Session as Session

import Access.EndPoint (EndPoint (..), (</>))
import Access.Types ( Context (..)
                    , Token (..)
                    , Response
                    , Server
                    )

-- | Aquire token from the server.
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

listVPodCollection :: Context -> IO Response
listVPodCollection Context {..} =
    listVPodCollection' `E.catch` apiHandler
    where
      listVPodCollection' :: IO Response
      listVPodCollection' = do
          Token {..} <- readIORef tokenRef
          let url  = server </> VPodAdminService_VPodCollection
              opts = defaults & auth ?~ oauth2Bearer accessToken
          returnSuccess =<< Session.getWith opts session url

apiHandler :: HttpException -> IO Response
apiHandler = returnFailure . show

returnSuccess :: C.Response LBS.ByteString -> IO Response
returnSuccess resp = return (Right $ resp ^. responseBody)

returnFailure :: String -> IO Response
returnFailure = return . Left
