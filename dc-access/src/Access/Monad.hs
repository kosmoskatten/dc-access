{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}
module Access.Monad
    ( AccessT
    , Access
    , Config (..)
    , runAccess
    ) where

import Control.Lens ((?~), (&))
import Control.Monad.Reader ( ReaderT
                            , MonadReader
                            , runReaderT
                            )
import Control.Monad.Trans (MonadIO, MonadTrans, liftIO)
import Data.ByteString (ByteString)
import Data.IORef (newIORef)
import Data.Word (Word16)
import Network.Connection (TLSSettings (..))
import Network.HTTP.Client.TLS (mkManagerSettings)
import Network.Wreq (defaults, auth, basicAuth)
import Network.Wreq.Session (withSessionControl)
import Text.Printf (printf)

import Access.Rest (aquireToken)
import Access.Types (Context (..), Server)

data Config
    = Config
      { hostname :: !String
      , port     :: !Word16
      , insecure :: !Bool
      , username :: !ByteString
      , password :: !ByteString
      }

newtype AccessT m a = AccessT { unAccessT :: ReaderT Context m a }
    deriving ( Functor, Applicative, Monad
             , MonadReader Context, MonadIO, MonadTrans
             )

type Access = AccessT IO

runAccess :: Access a -> Config -> IO a
runAccess action Config {..} = do
    let tlsSettings = TLSSettingsSimple insecure False False
        mgrSettings = mkManagerSettings tlsSettings Nothing
        server'     = mkServer hostname port
        options     = defaults & auth ?~ basicAuth username password
    withSessionControl Nothing mgrSettings $ \session -> do
        eToken <- aquireToken session server' options
        case eToken of
            Right token -> do
                tokenRef' <- liftIO $ newIORef token
                let context = Context { server   = server'
                                      , tokenRef = tokenRef' }
                runReaderT (unAccessT action) context

            Left err    -> error err

mkServer :: String -> Word16 -> Server
mkServer = printf "https://%s:%u"
