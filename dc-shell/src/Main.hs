{-# LANGUAGE OverloadedStrings #-}
module Main
    ( main
    ) where

import Control.Monad (when)
import Control.Monad.Trans (liftIO, lift)

import Access (Config (..), successful, json, listVPodCollection)
import Shell (Shell, runShell)

main :: IO ()
main = do
    let c = Config { hostname = "192.168.1.75"
                   , port     = 10443
                   , insecure = True
                   , username = "sysadmin"
                   , password = "sysadmin123"
                   }
    runShell fetchVPods c

fetchVPods :: Shell ()
fetchVPods = do
    resp <- lift $ listVPodCollection
    when (successful resp) $
      liftIO $ print (json resp)

