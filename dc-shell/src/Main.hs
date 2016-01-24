{-# LANGUAGE OverloadedStrings #-}
module Main
    ( main
    ) where

import Access (Config (..))
import Shell (Shell, runShell)

main :: IO ()
main = do
    let c = Config { hostname = "192.168.1.75"
                   , port     = 10443
                   , insecure = True
                   , username = "sysadmin"
                   , password = "sysadmin123"
                   }
    runShell (return ()) c
