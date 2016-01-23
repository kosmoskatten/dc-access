module Access.EndPoint
    ( EndPoint (..)
    , (</>)
    ) where

import Access.Types (Server)

data EndPoint
    = TokenService_Token

(</>) :: Server -> EndPoint -> String
(</>) server ep = server `mappend` url ep

url :: EndPoint -> String
url TokenService_Token = "/rest/v0/TokenService/Actions/TokenService.Token"
