module Main where

import Network.HTTP.Proxy.Server
import Data.Default.Class
import Data.ByteString

main = proxyMain (def :: Settings ByteString) {hostname = Just "localhost"}
