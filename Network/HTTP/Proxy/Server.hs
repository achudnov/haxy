{-# LANGUAGE ScopedTypeVariables #-}
-- | A library for programming custom proxy servers.
module Network.HTTP.Proxy.Server (proxyMain 
                                 ,Settings (..)
                                 ,Cache (..)
                                 ,Default(..)) where

import Network.HTTP hiding (port)
import Network.HTTP.Server hiding (Response, Request)
import Network.HTTP.Server.Logger
import Data.Default.Class
import Network.HostName

-- | Proxy entry-point. Spawns a new proxy server.
proxyMain :: forall s. HStream s => Settings s -> IO ()
proxyMain settings = 
  do hname <- case hostname settings of
       Nothing -> getHostName
       Just hostn -> return hostn
     let config = defaultConfig {srvPort = fromInteger $ portnum settings
                                ,srvHost = hname
                                ,srvLog  = mylogger}
     putStrLn "Proxy server started on port 3128\n"
     serverWith config (proxyHandler settings)
     
mylogger = stdLogger

proxyHandler :: HStream s => Settings s -> Handler s
proxyHandler settings _ _ request = 
  -- check that the request is authorized
  isAuthorized settings request >>= 
  \authorized -> if authorized then processRequest settings request
                 else errorProxyUnauthorized
                      
-- |Processes the request; this is the main proxy procedure                     
processRequest :: HStream s => Settings s -> Request s -> IO (Response s)
processRequest settings request = do
  -- modify the request
  modRequest <- requestModifier settings request
  -- check the cache
  mCachedResponse <- queryCache (cache settings) modRequest
  case mCachedResponse of
    -- found in cache: return
    Just response -> return response
    -- not found: fetch it from a remote server, invoke the
    -- 'responseModifier' hook, record in cache and return
    Nothing       -> do 
      response <- fetch request
      modResponse <- responseModifier settings request response
      recordInCache (cache settings) request modResponse 
      return modResponse
      
fetch :: HStream s => Request s -> IO (Response s)
fetch request = do
  result <- simpleHTTP request
  case result of 
    Left err  -> do putStrLn ("Connection error: " ++ show err)
                    errorInternalServerError
    Right rsp -> return rsp
    
-- | Proxy server settings                
data Settings s = 
  Settings {requestModifier  :: Request s -> IO (Request s)
            -- ^ A function for modifying requests. Will be called for
            -- each request received; the modified request will be
            -- forwarded to the target server. Defaults to an identity
            -- function.
           ,responseModifier :: Request s -> Response s -> IO (Response s)
            -- ^ A function for modifying responses. Will be called
            -- for each response received; the modified response will
            -- be forwarded to the client. Defaults to an identity
            -- function.
           ,cache            :: Cache s
            -- ^ The cache. Use 'def' for no cache.
           ,isAuthorized     :: Request s -> IO Bool
            -- ^ Authorization function. Allows denying certain
            -- requests. Defaults to allowing all requests
           ,logger           :: String -> IO ()
            -- ^ A logging function. The default is no logging.
           ,portnum          :: Integer
            -- ^ Proxy server port number; default is 3128
           ,hostname         :: Maybe String
            -- ^ The server host name. Defaults to the result of
            -- 'getHostName'
           }

instance Default (Settings s) where
  def = Settings {requestModifier  = return
                 ,responseModifier = \_ -> return
                 ,cache            = def
                 ,isAuthorized     = return . const True
                 ,logger           = \_ -> return ()
                 ,portnum          = 3128
                 ,hostname         = Nothing}

-- | The cache.
data Cache s = Cache {queryCache :: Request s -> IO (Maybe (Response s))
                      -- ^ Retreive the response to a request from the
                      -- cache.
                     ,recordInCache :: Request s -> Response s -> IO ()
                      -- ^ Record the response to a request in the
                      -- cache.
                     }

instance Default (Cache s) where
  def = Cache {queryCache = return . const Nothing
              ,recordInCache = \_ -> return . const ()}
        
-- |A generic 500 response
errorInternalServerError :: HStream s => IO (Response s)
errorInternalServerError = return $ err_response InternalServerError

-- |A generic 407 response. TODO: RFC 2068 requres to send
-- Proxy-Authenticate header with this response code
errorProxyUnauthorized :: HStream s => IO (Response s)
errorProxyUnauthorized = return $ err_response ProxyAuthenticationRequired

-- |A generic 400 response
errorBadRequest :: HStream s => IO (Response s)
errorBadRequest = return $ err_response BadRequest
