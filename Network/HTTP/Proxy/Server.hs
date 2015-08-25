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
import Control.Monad.Reader

-- | The proxy monad: Reader (for settings) over IO
type Proxy s a = ReaderT (Settings s) IO a

type ProxyResponse s = Proxy s (Response s)

-- | Proxy entry-point. Spawns a new proxy server.
proxyMain :: forall s. HStream s => Settings s -> IO ()
proxyMain settings = (`runReaderT` settings) $
  do mhname <- asks hostname
     hname <- case mhname of
       Nothing -> lift getHostName
       Just hostn -> return hostn
     log <- asks logger
     port <- asks portnum
     let config = defaultConfig {srvPort = fromInteger port
                                ,srvHost = hname
                                ,srvLog  = log}
     myLogInfo $ "Proxy server started on port " ++ (show port)
     lift $ serverWith config (proxyHandler settings)

myLogInfo :: String -> Proxy s ()
myLogInfo s = asks logger >>= \l -> lift (logInfo l 0 s)

myLogWarning :: String -> Proxy s ()
myLogWarning s = asks logger >>= \l -> lift (logWarning l s)

myLogError :: String -> Proxy s ()
myLogError s = asks logger >>= \l -> lift (logError l s)

proxyHandler :: HStream s => Settings s -> Handler s
proxyHandler settings _ _ request = (`runReaderT` settings) $ do
  -- check that the request is authorized
  myLogInfo "Checking request authorization"
  authorized <- lift $ isAuthorized settings request
  if authorized then processRequest settings request
                else do myLogWarning $ "Rejecting an unauthorized request: "
                           ++ (show request)
                        errorProxyUnauthorized

-- | Processes the request; this is the main proxy procedure
processRequest :: HStream s => Settings s -> Request s -> ProxyResponse s
processRequest settings request = do
  -- modify the request
  myLogInfo "Modifying the request"
  modRequest <- lift $ requestModifier settings request
  -- check the cache
  myLogInfo "Querying cache"
  mCachedResponse <- lift $ queryCache (cache settings) modRequest
  case mCachedResponse of
    -- found in cache: return
    Just response -> do
      myLogInfo "Cache hit: returning cached response"
      return response
    -- not found: fetch it from a remote server, invoke the
    -- 'responseModifier' hook, record in cache and return
    Nothing       -> do
      myLogInfo "Cache miss: forwarding the request"
      response <- fetch modRequest
      myLogInfo "Modifying the response"
      modResponse <- lift $ responseModifier settings request response
      myLogInfo "Caching the modified response"
      lift $ recordInCache (cache settings) request modResponse
      return modResponse

fetch :: HStream s => Request s -> ProxyResponse s
fetch request = do
  result <- lift $ simpleHTTP request
  case result of
    Left err  -> do myLogError $
                      "Connection error while fetching an external resource: "
                      ++ show err
                    lift errorInternalServerError
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
           ,logger           :: Logger
            -- ^ A logging function. The default is 'stdLogger' from
            -- http-server.
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
                 ,logger           = stdLogger
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
errorProxyUnauthorized :: HStream s => ProxyResponse s
errorProxyUnauthorized = return $ err_response ProxyAuthenticationRequired

-- |A generic 400 response
errorBadRequest :: HStream s => IO (Response s)
errorBadRequest = return $ err_response BadRequest
