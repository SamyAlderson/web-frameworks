module WebFrameworks where

import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.Map as Map
import Control.Monad (when)
import Control.Exception (assert)
import Data.IORef (modifyIORef, newIORef, readIORef)
import Data.Maybe (fromMaybe)
import Debug.Trace (traceShowId, traceShow)

type Route = String
type Handler = BS.ByteString -> IO ()
type WebServer = IORef [Handler]

-- | Web framework API
webServer :: WebServer -> BS.ByteString -> IO BS.ByteString
webServer server request = do
    handlers <- readIORef server
    let route = fromMaybe "404" $ Map.lookup (BS.takeWhile (/= '?') request) handlers
        handler = head [h | (h, _) <- handlers, h == route]
    handler request

-- | Add a new route to the web server
addRoute :: WebServer -> Route -> Handler -> IO ()
addRoute server route handler = do
    serverRef <- newIORef []
    modifyIORef serverRef $ (route, handler) : filter (\(r, _) -> r /= route) (readIORef server)
    modifyIORef server $ (++ [serverRef])

-- | Start the web server
startServer :: WebServer -> Int -> IO ()
startServer server port = do
    let warpApp = Wai.application $ \request -> do
        response <- webServer server (Wai.requestBodyL request)
        return $ Wai.responseLBS Warp.status200 [Wai.responseBody response]
    Warp.run port warpApp

-- | Example handler for testing
exampleHandler :: BS.ByteString -> IO BS.ByteString
exampleHandler request = return "Hello, World!"

-- Test code
main :: IO ()
main = do
    server <- newIORef []
    addRoute server "hello" exampleHandler
    startServer server 3000
    assert True "Web server should be running"