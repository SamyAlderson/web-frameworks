-- | Async I/O implementation for web-frameworks.
-- | Copyright (c) 2026, [Your Name]
-- | License: MIT

module WebFrameworks.Async where

import Control.Monad (forever, liftM2)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Network.Wai.Handler.WebSockets (websocketsOr)
import WebSockets (WS)

-- | Type alias for async IO action
type AsyncIO a = IO a

-- | Run an async IO action
runAsyncIO :: AsyncIO a -> IO a
runAsyncIO = (`catch` (\e -> print e >> return ()))

-- | Run multiple async IO actions concurrently
runConcurrently :: [AsyncIO a] -> IO [a]
runConcurrently = mapM runAsyncIO

-- | Run an async IO action repeatedly
runRepeatedly :: AsyncIO a -> Int -> IO [a]
runRepeatedly action 0 = return []
runRepeatedly action n = do
  a <- runAsyncIO action
  bs <- runRepeatedly action (n-1)
  return (a:bs)

-- | WebSocket handler
websocketHandler :: WS -> IO ()
websocketHandler ws = do
  liftIO $ print "WebSocket connected"
  forever $ do
    msg <- liftIO $ WS.receiveData ws
    liftIO $ print msg
    liftIO $ WS.sendTextData ws "Hello from server!"

-- | WebSocket endpoint
websocketEndpoint :: IO ()
websocketEndpoint = do
  liftIO $ print "WebSocket endpoint started"
  websocketsOr forever websocketHandler WS.listen 1234
  liftIO $ print "WebSocket endpoint stopped"

-- | Tests
main :: IO ()
main = do
  websocketEndpoint
  -- This should print "Hello from server!" repeatedly
  runConcurrently [websocketEndpoint, websocketEndpoint]