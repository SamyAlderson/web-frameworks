module Websocket where

import Control.Monad (liftM2)
import qualified Data.ByteString.Char8 as BS
import Network.WebSockets (WebSocket, acceptConnection, sendText)
import qualified Network.WebSockets as WS

-- | Handle incoming websocket connection
handleConnection :: WebSocket -> IO ()
handleConnection conn = do
  -- Read initial message from client
  msg <- WS.receiveData conn

  -- Check if it's a ping request
  if BS.take 4 msg == "ping" then do
    -- Send pong response back
    WS.sendText conn "pong"
    -- Wait for next message
    handleConnection conn
  else do
    -- Handle incoming message
    putStrLn $ "Received message: " ++ BS.unpack msg

    -- Send response back to client
    WS.sendText conn "hello, client"

    -- Close connection after sending response
    WS.sendClose conn "goodbye"

-- | Handle incoming websocket upgrade request
handleUpgrade :: BS.ByteString -> BS.ByteString -> IO ()
handleUpgrade path secConn = do
  -- Create new websocket connection
  conn <- acceptConnection secConn

  -- Run handleConnection on the new connection
  handleConnection conn

-- | Start websocket server
startServer :: Int -> IO ()
startServer port = do
  putStrLn $ "Starting websocket server on port " ++ show port
  WS.runWebSocketsApp handleUpgrade (WS.defaultSettings {
    WS.host = "localhost",
    WS.port = port
  })

-- | Main function
main :: IO ()
main = do
  startServer 8080