-- tests/WebFrameworksTest.hs
module WebFrameworksTest where

import Test.Hspec
import Test.HUnit
import qualified Data.ByteString.Lazy.Char8 as BSL
import Network.HTTP.Types
import Network.Wai
import WebFrameworks

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Routing" $ do
    it "should match a route" $ do
      let app = webApp $ do
            get "/foo" $ do
              respond "foo"
      res <- runRequest app (methodGet "/foo")
      res `shouldBe` Response { status = status200, responseBody = BSL.pack "foo" }

    it "should not match a route" $ do
      let app = webApp $ do
            get "/foo" $ do
              respond "foo"
      res <- runRequest app (methodGet "/bar")
      res `shouldBe` Response { status = status404, responseBody = BSL.pack "Not Found" }

  describe "Websocket" $ do
    it "should establish a websocket connection" $ do
      let app = webApp $ do
            websocket "/ws" $ do
              respond $ BSL.pack "Hello, world!"
      res <- runRequest app (methodGet "/ws")
      res `shouldBe` Response { status = status101, responseBody = BSL.pack "Hello, world!" }

  describe "Async I/O" $ do
    it "should run a request concurrently" $ do
      let app = webApp $ do
            get "/foo" $ do
              respond "foo"
            get "/bar" $ do
              respond "bar"
      res <- runRequest app (methodGet "/foo")
      res1 <- runRequest app (methodGet "/bar")
      res `shouldBe` res1

-- | Helper function to run a request with the given method and path
runRequest :: Application -> Request -> IO Response
runRequest app req = do
  let (status, res) = run app req
  return res

-- | Mock application for testing
webApp :: Application
webApp = do
  -- TODO: handle multiple routes
  get "/foo" $ do
    respond "foo"
  get "/bar" $ do
    respond "bar"
  websocket "/ws" $ do
    respond $ BSL.pack "Hello, world!"

-- | Mock request for testing
type Request = (Method, String)

data Method = GET | POST | PUT | DELETE
  deriving Show

instance Show Request where
  show (GET, path) = "GET " ++ path
  show (POST, path) = "POST " ++ path
  show (PUT, path) = "PUT " ++ path
  show (DELETE, path) = "DELETE " ++ path

-- | Mock response for testing
type Response = (Int, BSL.ByteString)

-- | Mock status code for testing
status200, status404, status101 :: Int
status200 = 200
status404 = 404
status101 = 101