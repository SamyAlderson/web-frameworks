module Routing where

import WebFramework.Types
import WebFramework.Monad
import Network.Wai (Application, Middleware)
import Network.HTTP.Types (status200, status404)
import Data.Map (Map, alter, insert, empty)
import Control.Monad (when, foldM)

data Route = Route
  { routePath :: String
  , routeHandler :: Handler
  }

type Routes = Map String Handler

insertRoute :: String -> Handler -> Routes -> Routes
insertRoute path handler routes = alter f routes
  where
    f Nothing = Just handler
    f (Just h) = Just $ handler `mappend` h

matchRoute :: String -> Routes -> Maybe (Handler, Routes)
matchRoute path routes = foldM f ( Nothing, routes ) (reverse (Data.Map.assocs routes))
  where
    f ( Just handler, r) (_, _, routePath, routeHandler) 
      | path == routePath = Just ( routeHandler `mappend` handler, r )
      | otherwise = (Just handler, r)

handleRequest :: String -> Routes -> Request -> (Response, Routes)
handleRequest path routes req =
  case matchRoute path routes of
    Just (handler, newRoutes) -> ( runHandler handler req, newRoutes )
    _ -> ( response404, routes )
  where
    response404 = ( response status404 [], routes )

type Middleware = Application -> Application

addRoutes :: Routes -> Middleware
addRoutes routes app req sendResponse =
  case matchRoute (pathInfo req) routes of
    Just (handler, newRoutes) -> handler req `mappend` (addRoutes newRoutes app)
    _ -> app req sendResponse

runRoutes :: Middleware -> Application
runRoutes middleware = middleware (waiApp)

-- For development purposes only
debugRoutes :: Routes -> Middleware
debugRoutes routes app req sendResponse =
  let log = print (matchRoute (pathInfo req) routes)
  in app req sendResponse

```

```haskell
-- Test file for Routing module
import Test.Hspec
import Test.Hspec.Core.Spec
import Network.HTTP.Types (status200, status404)
import Data.Map (empty)
import Routing (Routes, runRoutes, addRoutes, matchRoute, handleRequest)

spec :: Spec
spec = do
  describe "routing" $ do
    it "should match route with exact path" $ do
      let routes = insertRoute "/hello" (return $ response status200 []) empty
          req = request "GET" "/hello"
          (res, _) = handleRequest "/hello" routes req
      res `shouldBe` (response status200 [], empty)
    it "should match route with prefix" $ do
      let routes = insertRoute "/hello/world" (return $ response status200 []) empty
          req = request "GET" "/hello/newworld"
          (res, _) = handleRequest "/hello/world" routes req
      res `shouldBe` (response status200 [], empty)
    it "should return 404 for unmatched route" $ do
      let routes = empty
          req = request "GET" "/hello"
          (res, _) = handleRequest "/hello" routes req
      res `shouldBe` (response status404 [], empty)