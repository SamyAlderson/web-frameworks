# web-frameworks
A haskell web framework for building complex web apps.

## Features

* Flexible routing system
* Websocket support
* Async I/O

## Installation

    $ cabal install web-frameworks
    $ stack install web-frameworks

## Usage

```haskell
import Network.WebFrameworks

main :: IO ()
main = runWebApp "http://localhost:3000" $ do
  route "/home" $ homeHandler
  route "/ws" $ websocketHandler

homeHandler :: Request -> Response
homeHandler _ = textResponse "home page"

websocketHandler :: Request -> WebSocket ()
websocketHandler _ = send "connected!"
```

## Contributing

* Fork this repo
* Make changes
* Send pull request

## Project Structure

```
web-frameworks/
  src/
    Main.hs
    Network/WebFrameworks.hs
    Network/WebFrameworks/Routing.hs
  tests/
    Spec.hs
  README.md
  LICENSE
```

## License

This project is licensed under the MIT License.

## Copyright

2026 Samy Alderson.

Note: this is a basic framework and you'll need to add more features and tests to make it production ready.