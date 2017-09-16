{-# LANGUAGE CPP #-}

module Main where

import Example

#ifndef ghcjs_HOST_OS

import Control.Lens ((^.))
import Language.Javascript.JSaddle
import Language.Javascript.JSaddle.WebSockets
import Network.Wai.Handler.Warp
import Network.Wai.Application.Static
import Network.WebSockets

#endif

main :: IO ()

#ifdef ghcjs_HOST_OS

main = example

#else

main = do
  putStrLn $ "http://localhost:" ++ show port
  server <- jsaddleWithAppOr defaultConnectionOptions app static
  runSettings settings server
  where
    settings = setPort port $ setTimeout 3600 $ defaultSettings
    static = staticApp $ defaultFileServerSettings dir
    app = makeHead >> example >> syncPoint
    port = 3708
    dir = "lib"

-- Needed for non js targets, since the js-sources in the cabal file are not
-- linked
makeHead :: JSM ()
makeHead = do

  document <- jsg "document"

  jquery <- document ^. js1 "createElement" "script"
  jquery ^. jss "src" "jquery.min.js"
  document ^. js "head" ^. js1 "appendChild" jquery

  semantic <- document ^. js1 "createElement" "script"
  semantic ^. jss "src" "semantic.min.js"
  document ^. js "head" ^. js1 "appendChild" semantic

  -- Ugly hack: two syncPoints seem to give enough time for jquery / semantic to
  -- load in before the remainder of the app is loaded.
  syncPoint
  syncPoint

#endif
