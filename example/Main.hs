{-# LANGUAGE CPP #-}

module Main where

import Example

#ifndef ghcjs_HOST_OS

import Control.Lens ((^.))
import Control.Monad (void)
import Language.Javascript.JSaddle
import Language.Javascript.JSaddle.WebSockets
import Network.Wai.Handler.Warp
import Network.Wai.Application.Static
import Network.Wai
import Network.WebSockets

#endif

main :: IO ()

#ifdef ghcjs_HOST_OS

main = example

#else

port :: Int
port = 3708

-- | Start the warp server
main = do
  putStrLn $ "starting warp server: http://localhost:" ++ show port
  runApp id $ return ()

-- | Restart the warp server and tell any connected clients to refresh
debug :: IO ()
debug = do
  debugWrapper runApp
  putStrLn $ "restarting warp server: http://localhost:" ++ show port

runApp :: Middleware -> JSM ()-> IO ()
runApp middleware preApp = do
  server <- jsaddleWithAppOr defaultConnectionOptions
    (preApp >> app) (middleware static)
  runSettings settings server
  where
    settings = setPort port $ setTimeout 3600 $ defaultSettings
    static = staticApp $ defaultFileServerSettings dir
    app = makeHead >> example >> syncPoint
    dir = "lib"

-- Needed for non js targets, since the js-sources in the cabal file are not
-- linked
makeHead :: JSM ()
makeHead = do

  document <- jsg "document"

  jquery <- document ^. js1 "createElement" "script"
  jquery ^. jss "src" "jquery.min.js"
  void $ document ^. js "head" ^. js1 "appendChild" jquery

  semantic <- document ^. js1 "createElement" "script"
  semantic ^. jss "src" "semantic.min.js"
  void $ document ^. js "head" ^. js1 "appendChild" semantic

  -- Ugly hack: two syncPoints seem to give enough time for jquery / semantic to
  -- load in before the remainder of the app is loaded.
  syncPoint
  syncPoint

#endif
