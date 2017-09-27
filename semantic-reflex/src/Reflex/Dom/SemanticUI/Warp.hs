{-# LANGUAGE TemplateHaskell #-}

-- | This module is to help with fast development cycles. Use 'server' or
-- 'daemon' to start a warp server on the given port where you can see your
-- app.
module Reflex.Dom.SemanticUI.Warp where

import Control.Lens ((^.))
import Control.Monad (void)
import Language.Javascript.JSaddle
import Language.Javascript.JSaddle.WebSockets
import Network.Wai.Handler.Warp
import Network.Wai.Application.Static
import Network.Wai
import Network.WebSockets

import Data.FileEmbed

-- | Start the warp server
server :: Int -> JSM () -> IO ()
server port app = do
  putStrLn $ "starting warp server: http://localhost:" ++ show port
  runApp port app id $ return ()

-- | Restart the warp server and tell any connected clients to refresh
daemon :: Int -> JSM () -> IO ()
daemon port app = do
  debugWrapper (runApp port app)
  putStrLn $ "restarting warp server: http://localhost:" ++ show port

-- | We serve the standard jsaddle-warp app but with a static server for the
-- javascript and theme folder.
runApp :: Int -> JSM () -> Middleware -> JSM ()-> IO ()
runApp port mainApp middleware preApp = do
  print dir
  server <- jsaddleWithAppOr defaultConnectionOptions
    (preApp >> app) (middleware static)
  runSettings settings server
  where
    settings = setPort port $ setTimeout 3600 $ defaultSettings
    static = staticApp $ defaultFileServerSettings dir
    app = makeHead >> mainApp >> syncPoint
    dir = $(strToExp =<< makeRelativeToProject "lib")

-- Needed for non js targets, since the js-sources in the cabal file are not
-- linked
makeHead :: JSM ()
makeHead = do

  document <- jsg "document"

  jquery <- document ^. js1 "createElement" "script"
  jquery ^. jss "src" "jquery-3.2.1.min.js"
  void $ document ^. js "head" ^. js1 "appendChild" jquery

  semantic <- document ^. js1 "createElement" "script"
  semantic ^. jss "src" "semantic.min.js"
  void $ document ^. js "head" ^. js1 "appendChild" semantic
