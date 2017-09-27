{-# LANGUAGE CPP #-}

module Main where

import Example

#ifndef ghcjs_HOST_OS

import Reflex.Dom.SemanticUI.Warp

#endif

main :: IO ()

#ifdef ghcjs_HOST_OS

main = example

#else

port :: Int
port = 3708

-- | Start the warp server
main = server port example

-- | Restart the warp server and tell any connected clients to refresh
debug :: IO ()
debug = daemon port example

#endif
