module Wayland.Client.Buffer where

import Wayland.Wire.Protocol
import Control.Monad.State.Strict

wlBufferListener :: WlBufferListener
wlBufferListener = WlBufferListener (Just bufferRelease)

bufferRelease :: TwlBufferRelease
bufferRelease wobj = do
  liftIO $ putStrLn "Received wl_buffer_destroy event"
  wlBufferDestroy wobj
