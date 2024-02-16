module Wayland.Client.Callback where

import Wayland.Wire.Protocol
import Control.Monad.State.Strict

-- Define callback function for the wl_callback object
wlCallbackListener :: WlCallbackListener
wlCallbackListener = WlCallbackListener (Just callbackDone)

-- Callback for 'Done' event
callbackDone :: TwlCallbackDone
callbackDone _ wuint =
  liftIO $ putStrLn $ "Received callback done for " <> show wuint
