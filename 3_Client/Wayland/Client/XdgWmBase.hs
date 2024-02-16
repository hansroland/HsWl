module Wayland.Client.XdgWmBase where

import Wayland.Wire.Protocol
import Control.Monad.State.Strict

-- Define callback functions for the xdg_wm_base object
xdgWmBaseListener :: XdgWmBaseListener
xdgWmBaseListener = XdgWmBaseListener (Just wmBasePing)

wmBasePing :: TxdgWmBasePing
wmBasePing wmBase serial = do
  liftIO $ putStrLn "XDG PING Called"
  xdgWmBasePong wmBase serial

