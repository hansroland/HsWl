module Wayland.Client.XdgToplevel where

import Wayland.Wire.Protocol
import Control.Monad.State.Strict

-- Define callback functions for the xdg_toplevel object
xdgToplevelListener :: XdgToplevelListener
xdgToplevelListener = XdgToplevelListener (Just toplevelConfigure) Nothing

toplevelConfigure :: TxdgToplevelConfigure
toplevelConfigure _ width height states = do
  liftIO $ putStrLn ("Received XdgToplevelConfigure event. width: "
      <> show width <> " height:" <> show height <> " states:" <> show states)
