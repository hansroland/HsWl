{-# LANGUAGE OverloadedStrings #-}
module Wayland.Client.Shm where

import Wayland.Wire.Protocol

import Control.Monad.State.Strict



-- Define callback function for the wl_shm object
wlShmListener :: WlShmListener
wlShmListener = WlShmListener (Just shmFormat)

shmFormat :: TwlShmFormat
shmFormat _ format =
  liftIO $ putStrLn ("Received possible wl_shm format event: " <> show format)
