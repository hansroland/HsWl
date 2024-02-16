{-# LANGUAGE OverloadedStrings #-}
module Wayland.Client.Seat where

import Wayland.Wire.Protocol

import           Control.Monad.State.Strict
import           Data.Bits

-- --------------------------------------------------------------------
-- Seat
-- --------------------------------------------------------------------
wlSeatListener :: WlSeatListener
wlSeatListener = WlSeatListener
                      (Just seatCapabilities)
                      (Just seatName)

seatCapabilities  :: TwlSeatCapabilities
seatCapabilities wobj capa = do
    liftIO $ putStrLn ("WlSeatCapabilities called capa: " <> show capa)
    -- Bitwise 1 = pointer
    -- Bitwise 2 = keyboard
    if capa .&. 1 == 1
      then do
        liftIO $ putStrLn "Pointer is available"
        _pointer <- wlSeatGetPointer wobj "wl_pointer"
        pure ()
      else liftIO $ putStrLn "Pointer is missing"
    if capa .&. 2 == 2
      then do
        liftIO $ putStrLn "Terminal is available"
      else liftIO $ putStrLn "Terminal is missing"
    if capa .&. 4 == 4
      then do
        liftIO $ putStrLn "Touch is available"
      else liftIO $ putStrLn "Touch is missing"
    pure ()


seatName :: TwlSeatName
seatName _ name = do
   liftIO $ putStrLn ("WlSeatName name: " <> show name)
   pure ()


wlPointerListener :: WlPointerListener
wlPointerListener = WlPointerListener
    (Just pointerEnter)
    (Just pointerLeave)
    (Just pointerMotion)
    (Just pointerButton)
    (Just pointerAxis)
    (Just pointerFrame)
    (Just pointerAxisSource)
    (Just pointerAxisStop)
    (Just pointerAxisDiscrete)

pointerEnter :: TwlPointerEnter
pointerEnter _wobj _serial _surface _surfaceX _surfaceY = do
    liftIO $ putStrLn "wlPointerEnter"
    pure ()

pointerLeave :: TwlPointerLeave
pointerLeave _wobj _serial _surface = do
    liftIO $ putStrLn "wlPointerLeave"
    pure ()

pointerMotion :: TwlPointerMotion
pointerMotion _wobj _time _surfaceX _surfaceY = do
    pure ()

pointerButton :: TwlPointerButton
pointerButton _wobj _serial _time _button _state = do
    liftIO $ putStrLn "wlPointerButton"
    pure ()

pointerAxis :: TwlPointerAxis
pointerAxis _wobj _time _axis _value = do
    pure ()

pointerFrame :: TwlPointerFrame
pointerFrame _wobj = do
    pure ()

pointerAxisSource :: TwlPointerAxisSource
pointerAxisSource _wobj _axisSource = do
    pure ()

pointerAxisStop :: TwlPointerAxisStop
pointerAxisStop _wobj _time _axis = do
    pure ()

pointerAxisDiscrete :: TwlPointerAxisDiscrete
pointerAxisDiscrete _wobj _axis _discrete = do
    pure ()
