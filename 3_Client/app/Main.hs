{-# LANGUAGE OverloadedStrings #-}

module Main where

import Wayland.Client.Client
import Wayland.Wire.Wire

main :: IO ()
main = do
  connectToServer runClient



