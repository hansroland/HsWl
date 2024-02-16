{-# LANGUAGE OverloadedStrings #-}
module Wayland.Client.Display where

import Wayland.Wire.Protocol
import Wayland.Client.Types

import qualified Data.Text.IO               as TIO
import Control.Monad.State.Strict
import qualified Data.EnumMap.Strict        as Map




-- Define the callbacks for the Display events

wlDisplayListener :: WlDisplayListener
wlDisplayListener = WlDisplayListener
  (Just displayError)
  (Just displayDeleteId)

displayError :: TwlDisplayError
displayError _ _obj2 _code message = do
  liftIO $ TIO.putStrLn ("wlDisplayError: " <> getString message) --TODO show also code

-- When a client deletes an object that it had created,
-- 	the server will send this event to acknowledge that it has
-- 	seen the delete request. When the client receives this event,
-- 	it will know that it can safely reuse the object ID.
displayDeleteId :: TwlDisplayDeleteId
displayDeleteId _ obj = do
    liftIO $ putStrLn $ "GOT myDisplayDeleteId " <> show obj
    removeActiveIfac $ fromIntegral obj

-- | removeActiveIfac remove an interface from the map of active interfaces
removeActiveIfac :: WObj -> ClMonad ()
removeActiveIfac wobj  = do
    st <- get
    let newacts =  Map.delete wobj $ clActiveIfaces st
    liftIO $ printActiveIfaces newacts
    put $ st {clActiveIfaces = newacts }

