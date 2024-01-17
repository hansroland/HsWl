{-# LANGUAGE OverloadedStrings #-}

module Main where


import Wire
-- import Mayland


import Types
import Client
import ClientSupport
import qualified Network.Socket             as Socket
import qualified Control.Monad.State.Strict as ST
import qualified Data.Text.IO               as TIO

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  -- Socket.connectToServer
  connectToServer runClient

-- ---------------------------------------------------------------------------
-- Real Client Code
-- ---------------------------------------------------------------------------
runClient :: Socket.Socket -> ClMonad ()
runClient serverSock = do
    displayGetRegistry
    displaySync
    setDisplayListener myDisplayListener
    setRegistryListener myRegistryListener
    setCallbackListener myCallbackListener
    sendRequests serverSock

    socketLoop serverSock
    return ()


-- Define the callbacks for the Display events
myDisplayListener :: WlDisplayListener
myDisplayListener = WlDisplayListener
  (Just myDisplayError)
  (Just myDisplayDeleteId)

myDisplayError :: TwlDisplayError
myDisplayError _obj _code message = do
  ST.liftIO $ TIO.putStrLn ("wlDisplayError: " <> getString message) --TODO show also code

-- When a client deletes an object that it had created,
-- 	the server will send this event to acknowledge that it has
-- 	seen the delete request. When the client receives this event,
-- 	it will know that it can safely reuse the object ID.
myDisplayDeleteId :: TwlDisplayDeleteId
myDisplayDeleteId obj = do
    ST.liftIO $ putStrLn $ "GOT myDisplayDeleteId " <> show obj
    removeActiveIfac $ fromIntegral obj

-- Define the callbacks for the RegisteryListener events
myRegistryListener :: WlRegistryListener
myRegistryListener = WlRegistryListener
    (Just myRegistryGlobal)
    (Just myRegistryGlobalRemove )

-- | Event: announce global object opc:0
  -- wlRegistryGlobal :: ClState ->  WUint -> WString -> WUint -> IO ()
myRegistryGlobal :: TwlRegistryGlobal
myRegistryGlobal name interface version = do
    ST.liftIO $ putStrLn $ "wlRegistryGlobal: "
      <> show name
      <> " " <> show name
      <> " " <> show interface
      <> " " <> show version

    case interface of
      WString "wl_compositor" -> do
        ST.liftIO $ putStrLn "GOTCHA REGISTER compositor"
        bindToInterface (fromIntegral name) interface

      WString "wl_shm" -> do
        ST.liftIO $ putStrLn "GOTCHA REGISTER wl_shm"
        bindToInterface (fromIntegral name) interface

      _ -> pure()
    {-
    if (strcmp(interface, wl_compositor_interface.name) == 0) {
        state->compositor = wl_registry_bind(
           registry, name, &wl_compositor_interface, 4);
    -}


    pure ()

myRegistryGlobalRemove :: TwlRegistryGlobalRemove
myRegistryGlobalRemove obj = do
    ST.liftIO $ putStrLn $ "GOT myRegistryGlobalRemove " <> show obj

-- Define callback function for the wl_callback object
myCallbackListener :: WlCallbackListener
myCallbackListener = WlCallbackListener (Just myCallbackDone)


myCallbackDone :: TwlCallbackDone
myCallbackDone wuint =
  ST.liftIO $ putStrLn $ "Received callback done for " <> show wuint

