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
    setDisplayListener myDisplayListener
    setRegistryListener myRegistryListener
    setCallbackListener myCallbackListener
    displaySync
    sendRequests serverSock
    socketRead serverSock

{-
    state.wl_surface = wl_compositor_create_surface(state.wl_compositor);
    state.xdg_surface = xdg_wm_base_get_xdg_surface(
            state.xdg_wm_base, state.wl_surface);
    printf ("RSX Before xdg_surface_add_listener\n");
    xdg_surface_add_listener(state.xdg_surface, &xdg_surface_listener, &state);
    printf ("RSX Before xdg_surface_get_toplevel\n");
    state.xdg_toplevel = xdg_surface_get_toplevel(state.xdg_surface);
    xdg_toplevel_set_title(state.xdg_toplevel, "Example client");
    printf ("RSX Before wl_surface_commit\n");
    wl_surface_commit(state.wl_surface);
-}


    setShmListener myShmListener
    -- TODO: cleanup name setgWmBaseListener
    setgWmBaseListener myXdgWmBaseListener
    setgToplevelListener myXdgToplevelListener
    surface <- compositorCreateSurface
    xdgSurface <- xdgGetXdgSurface surface
    topLevel <- surfaceAssignToplevel        -- xdgSurface
    toplevelSetTitle $ WString "Example client"
    surfaceCommit
    sendRequests serverSock
    socketRead serverSock

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
-- --------------------------------------------------------------------


-- Define the callbacks for the RegisteryListener events
myRegistryListener :: WlRegistryListener
myRegistryListener = WlRegistryListener
    (Just myRegistryGlobal)
    (Just myRegistryGlobalRemove )

-- | Event: announce global object opc:0
  -- wlRegistryGlobal :: ClState ->  WUint -> WString -> WUint -> IO ()
myRegistryGlobal :: TwlRegistryGlobal
myRegistryGlobal name interface version = do
   {-
    ST.liftIO $ putStrLn $ "myRegistryGlobal: "
      <> show name
      <> " " <> show name
      <> " " <> show interface
      <> " " <> show version
   -}

    case interface of
      WString "wl_compositor" -> do
        ST.liftIO $ putStrLn "GOTCHA REGISTER compositor"
        bindToInterface (fromIntegral name) interface version

      WString "wl_shm" -> do
        ST.liftIO $ putStrLn "GOTCHA REGISTER wl_shm"
        bindToInterface (fromIntegral name) interface version

      WString "xdg_wm_base" -> do
        ST.liftIO $ putStrLn "GOTCHA REGISTER xdg_wm_base"
        bindToInterface (fromIntegral name) interface version
      _ -> pure()

    pure ()

myRegistryGlobalRemove :: TwlRegistryGlobalRemove
myRegistryGlobalRemove obj = do
    ST.liftIO $ putStrLn $ "GOT myRegistryGlobalRemove " <> show obj
-- --------------------------------------------------------------------


-- Define callback function for the wl_callback object
myCallbackListener :: WlCallbackListener
myCallbackListener = WlCallbackListener (Just myCallbackDone)


myCallbackDone :: TwlCallbackDone
myCallbackDone wuint =
  ST.liftIO $ putStrLn $ "Received callback done for " <> show wuint
-- --------------------------------------------------------------------

-- Define callback function for the wl_shm object
myShmListener :: WlShmListener
myShmListener = WlShmListener (Just myShmFormat)

myShmFormat :: TwlShmFormat
myShmFormat format =
  ST.liftIO $ putStrLn ("Received possible wl_shm format event: " <> show format)
  -- TODO Use the enum entries
-- --------------------------------------------------------------------


-- Define callback functions for the xdg_wm_base object
myXdgWmBaseListener :: XdgWmBaseListener
myXdgWmBaseListener = XdgWmBaseListener (Just myXdgWmBasePing)

myXdgWmBasePing :: TxdgWmBasePing
myXdgWmBasePing serial = do
  ST.liftIO $ putStrLn "Received xdg_wm_base_ping"
  xdgSendPongAnswer serial

-- --------------------------------------------------------------------


-- Define callback functions for the xdg_toplevel object
myXdgToplevelListener :: XdgToplevelListener
myXdgToplevelListener = XdgToplevelListener (Just myXdgToplevelConfigure) Nothing

myXdgToplevelConfigure :: TxdgToplevelConfigure
myXdgToplevelConfigure width height states = do
  ST.liftIO $ putStrLn ("Received XdgToplevelConfigure event. width: "
          <> show width <> " height:" <> show height <> " states:" <> show states)
