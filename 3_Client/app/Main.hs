{-# LANGUAGE OverloadedStrings #-}

module Main where


import Protocol
-- import Mayland


import Types
import Client
import ClientSupport
import Shm

import qualified Network.Socket             as Socket
import qualified Control.Monad.State.Strict as ST
import qualified Data.Text.IO               as TIO

import Foreign (ForeignPtr, nullPtr, withForeignPtr, newForeignPtr_, pokeElemOff)
import Data.Word (Word32)
import Control.Monad

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
    _ <- wlDisplayGetRegistry cWlRegistry
    setDisplayListener myDisplayListener
    setRegistryListener myRegistryListener
    setCallbackListener myCallbackListener
    _ <- wlDisplaySync cWlCallback
    sendRequests serverSock
    socketRead serverSock

    -- https://gitlab.freedesktop.org/wayland/wayland/-/blob/main/src/wayland-shm.c?ref_type=heads
    -- https://en.m.wikipedia.org/wiki/Unix_domain_socket


    setShmListener myShmListener
    -- TODO: cleanup name setgWmBaseListener
    setgWmBaseListener myXdgWmBaseListener
    setgToplevelListener myXdgToplevelListener
    setgSurfaceListener myXdgSurfaceListener
    setBufferListener myBufferListener
    surface <- wlCompositorCreateSurface cWlSurface
    xdgSurface <- xdgWmBaseGetXdgSurface cXdgSurface surface
    topLevel <- xdgSurfaceGetToplevel cXdgToplevel
    xdgToplevelSetTitle $ WString "Example client"
    wlSurfaceCommit

    let loop = do
          ST.liftIO $ putStrLn "Send - Read Loop"
          sendRequests serverSock
          socketRead serverSock
          loop

    _ <- loop

    -- TODO a nice disconnect from the server
    -- wlDisplayDisconnect ptrDisplay
    ST.liftIO $ putStrLn "disconnected!"


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
        _ <- rsxRegistryBind (fromIntegral name) interface version cWlCompositor
        pure ()

      WString "wl_shm" -> do
        ST.liftIO $ putStrLn "GOTCHA REGISTER wl_shm"
        _ <- rsxRegistryBind (fromIntegral name) interface version cWlShm
        pure ()

      WString "xdg_wm_base" -> do
        ST.liftIO $ putStrLn "GOTCHA REGISTER xdg_wm_base"
        _ <- rsxRegistryBind (fromIntegral name) interface version cXdgWmBase
        pure ()

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
  ST.liftIO $ putStrLn "XDG PING Called"
  xdgWmBasePong serial

-- --------------------------------------------------------------------

myBufferListener :: WlBufferListener
myBufferListener = WlBufferListener (Just myWlBufferRelease)

myWlBufferRelease :: TwlBufferRelease
myWlBufferRelease = do
  ST.liftIO $ putStrLn "Received wl_buffer_destroy event"
  wlBufferDestroy


-- Define callback functions for the xdg_toplevel object
myXdgToplevelListener :: XdgToplevelListener
myXdgToplevelListener = XdgToplevelListener (Just myXdgToplevelConfigure) Nothing

myXdgToplevelConfigure :: TxdgToplevelConfigure
myXdgToplevelConfigure width height states = do
  ST.liftIO $ putStrLn ("Received XdgToplevelConfigure event. width: "
          <> show width <> " height:" <> show height <> " states:" <> show states)
-- --------------------------------------------------------------------


myXdgSurfaceListener :: XdgSurfaceListener
myXdgSurfaceListener = XdgSurfaceListener (Just myXdgSurfaceConfigure)

myXdgSurfaceConfigure :: TxdgSurfaceConfigure
myXdgSurfaceConfigure serial = do
  ST.liftIO $ putStrLn "XDGConfigureHandler called"
  xdgSurfaceAckConfigure serial
  buffer <- drawFrame
  _ <- wlSurfaceAttach buffer 0 0
  wlSurfaceCommit
  pure ()

-- --------------------------------------------------------------------
-- Create and fill a buffer
-- --------------------------------------------------------------------
drawFrame :: ClMonad WObj
drawFrame {-shm-} = do
  ST.liftIO $ putStrLn "drawFrame active"
  let width  = 640 :: Int
      height = 480 :: Int
      stride = width * (4 :: Int)              -- 2560
      size   = stride * height                 -- 1228800  x'012C00'

  fd <- ST.liftIO $ allocateShmFile size
  ST.liftIO $ putStrLn $ "drawFrame fd: " <> show fd
  memAddr <- ST.liftIO $ mmap nullPtr (fromIntegral size)
            (cPROT_READ + cPROT_WRITE) cMAP_SHARED (fromIntegral fd) 0
  fmemAddr <- ST.liftIO $ newForeignPtr_ memAddr
  ST.liftIO $ fillBuffer fmemAddr width height
  -- myPutStrLn (" memAddr " ++ show memAddr ++ " " ++ show cMAP_FAILED)
  shmPool <- wlShmCreatePool cWlShmPool (fromIntegral fd) (fromIntegral size)
  buffer <- wlShmPoolCreateBuffer cWlBuffer 0
            (fromIntegral width) (fromIntegral height) (fromIntegral stride) wL_SHM_FORMAT_XRGB8888
  wlShmPoolDestroy
  ST.liftIO $ closeShmFile fd
  ST.liftIO $ munmap memAddr (fromIntegral size)
  pure buffer

fillBuffer :: ForeignPtr Word32 -> Int -> Int ->IO ()
fillBuffer ptr width heigth = do
    withForeignPtr ptr $ \p ->
      forM_ [0..heigth-1] $ \y ->
        forM_ [0..width-1] $ \x ->
          pokeElemOff p (x + y * width) (calcColor x y)       -- color value
    pure ()

calcColor :: Int -> Int -> Word32
calcColor x y =
  let ex = even (x `div` 10)
      ey = even (y `div` 10)
  in if (ex && ey) || (not ex && not ey)
      then 0XFF00FFFF
      else 0XFFFF0000
