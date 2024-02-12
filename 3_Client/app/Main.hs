{-# LANGUAGE OverloadedStrings #-}

module Main where

import Protocol
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
import Data.Bits

main :: IO ()
main = do
  connectToServer runClient

-- ---------------------------------------------------------------------------
-- Real Client Code
-- ---------------------------------------------------------------------------
runClient :: Socket.Socket -> ClMonad ()
runClient serverSock = do
    _ <- wlDisplayGetRegistry wlDisplayWObj cWlRegistry
    setDisplayListener myDisplayListener
    setRegistryListener myRegistryListener
    setCallbackListener myCallbackListener
    _ <- wlDisplaySync wlDisplayWObj cWlCallback
    sendRequests serverSock
    socketRead serverSock

    -- https://gitlab.freedesktop.org/wayland/wayland/-/blob/main/src/wayland-shm.c?ref_type=heads
    -- https://en.m.wikipedia.org/wiki/Unix_domain_socket

    -- https://bugaevc.gitbooks.io/writing-wayland-clients/content/about-this-book/what-is-wayland.html


    setShmListener myShmListener
    -- TODO: cleanup name setgWmBaseListener
    setgWmBaseListener myXdgWmBaseListener
    setgToplevelListener myXdgToplevelListener
    setgSurfaceListener myXdgSurfaceListener
    setBufferListener myBufferListener

    setSeatListener myWlSeatListener
    setPointerListener myWlPointerListener

    compositor <- getWObjForGlobal cWlCompositor
    surface <- wlCompositorCreateSurface compositor cWlSurface
    wmBase <- getWObjForGlobal cXdgWmBase
    xdgSurface <- xdgWmBaseGetXdgSurface wmBase cXdgSurface surface
    addLinkTo (surface, cWlSurface)  xdgSurface
    topLevel <- xdgSurfaceGetToplevel xdgSurface cXdgToplevel
    xdgToplevelSetTitle topLevel $ WString "Example client"
    wlSurfaceCommit surface

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
myDisplayError _ _obj2 _code message = do
  ST.liftIO $ TIO.putStrLn ("wlDisplayError: " <> getString message) --TODO show also code

-- When a client deletes an object that it had created,
-- 	the server will send this event to acknowledge that it has
-- 	seen the delete request. When the client receives this event,
-- 	it will know that it can safely reuse the object ID.
myDisplayDeleteId :: TwlDisplayDeleteId
myDisplayDeleteId _ obj = do
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
myRegistryGlobal wobj name interface version = do
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
        _ <- rsxRegistryBind wobj (fromIntegral name) interface version cWlCompositor
        pure ()

      WString "wl_shm" -> do
        ST.liftIO $ putStrLn "GOTCHA REGISTER wl_shm"
        _ <- rsxRegistryBind wobj (fromIntegral name) interface version cWlShm
        pure ()

      WString "xdg_wm_base" -> do
        ST.liftIO $ putStrLn "GOTCHA REGISTER xdg_wm_base"
        _ <- rsxRegistryBind wobj (fromIntegral name) interface version cXdgWmBase
        pure ()

      WString "wl_seat" -> do
        ST.liftIO $ putStrLn "GOTCHA REGISTER wl_seat"
        _ <- rsxRegistryBind wobj (fromIntegral name) interface version cWlSeat
        pure ()

      _ -> pure()

    pure ()

myRegistryGlobalRemove :: TwlRegistryGlobalRemove
myRegistryGlobalRemove _ obj = do
    ST.liftIO $ putStrLn $ "GOT myRegistryGlobalRemove " <> show obj
-- --------------------------------------------------------------------


-- Define callback function for the wl_callback object
myCallbackListener :: WlCallbackListener
myCallbackListener = WlCallbackListener (Just myCallbackDone)


myCallbackDone :: TwlCallbackDone
myCallbackDone _ wuint =
  ST.liftIO $ putStrLn $ "Received callback done for " <> show wuint
-- --------------------------------------------------------------------

-- Define callback function for the wl_shm object
myShmListener :: WlShmListener
myShmListener = WlShmListener (Just myShmFormat)

myShmFormat :: TwlShmFormat
myShmFormat _ format =
  ST.liftIO $ putStrLn ("Received possible wl_shm format event: " <> show format)
  -- TODO Use the enum entries
-- --------------------------------------------------------------------


-- Define callback functions for the xdg_wm_base object
myXdgWmBaseListener :: XdgWmBaseListener
myXdgWmBaseListener = XdgWmBaseListener (Just myXdgWmBasePing)

myXdgWmBasePing :: TxdgWmBasePing
myXdgWmBasePing wmBase serial = do
  ST.liftIO $ putStrLn "XDG PING Called"
  xdgWmBasePong wmBase serial

-- --------------------------------------------------------------------

myBufferListener :: WlBufferListener
myBufferListener = WlBufferListener (Just myWlBufferRelease)

myWlBufferRelease :: TwlBufferRelease
myWlBufferRelease wobj = do
  ST.liftIO $ putStrLn "Received wl_buffer_destroy event"
  wlBufferDestroy wobj


-- Define callback functions for the xdg_toplevel object
myXdgToplevelListener :: XdgToplevelListener
myXdgToplevelListener = XdgToplevelListener (Just myXdgToplevelConfigure) Nothing

myXdgToplevelConfigure :: TxdgToplevelConfigure
myXdgToplevelConfigure _ width height states = do
  ST.liftIO $ putStrLn ("Received XdgToplevelConfigure event. width: "
      <> show width <> " height:" <> show height <> " states:" <> show states)
-- --------------------------------------------------------------------

myXdgSurfaceListener :: XdgSurfaceListener
myXdgSurfaceListener = XdgSurfaceListener (Just myXdgSurfaceConfigure)

myXdgSurfaceConfigure :: TxdgSurfaceConfigure
myXdgSurfaceConfigure wobj serial = do
  ST.liftIO $ putStrLn ("XDGConfigureHandler called wobj:" <> show wobj)
  xdgSurfaceAckConfigure wobj serial
  shm <- getWObjForGlobal cWlShm
  buffer <- drawFrame shm
  surface <- getWObjLink wobj cWlSurface
  _ <- wlSurfaceAttach surface buffer 0 0
  wlSurfaceCommit surface
  pure ()

-- --------------------------------------------------------------------

myWlSeatListener :: WlSeatListener
myWlSeatListener = WlSeatListener
                      (Just myWlSeatCapabilities)
                      (Just myWlSeatName)

myWlSeatCapabilities  :: TwlSeatCapabilities
myWlSeatCapabilities wobj capa = do
    ST.liftIO $ putStrLn ("WlSeatCapabilities called capa: " <> show capa)
    -- Bitwise 1 = pointer
    -- Bitwise 2 = keyboard
    if capa .&. 1 == 1
      then do
        ST.liftIO $ putStrLn "Pointer is available"
        _pointer <- wlSeatGetPointer wobj "wl_pointer"
        pure ()
      else ST.liftIO $ putStrLn "Pointer is missing"
    if capa .&. 2 == 2
      then do
        ST.liftIO $ putStrLn "Terminal is available"
      else ST.liftIO $ putStrLn "Terminal is missing"
    if capa .&. 4 == 4
      then do
        ST.liftIO $ putStrLn "Touch is available"
      else ST.liftIO $ putStrLn "Touch is missing"
    pure ()


myWlSeatName :: TwlSeatName
myWlSeatName _ name = do
   ST.liftIO $ putStrLn ("WlSeatName name: " <> show name)
   pure ()

-- --------------------------------------------------------------------
myWlPointerListener :: WlPointerListener
myWlPointerListener = WlPointerListener
    (Just myWlPointerEnter)
    (Just myWlPointerLeave)
    (Just myWlPointerMotion)
    (Just myWlPointerButton)
    (Just myWlPointerAxis)
    (Just myWlPointerFrame)
    (Just myWlPointerAxisSource)
    (Just myWlPointerAxisStop)
    (Just myWlPointerAxisDiscrete)

myWlPointerEnter :: TwlPointerEnter
myWlPointerEnter _wobj _serial _surface _surfaceX _surfaceY = do
    ST.liftIO $ putStrLn "wlPointerEnter"
    pure ()

myWlPointerLeave :: TwlPointerLeave
myWlPointerLeave _wobj _serial _surface = do
    ST.liftIO $ putStrLn "wlPointerLeave"
    pure ()

myWlPointerMotion :: TwlPointerMotion
myWlPointerMotion _wobj _time _surfaceX _surfaceY = do
    pure ()

myWlPointerButton :: TwlPointerButton
myWlPointerButton _wobj _serial _time _button _state = do
    ST.liftIO $ putStrLn "wlPointerButton"
    pure ()

myWlPointerAxis :: TwlPointerAxis
myWlPointerAxis _wobj _time _axis _value = do
    pure ()

myWlPointerFrame :: TwlPointerFrame
myWlPointerFrame _wobj = do
    pure ()

myWlPointerAxisSource :: TwlPointerAxisSource
myWlPointerAxisSource _wobj _axisSource = do
    pure ()

myWlPointerAxisStop :: TwlPointerAxisStop
myWlPointerAxisStop _wobj _time _axis = do
    pure ()

myWlPointerAxisDiscrete :: TwlPointerAxisDiscrete
myWlPointerAxisDiscrete _wobj _axis _discrete = do
    pure ()

-- --------------------------------------------------------------------
-- Create and fill a buffer
-- --------------------------------------------------------------------
drawFrame :: WObj -> ClMonad WObj
drawFrame shm = do
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
  shmPool <- wlShmCreatePool shm cWlShmPool (fromIntegral fd) (fromIntegral size)
  buffer <- wlShmPoolCreateBuffer shmPool cWlBuffer 0
            (fromIntegral width) (fromIntegral height) (fromIntegral stride) wL_SHM_FORMAT_XRGB8888
  wlShmPoolDestroy shmPool
  -- The fd will be closed, after it has been sent to the server
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
