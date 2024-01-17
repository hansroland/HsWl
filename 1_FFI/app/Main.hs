module Main where

-- See: https://wayland-book.com/wayland-display/creation.html
--      https://wayland-book.com/wayland-display/event-loop.html
--      https://gitlab.freedesktop.org/wayland/weston/-/tree/main/clients

import ClientState
import WaylandADTs
import Client
import Control.Monad ( when )
import Foreign.Ptr (nullPtr, castPtr)
import System.Posix.IO ( closeFd )
import Foreign.C.String ( peekCString )

-- Bind callbacks in the registry
registryGlobalHandler :: FunRegistryListenerGlobal
registryGlobalHandler _ _ name piface ver = do
    cstate0 <- getClientState
    let cstate1 = incrementCount cstate0
    iface <- peekCString piface
    cstate2 <-
      case iface of
        "wl_compositor" -> do
            compIfac <- getCompositorInterface
            ptrComp <- registryBind (clRegistry cstate1) name compIfac ver
            when (isValidPointer ptrComp ) $ myPutStrLn "COMPOSITOR"
            pure $ cstate1 {clCompositor = ptrComp}
        "wl_shm" -> do
            shmIfac <- getShmInterface
            ptrShm <- registryBind (clRegistry cstate1) name shmIfac ver
            when (isValidPointer ptrShm ) $ myPutStrLn "SHM"
            pure $ cstate1 {clShm = ptrShm}
        "xdg_wm_base" -> do
            xdgIfac <- getXdgWmBaseInterface
            ptrXdg <- registryBind (clRegistry cstate1) name xdgIfac ver
            when (isValidPointer xdgIfac ) $ myPutStrLn "XDG_WM_BASE"
            pinger <- wrapFunXdgWmBaseListenerPing xdgWmRegistryPingHandler
            let listener =  XdgWmBaseListenerData {ping = pinger}
            xdgWmBaseAddListener ptrXdg listener
            -- TODO: Release listener ???
            pure $ cstate1 {clXdgWmBase = ptrXdg}
        _ -> pure cstate1

    myPutStrLn $ "interface: " <> show iface
      <> ", version: " <> show ver
      <> ", name: " <> show name
    setClientState cstate2
    pure ()


registryGlobalRemoveHandler :: FunRegistryListenerGlobalRemove
registryGlobalRemoveHandler _ _ nm = do
    myPutStrLn $ "removed: " <> show nm


-- Callbacks for XdgWmBaseListenerPing
xdgWmRegistryPingHandler :: FunXdgWmBaseListenerPing
xdgWmRegistryPingHandler _ _ seqno = do
  myPutStrLn "XDG PING Called"
  cstate0 <- getClientState
  xdgWmBasePong (clXdgWmBase cstate0) seqno

-- Callback for XdgSurfaceListenerConfigure
xdgSurfaceConfigureHandler :: XdgSurfaceListenerConfigure
xdgSurfaceConfigureHandler _ _ serial = do
  myPutStrLn "XDGConfigureHandler called"
  cstate0 <- getClientState
  rslt <- xdgSurfaceAckConfigure (clXdgSurface cstate0) serial
  buffer <- drawFrame (clShm cstate0)
  _ <- surfaceAttach (clSurface cstate0) buffer 0 0
  surfaceCommit (clSurface cstate0)
  return rslt

-- Callbacks for buffer management
bufferReleaseHandler :: FunBufferListenerRelease
bufferReleaseHandler _ ptrBuff = do
  -- TODO Why do we need the case ???
  myPutStrLn "Callback bufferReleaseHandler active!!!!!"
  bufferDestroy $ castPtr ptrBuff

-- --------------------------------------------------------------------
-- Create and fill a buffer
-- --------------------------------------------------------------------
drawFrame :: PtrShm -> IO PtrBuffer
drawFrame shm = do
  myPutStrLn "drawFrame active"
  let width  = 640 :: Int
      height = 480 :: Int
      stride = width * (4 :: Int)
      size   = stride * height
  fd <- allocateShmFile size
  memAddr <- mmap nullPtr (fromIntegral size)
            (cPROT_READ + cPROT_WRITE) cMAP_SHARED (fromIntegral fd) 0
  myPutStrLn (" memAddr " ++ show memAddr ++ " " ++ show cMAP_FAILED)
  shmPool <- shmCreatePool shm (fromIntegral fd) (fromIntegral size)
  buffer <- shmPoolCreateBuffer shmPool 0
            (fromIntegral width) (fromIntegral height) (fromIntegral stride) wL_SHM_FORMAT_XRGB8888
  shmPoolDestroy shmPool
  closeFd fd
  munmap memAddr (fromIntegral size)
  -- wl_buffer_add_listener(buffer, &wl_buffer_listener, NULL);
  bufHandler <- wrapBufferHandler bufferReleaseHandler
  let bufListener = BufferListenerData {release = bufHandler}
  bufferAddListener buffer bufListener
  --
  pure buffer

---
main :: IO ()
main = do
  ptrDisplay <- displayConnect ""

  if not (isValidPointer ptrDisplay)
    then myPutStrLn "Failed to connect to Wayland display."
    else do
      myPutStrLn "Connection established!"
      -- init registry
      ptrRegistry <- displayGetRegistry ptrDisplay
      cstate0 <- getClientState
      setClientState cstate0 {clDisplay = ptrDisplay, clRegistry = ptrRegistry }
      --
      pglob <- wrapRegistryHandler registryGlobalHandler                               -- TODO Release?
      pglobRemove <- wrapRegistryRemoveHandler registryGlobalRemoveHandler             -- TODO Release?
      let listener = RegistryListenerData{ pglobal = pglob, pglobalRemove = pglobRemove }
      _ <- registryAddListener ptrRegistry listener
      -- start event loop
      displayRoundtrip ptrDisplay
      -- when (isValidPointer )
      cstate2 <- getClientState
      surface <- compositorCreateSurface $ clCompositor cstate2
      -- let cstate3 = cstate2 {clSurface = surface}
      xdgSurface <- xdgWmBaseGetXdgSurface (clXdgWmBase cstate2) surface
      config <- wrapFunXdgSurfaceListenerConfigure xdgSurfaceConfigureHandler
      let xdgSurfaceListener = XdgSurfaceListenerData {configure = config}
      myPutStrLn "RSX Before xdgSurfaceAddListener"
      xdgSurfaceAddListener xdgSurface  xdgSurfaceListener
      topLevel <- xdgSurfaceGetToplevel xdgSurface
      xdgToplevelSetTitle topLevel "My first Wayland window"
      let cstate3 = cstate2 {clSurface = surface, clXdgSurface = xdgSurface,  clXdgToplevel = topLevel}
      setClientState cstate3
      myPutStrLn ("RegistryGlobalHandler was called " <> show (clCount cstate3) <> " times.")

      myPutStrLn "RSX Before surfaceCommit"
      surfaceCommit surface

      let loop = do
            myPutStrLn "displayDispatch"
            _ <- displayDispatch ptrDisplay
            loop
      _ <- loop

      displayDisconnect ptrDisplay
      myPutStrLn "disconnected!"
