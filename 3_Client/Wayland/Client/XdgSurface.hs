module Wayland.Client.XdgSurface where

import Wayland.Wire.Protocol
import Wayland.Client.Types
import Wayland.Client.OsShm
import Wayland.Client.Support
import Wayland.Client.FillFrame

import Control.Monad.State.Strict
import Foreign (nullPtr, newForeignPtr_)


xdgSurfaceListener :: XdgSurfaceListener
xdgSurfaceListener = XdgSurfaceListener (Just surfaceConfigure)

surfaceConfigure :: TxdgSurfaceConfigure
surfaceConfigure wobj serial = do
  liftIO $ putStrLn ("XDGConfigureHandler called wobj:" <> show wobj)
  xdgSurfaceAckConfigure wobj serial
  shm <- getWObjForGlobal cWlShm
  buffer <- createBuffer shm
  surface <- getWObjLink wobj cWlSurface
  _ <- wlSurfaceAttach surface buffer 0 0
  wlSurfaceCommit surface
  pure ()

-- | Create and fill a buffer
createBuffer :: WObj -> ClMonad WObj
createBuffer shm = do
  liftIO $ putStrLn "drawFrame active"
  let width  = 240                             -- 640 :: Int
      height = 80                              -- 480 :: Int
      stride = width * (4 :: Int)
      size   = stride * height

  fd <- liftIO $ allocateShmFile size
  liftIO $ putStrLn $ "drawFrame fd: " <> show fd
  memAddr <- liftIO $ mmap nullPtr (fromIntegral size)
            (cPROT_READ + cPROT_WRITE) cMAP_SHARED (fromIntegral fd) 0
  fmemAddr <- liftIO $ newForeignPtr_ memAddr

  liftIO $ fillFrame fmemAddr width height

  shmPool <- wlShmCreatePool shm cWlShmPool (fromIntegral fd) (fromIntegral size)
  buffer <- wlShmPoolCreateBuffer shmPool cWlBuffer 0
            (fromIntegral width) (fromIntegral height) (fromIntegral stride) wL_SHM_FORMAT_XRGB8888
  wlShmPoolDestroy shmPool
  -- The fd will be closed, after it has been sent to the server
  liftIO $ munmap memAddr (fromIntegral size)
  pure buffer


