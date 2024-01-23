{-# LANGUAGE OverloadedStrings #-}

module Client where

import ClientSupport
import Wire
import WireSupport

import Types
import WaylandSocket
import Shm

import qualified Network.Socket             as Socket
import qualified Data.ByteString            as BS
import           Control.Monad.State.Strict
import qualified Data.Text as T

-- * Interface wl_display - core global object

-- | sync - opc 0: asynchronous roundtrip
displaySync :: ClMonad ()
displaySync = do
    wobj <- getObjectId cWlDisplay
    newId <- createNewId cWlCallback
    addRequest $ wlDisplaySync wobj newId

-- | get_registry opc 1: get global registry object
displayGetRegistry :: ClMonad ()
displayGetRegistry = do
    wobj <- getObjectId cWlDisplay
    newId <- createNewId cWlRegistry
    addRequest $  wlDisplayGetRegistry wobj newId

-- * Interface wl_registry - global registry object

-- | bind opc 0: bind an object to the display
bindToInterface :: WInt -> WString-> WUint -> ClMonad ()
bindToInterface name interface version = do
    wobj <- getObjectId cWlRegistry
    newId <- createNewId $ unWString interface
    addRequest $ rsxRegistryBind  wobj (fromIntegral name) interface version (fromIntegral newId)

-- i Interface wl_callback - callback object
-- This interface has no requests !


-- * Interface wl_compositor - the compositor singleton

-- | create_surface opc 0: - Ask the compositor to create a new surface.
compositorCreateSurface :: ClMonad WObj
compositorCreateSurface = do
    wobj <- getObjectId cWlCompositor
    newId <- createNewId cWlSurface
    addRequest $ wlCompositorCreateSurface wobj newId
    pure $ fromIntegral newId                       -- TODO fromIntegral

-- * Interface wl_shm_pool - a shared memory pool

-- | pool create buffer opc 0:  create a buffer from the pool
shmPoolCreateBuffer :: WInt -> WInt -> WInt -> WInt -> WUint -> ClMonad ()
shmPoolCreateBuffer offset width height stride format = do
    wobj <- getObjectId cWlShmPool
    newId <- createNewId cWlBuffer
    -- TODO finish this function
    pure ()


-- | pool destroy opc 1:
shmPoolDestroy :: ClMonad ()
shmPoolDestroy = do
    wobj <- getObjectId cWlShmPool
    addRequest $ wlShmPoolDestroy wobj

-- * Interface wl_shm

-- | create_pool opc 0: create a shm pool
shmCreatePool :: WFd -> WInt -> ClMonad WNewId
shmCreatePool fd size = do
    wobj <- getObjectId cWlShm
    newId <- createNewId cWlShmPool
    -- https://gitlab.freedesktop.org/wayland/wayland/-/blob/main/src/wayland-shm.c?ref_type=heads
    -- https://en.m.wikipedia.org/wiki/Unix_domain_socket
    addRequest $ wlShmCreatePool wobj newId fd size


    pure newId




-- i Interface wl_buffer - content for a wl_surface

-- * Interface wl_surface

-- - destroy opc 0: delete surface

-- - attach opc 1: set the surface contents

-- - damage opc 2: mark part of the surface damaged

-- - frame opc 3: request a frame throttling hint

-- - set_opaque_region opc 4: set opaque region

-- - set_input_region opc 5: set input region

-- | commit opc 6: commit pending surface state
surfaceCommit :: ClMonad ()
surfaceCommit = do
    wobj <- getObjectId cWlSurface
    addRequest $ wlSurfaceCommit wobj

-- i Interface wl_seat - group of input devices

-- i Interface wl_pointer - pointer input device

-- i Interface wl_keyboard - keyboard input device

-- i Interface wl_putput - compositor output region

-- i Interface wl_region - A region object describes an area.

-- * Interface xdg_wm_base


-- - destroy opc 0 - destroy xdg_wm_base

-- - create_positioner opc 1 - create a positioner object

-- | get_xdg_surface  opc 2 - create a shell surface from a surface
xdgGetXdgSurface :: WObj -> ClMonad WObj
xdgGetXdgSurface surface = do
    wobj <- getObjectId cXdgWmBase
    newId <- createNewId cXdgSurface
    addRequest $ xdgWmBaseGetXdgSurface wobj newId surface
    pure $ fromIntegral newId                       -- TODO fromIntegral

-- | pong opc 3 - respond to a ping event
xdgSendPongAnswer :: WUint -> ClMonad ()
xdgSendPongAnswer serial = do
    wobj <- getObjectId cXdgWmBase
    addRequest $ xdgWmBasePong wobj serial

-- i xdg_positioner - child surface positioner

-- * Interface xdg_surface - desktop user interface surface base interface

-- | destroy opc 0: destroy the xdg_surface
surfaceAckConfigure :: WUint -> ClMonad ()
surfaceAckConfigure serial = do
    wobj <- getObjectId cXdgSurface
    addRequest $ xdgSurfaceAckConfigure wobj serial

-- | get_toplevel opc 1: assign the xdg_toplevel surface role
surfaceAssignToplevel ::  ClMonad WObj
surfaceAssignToplevel = do
    wobj <- getObjectId cXdgSurface
    newId <- createNewId cXdgToplevel
    addRequest $ xdgSurfaceGetToplevel wobj newId
    pure $ fromIntegral newId                       -- TODO fromIntegral

-- - get_popup opc 2: assign the xdg_popup surface role

-- - set_window_geometry opc 3: set the new window geometry

-- - ack_configure - ack a configure event

-- * Interface xdg_toplevel - toplevel surface

-- - destroy opc 0: destroy the xdg_toplevel

-- - set_parent opc 1: set the parent of this surface

-- | set_title opc 2: set surface title
-- Set a short title for the surface
toplevelSetTitle :: WString -> ClMonad ()
toplevelSetTitle title = do
    wobj <- getObjectId cXdgToplevel
    addRequest $ xdgToplevelSetTitle wobj title

-- i Interface xdg_popup - short-lived, popup surfaces for menus

-- * Management (non request) functions

-- | removeActiveIfac remove an interface from the list of active interfaces
removeActiveIfac :: WObj -> ClMonad ()
removeActiveIfac obj  = do
    st <- get
    let newacts = filter oks (clActiveIfaces st)
    liftIO $ printActiveIfaces newacts
    put $ st {clActiveIfaces = newacts }
  where
    oks (o,_) = o /= obj

-- | printActiveIfaces - for debugging
printActiveIfaces :: [IfacKey] -> IO ()
printActiveIfaces keys = do
    putStrLn ("ACTIVE Interfaces: " <> concatMap showIfac keys)
  where
    showIfac :: IfacKey -> String
    showIfac (obj, txt ) = "(" <> show obj <> ", "  <> T.unpack txt  <> ")"

-- | sendRequests - send all requests from the request list
sendRequests :: Socket.Socket -> ClMonad ()
sendRequests serverSock = do
    reqs <- collectRequests
    _ <- liftIO $ sendToWayland serverSock reqs []
    st <- get
    put st {clReqs = []}
  where
    collectRequests :: ClMonad BS.ByteString
    collectRequests = do mconcat. reverse . clReqs <$> get

