{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Client (
    RegistryListenerData(..),
    FunRegistryListenerGlobal,
    FunRegistryListenerGlobalRemove,
    displayConnect,
    isValidPointer,
    displayDisconnect,
    displayRoundtrip,
    displayGetRegistry,
    displayDispatch,
    registryAddListener,
    registryBind,
    wrapRegistryHandler,
    wrapRegistryRemoveHandler,
    -- Compositor
    compositorCreateSurface,
    surfaceCommit,
    surfaceAttach,
    -- Shm
    allocateShmFile,
    shmCreatePool,
    shmPoolCreateBuffer,
    shmPoolDestroy,
    wL_SHM_FORMAT_XRGB8888,
    -- Buffer Management
    BufferListenerData(..),
    FunBufferListenerRelease,
    bufferAddListener,
    wrapBufferHandler,
    bufferDestroy,
    mmap,
    munmap,
    -- XdgWm
    XdgWmBaseListenerData(..),
    FunXdgWmBaseListenerPing,
    xdgWmBaseAddListener,
    wrapFunXdgWmBaseListenerPing,
    xdgWmBasePong,
    XdgSurfaceListenerConfigure,
    XdgSurfaceListenerData(..),
    xdgSurfaceAddListener,
    wrapFunXdgSurfaceListenerConfigure,
    xdgWmBaseGetXdgSurface,
    xdgSurfaceGetToplevel,
    xdgToplevelSetTitle,
    xdgSurfaceAckConfigure,
    -- Interfaces
    getCompositorInterface,
    getShmInterface,
    getXdgWmBaseInterface,
    -- Posix Memory Mapping
    cPROT_EXEC,
    cPROT_NONE,
    cPROT_READ,
    cPROT_WRITE,
    cMAP_FIXED,
    cMAP_PRIVATE,
    cMAP_SHARED,
    cMAP_FAILED,
    -- Utilities
    myPutStrLn
) where

-- import ClientState
import WaylandADTs

import Test.RandomStrings

import Foreign.C.String
import Foreign.Ptr (Ptr, FunPtr, nullPtr)
import Foreign.Storable
import Foreign.Marshal.Alloc

import System.Posix.SharedMem
import System.Posix.Types
import System.Posix.Files

import System.IO ( hFlush, stdout )     -- DEBUG DEBUG myPutStrLn

#include <wayland-client.h>
#include "xdg-shell-client-protocol.h"

-- -------------------------------------------------------------------
-- Display connect and disconnect
-- -------------------------------------------------------------------

-- struct wl_display *wl_display_connect(const char *name);
-- | Connect to a display with a specified name
-- stuct wl_display *
--    wl_display_connect(const char *name);
-- We send a string (or a null) and recive a pointer to a wl_display struct

-- | interface to the wayland library wl_display_connect
displayConnect :: String -> IO (PtrDisplay)
displayConnect _ = do
        -- TODO: use similar function as withCString !!
        display <- wlDisplayConnect nullPtr
        return display

displayDisconnect :: PtrDisplay -> IO ()
displayDisconnect pdisp = do
    wlDisplayDisconnect pdisp

foreign import capi unsafe "wayland-client.h wl_display_connect"
    wlDisplayConnect :: CString -> IO (PtrDisplay)

foreign import capi unsafe "wayland-client.h wl_display_disconnect"
   wlDisplayDisconnect :: PtrDisplay -> IO ()

isValidPointer :: Ptr a -> Bool
isValidPointer ptr = ptr /= nullPtr

-- --------------------------------------------------------------------
-- Display Roundtrip
-- --------------------------------------------------------------------
foreign import capi "wayland-client.h wl_display_roundtrip"
   displayRoundtrip :: PtrDisplay -> IO ()


-- int wl_display_dispatch(struct wl_display *display);
foreign import capi "wayland-client.h wl_display_dispatch"
    displayDispatch :: PtrDisplay -> IO Int

-----------------------------------------------------------------------
-- Registry Handling
-----------------------------------------------------------------------

type FunRegistryListenerGlobal = Ptr () -> Ptr () -> C2HSImp.CUInt -> CString -> C2HSImp.CUInt -> IO ()
type FunRegistryListenerGlobalRemove = Ptr () -> Ptr () -> C2HSImp.CUInt -> IO ()

data RegistryListenerData = RegistryListenerData {
    pglobal       :: !(FunPtr FunRegistryListenerGlobal),
    pglobalRemove :: !(FunPtr FunRegistryListenerGlobalRemove)
    }

instance Storable RegistryListenerData where
  sizeOf _ = {#sizeof wl_registry_listener #}
  alignment _ = 4
  peek p = do
    fp1  <- {#get struct wl_registry_listener.global#} p
    fp2  <- {#get struct wl_registry_listener.global_remove#} p
    pure $ RegistryListenerData { pglobal = fp1, pglobalRemove = fp2 }
  poke p x = do
    {#set wl_registry_listener.global #} p (pglobal x)
    {#set wl_registry_listener.global_remove #} p (pglobalRemove x)

foreign import capi "wayland-client.h wl_display_get_registry"
    displayGetRegistry :: PtrDisplay -> IO (PtrRegistry)

-- static inline int wl_registry_add_listener(struct wl_registry *wl_registry,
--			 const struct wl_registry_listener *listener, void *data)
foreign import capi "wayland-client.h wl_registry_add_listener"
    registryAddListener' :: PtrRegistry -> Ptr RegistryListenerData -> PtrDummy -> IO ()

registryAddListener :: PtrRegistry -> RegistryListenerData -> IO ()
registryAddListener ptrRegistry registryListenerData =
  alloca $ \ p -> poke p registryListenerData >> registryAddListener' ptrRegistry p nullPtr

-- wl_registry_bind(struct wl_registry *wl_registry, uint32_t name,
-- const struct wl_interface *interface, uint32_t version)
foreign import capi "wayland-client.h wl_registry_bind"
   registryBind :: PtrRegistry -> C2HSImp.CUInt -> PtrInterface -> C2HSImp.CUInt -> IO (Ptr a)

-- a "wrapper" import gives a factory for converting a Haskell function to a foreign function pointer.
-- Wrapper stubs can't be used with CApiFFI.
foreign import ccall "wrapper"
  wrapRegistryHandler :: FunRegistryListenerGlobal -> IO (FunPtr (FunRegistryListenerGlobal))

foreign import ccall "wrapper"
  wrapRegistryRemoveHandler :: FunRegistryListenerGlobalRemove -> IO (FunPtr FunRegistryListenerGlobalRemove)

-- --------------------------------------------------------------------
-- Compositor functions
-- --------------------------------------------------------------------
foreign import capi "wayland-client.h wl_compositor_create_surface"
  compositorCreateSurface :: PtrCompositor ->  IO PtrSurface


-- --------------------------------------------------------------------
-- Wl Surface
-- --------------------------------------------------------------------
foreign import capi "wayland-client-protocol.h wl_surface_commit"
  surfaceCommit :: PtrSurface ->  IO ()

foreign import capi "wayland-client-protocol.h wl_surface_attach"
  surfaceAttach :: PtrSurface -> PtrBuffer -> C2HSImp.CInt -> C2HSImp.CInt -> IO ()

-- --------------------------------------------------------------------
-- Buffer management
-- --------------------------------------------------------------------
type FunBufferListenerRelease = Ptr () -> Ptr () -> IO ()

data BufferListenerData = BufferListenerData {
    release :: !(FunPtr FunBufferListenerRelease)
    }

instance Storable BufferListenerData where
  sizeOf _ = {#sizeof wl_buffer_listener #}
  alignment _ = 4
  peek p = do
    fp  <- {#get struct wl_buffer_listener.release#} p
    pure $ BufferListenerData { release = fp }
  poke p x = do
    {#set wl_buffer_listener.release#} p (release x)

-- a "wrapper" import gives a factory for converting a Haskell function to a foreign function pointer.
-- Wrapper stubs can't be used with CApiFFI.
foreign import ccall "wrapper"
  wrapBufferHandler :: FunBufferListenerRelease -> IO (FunPtr FunBufferListenerRelease)

foreign import capi "wayland-client-protocol.h wl_buffer_add_listener"
     bufferAddListener' :: PtrBuffer -> Ptr BufferListenerData -> Ptr () -> IO ()

bufferAddListener :: PtrBuffer -> BufferListenerData -> IO ()
bufferAddListener ptrBuffer bufferListenerData =
  alloca $ \ p -> poke p bufferListenerData >> bufferAddListener' ptrBuffer p nullPtr

foreign import capi "wayland-client.h wl_buffer_destroy"
    bufferDestroy :: PtrBuffer ->  IO ()

-- --------------------------------------------------------------------
-- Posix memory mapping
-- --------------------------------------------------------------------
foreign import ccall "mmap"                       -- Using ccall results in a warning
   mmap :: Ptr () -> C2HSImp.CSize -> C2HSImp.CInt -> C2HSImp.CInt-> Fd-> C2HSImp.CInt -> IO (Ptr ())

foreign import ccall "munmap"                     -- Using ccall results in a warning
   munmap :: Ptr () -> C2HSImp.CSize -> IO ()

cPROT_EXEC = 4
cPROT_EXEC :: (Num a) => a

cPROT_NONE = 0
cPROT_NONE :: (Num a) => a

cPROT_READ = 1
cPROT_READ :: (Num a) => a

cPROT_WRITE = 2
cPROT_WRITE :: (Num a) => a

cMAP_FIXED = 16
cMAP_FIXED :: (Num a) => a

cMAP_PRIVATE = 2
cMAP_PRIVATE :: (Num a) => a

cMAP_SHARED = 1
cMAP_SHARED :: (Num a) => a

cMAP_FAILED = C2HSImp.wordPtrToPtr 4294967295
cMAP_FAILED :: Ptr a

wL_SHM_FORMAT_XRGB8888 = 1
wL_SHM_FORMAT_XRGB8888 :: (Num a) => a

-- See https://stackoverflow.com/questions/30446690/how-do-i-read-a-shared-memory-using-haskells-mmap-library
-- See Bindings.Posix.Sys.Mman in bindings-posix
-- Note: Last CInt was Int64 !!
-- See: https://man7.org/linux/man-pages/man2/mmap.2.html

-- --------------------------------------------------------------------
-- Get interfaces from small own wayland2hs module.
-- --------------------------------------------------------------------
--  Note: This avoids defining the interface structs and the Storable
--        instances.
-- Here we use the ccall interface because of GHC 9.4.4 doesn't have
-- Foreign.C.ConstPtr
-- TODO Change to ConstPtr after upgrading to GHC 9.6
foreign import ccall "wayland2hs.h getCompositorInterface"
  getCompositorInterface :: IO PtrInterface

foreign import ccall "wayland2hs.h getShmInterface"
  getShmInterface :: IO PtrInterface

foreign import ccall "wayland2hs.h getXdgWmBaseInterface"
  getXdgWmBaseInterface :: IO PtrInterface

-- --------------------------------------------------------------------
-- Functions for Shared Memory handling
-- --------------------------------------------------------------------

-- Create a random filename for the Shared memory
randomFileName :: IO String
randomFileName = (<>) <$> pure "/wl_shm-" <*> (randomString (onlyLower randomASCII) 6)

-- Open Shared memory
createShmFile :: IO Fd
createShmFile = do
  fn <- randomFileName
  let flags = ShmOpenFlags { shmReadWrite = True
                           , shmCreate = True
                           , shmExclusive = True
                           , shmTrunc = False }
  -- TODO: check for success and unlink in case of failure !!
  shmOpen fn flags (CMode 0600)

allocateShmFile :: Int -> IO Fd
allocateShmFile sz = do
  fd <- createShmFile
  setFdSize fd (COff (fromIntegral sz))
  return fd

-- static inline struct wl_shm_pool *
-- wl_shm_create_pool(struct wl_shm *wl_shm, int32_t fd, int32_t size)
foreign import capi "wayland-client.h wl_shm_create_pool"
    shmCreatePool' :: PtrShm -> C2HSImp.CInt -> C2HSImp.CInt -> IO PtrShmPool

shmCreatePool :: PtrShm -> Fd -> Int -> IO PtrShmPool
shmCreatePool shm fd size =
   shmCreatePool' shm (fromIntegral fd) (fromIntegral size)

foreign import capi "wayland-client.h wl_shm_pool_destroy"
   shmPoolDestroy :: PtrShmPool -> IO ()

-- static inline struct wl_buffer *
-- wl_shm_pool_create_buffer(struct wl_shm_pool *wl_shm_pool, int32_t offset,
--             int32_t width, int32_t height, int32_t stride, uint32_t format)
foreign import capi "wayland-client.h wl_shm_pool_create_buffer"
   shmPoolCreateBuffer :: PtrShmPool -> C2HSImp.CInt -> C2HSImp.CInt ->
     C2HSImp.CInt -> C2HSImp.CInt -> C2HSImp.CInt -> IO PtrBuffer

-- --------------------------------------------------------------------
-- xdg interface
-- --------------------------------------------------------------------
type FunXdgWmBaseListenerPing = Ptr () -> Ptr () -> C2HSImp.CUInt -> IO ()

data XdgWmBaseListenerData = XdgWmBaseListenerData {
    ping :: !(FunPtr FunXdgWmBaseListenerPing)
    }

instance Storable XdgWmBaseListenerData where
  sizeOf _ = {#sizeof xdg_wm_base_listener #}
  alignment _ = 4
  peek p = do
    fp  <- {#get struct xdg_wm_base_listener.ping#} p
    pure $ XdgWmBaseListenerData { ping = fp }
  poke p x = do
    {#set xdg_wm_base_listener.ping#} p (ping x)

foreign import ccall "wrapper"                       -- wrapper needs ccall
  wrapFunXdgWmBaseListenerPing :: FunXdgWmBaseListenerPing -> IO (FunPtr FunXdgWmBaseListenerPing)

-- xdg_wm_base_pong(struct xdg_wm_base *xdg_wm_base, uint32_t serial)
-- Here we need capi, because the function is defined in the header file as
-- static inline void xdg_wm_base_pong(struct x ...
foreign import capi "xdg-shell-client-protocol.h xdg_wm_base_pong"
  xdgWmBasePong :: Ptr xdgWmBase -> C2HSImp.CUInt -> IO ()

-- static inline int xdg_wm_base_add_listener(struct xdg_wm_base *xdg_wm_base,
--			 const struct xdg_wm_base_listener
foreign import capi "xdg-shell-client-protocol.h xdg_wm_base_add_listener"
  xdgWmBaseAddListener' :: PtrXdgWmBase ->  Ptr XdgWmBaseListenerData -> PtrDummy -> IO ()


xdgWmBaseAddListener :: PtrXdgWmBase -> XdgWmBaseListenerData -> IO ()
xdgWmBaseAddListener ptrXdgWmBase  xdgWmBaseListener  =
  alloca $ \ p -> poke p xdgWmBaseListener >> xdgWmBaseAddListener' ptrXdgWmBase p nullPtr

-- static inline struct xdg_surface *
-- xdg_wm_base_get_xdg_surface(struct xdg_wm_base *xdg_wm_base, struct wl_surface *surface)
foreign import capi "xdg-shell-client-protocol.h xdg_wm_base_get_xdg_surface"
  xdgWmBaseGetXdgSurface :: PtrXdgWmBase -> PtrSurface -> IO PtrXdgSurface

-- static void
-- xdg_surface_configure(void *data,
--         struct xdg_surface *xdg_surface, uint32_t serial)
-- TODO Zweiter Ptr () sollte ein PtrXdgSurface sein!
type XdgSurfaceListenerConfigure = Ptr () -> Ptr () -> C2HSImp.CUInt -> IO ()

-- static const struct xdg_surface_listener xdg_surface_listener = {
--    .configure = xdg_surface_configure,
-- };
data XdgSurfaceListenerData = XdgSurfaceListenerData {
  configure :: FunPtr XdgSurfaceListenerConfigure
  }

instance Storable XdgSurfaceListenerData where
  sizeOf _ = {#sizeof xdg_surface_listener #}
  alignment _ = 4
  peek p = do
    fp  <- {#get struct xdg_surface_listener.configure#} p
    pure $ XdgSurfaceListenerData { configure = fp }
  poke p x = do
    {#set xdg_surface_listener.configure#} p (configure x)

foreign import ccall "wrapper"                    -- wrapper needs ccall
  wrapFunXdgSurfaceListenerConfigure :: XdgSurfaceListenerConfigure -> IO (FunPtr XdgSurfaceListenerConfigure)

-- static inline int
-- xdg_surface_add_listener(struct xdg_surface *xdg_surface,
-- 			 const struct xdg_surface_listener *listener, void *data)     from xdg-shell-client-protocol.h xdg_surface_add_listener
foreign import capi "xdg-shell-client-protocol.h xdg_surface_add_listener"
  xdgSurfaceAddListener' :: PtrXdgSurface ->  Ptr XdgSurfaceListenerData -> PtrDummy -> IO ()

xdgSurfaceAddListener :: PtrXdgSurface -> XdgSurfaceListenerData -> IO ()
xdgSurfaceAddListener ptrXdgSurface  xdgSurfaceListener  =
  alloca $ \ p -> poke p xdgSurfaceListener >> xdgSurfaceAddListener' ptrXdgSurface p nullPtr

foreign import capi "xdg-shell-client-protocol.h xdg_surface_get_toplevel"
  xdgSurfaceGetToplevel :: PtrXdgSurface -> IO PtrXdgToplevel

foreign import capi "xdg-shell-client-protocol.h xdg_toplevel_set_title"
  xdgToplevelSetTitle' :: PtrXdgToplevel -> CString -> IO ()

foreign import capi "xdg-shell-client-protocol.h xdg_surface_ack_configure"
  xdgSurfaceAckConfigure :: PtrXdgSurface -> C2HSImp.CUInt -> IO ()

xdgToplevelSetTitle :: PtrXdgToplevel -> String -> IO ()
xdgToplevelSetTitle toplevel title = do
   ctitle <- newCString title
   xdgToplevelSetTitle' toplevel ctitle
   -- TODO use withCString or free storage

myPutStrLn :: String -> IO ()
myPutStrLn str = putStrLn str >> hFlush stdout
