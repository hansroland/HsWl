module WaylandADTs (

    PtrDummy,
    PtrInterface,
    PtrDisplay,
    PtrRegistry,
    PtrCompositor,
    PtrShm,
    PtrShmPool,
    PtrXdgWmBase,
    PtrSurface,
    PtrBuffer,
    PtrXdgSurface,
    PtrXdgToplevel
) where
import Foreign.Ptr (Ptr)

-- -------------------------------------------------------------------
-- General Wayland data stuctures, opaque for Haskell
-- -------------------------------------------------------------------
data Dummy
data Interface
data Display
data Registry
data Compositor
data Shm
data ShmPool
data XdgWmBase
data Surface
data Buffer
data XdgSurface
data XdgToplevel

type PtrDummy = Ptr Dummy
type PtrInterface = Ptr Interface
type PtrDisplay = Ptr Display
type PtrRegistry = Ptr Registry
type PtrCompositor = Ptr Compositor
type PtrShm = Ptr Shm
type PtrShmPool = Ptr ShmPool
type PtrXdgWmBase = Ptr XdgWmBase
type PtrSurface = Ptr Surface
type PtrBuffer = Ptr Buffer
type PtrXdgSurface = Ptr XdgSurface
type PtrXdgToplevel = Ptr XdgToplevel

