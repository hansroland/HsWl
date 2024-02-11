{-# LANGUAGE OverloadedStrings #-}

-- *** ATTENTION *** Generated Code *** DO NOT MODIFY

module Protocol where
import Types
import ProtocolSupport

import Data.Binary
import Data.Binary.Put
import Data.Binary.Get

import qualified Control.Monad.State.Strict as ST
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Lazy       as BL
import qualified Control.Monad              as M
import Data.Maybe (fromMaybe, isJust, isNothing, fromJust)
import qualified Data.Text as T
import Data.Text (Text)
import Data.List (find)
import System.Posix.Types (Fd)

type ClMonad a = ST.StateT ClState IO a

-- Constants for Interface Names
cWlDisplay :: Text
cWlDisplay = "wl_display"
cWlRegistry :: Text
cWlRegistry = "wl_registry"
cWlCallback :: Text
cWlCallback = "wl_callback"
cWlCompositor :: Text
cWlCompositor = "wl_compositor"
cWlShmPool :: Text
cWlShmPool = "wl_shm_pool"
cWlShm :: Text
cWlShm = "wl_shm"
cWlBuffer :: Text
cWlBuffer = "wl_buffer"
cWlDataOffer :: Text
cWlDataOffer = "wl_data_offer"
cWlDataSource :: Text
cWlDataSource = "wl_data_source"
cWlDataDevice :: Text
cWlDataDevice = "wl_data_device"
cWlDataDeviceManager :: Text
cWlDataDeviceManager = "wl_data_device_manager"
cWlShell :: Text
cWlShell = "wl_shell"
cWlShellSurface :: Text
cWlShellSurface = "wl_shell_surface"
cWlSurface :: Text
cWlSurface = "wl_surface"
cWlSeat :: Text
cWlSeat = "wl_seat"
cWlPointer :: Text
cWlPointer = "wl_pointer"
cWlKeyboard :: Text
cWlKeyboard = "wl_keyboard"
cWlTouch :: Text
cWlTouch = "wl_touch"
cWlOutput :: Text
cWlOutput = "wl_output"
cWlRegion :: Text
cWlRegion = "wl_region"
cWlSubcompositor :: Text
cWlSubcompositor = "wl_subcompositor"
cWlSubsurface :: Text
cWlSubsurface = "wl_subsurface"
cXdgWmBase :: Text
cXdgWmBase = "xdg_wm_base"
cXdgPositioner :: Text
cXdgPositioner = "xdg_positioner"
cXdgSurface :: Text
cXdgSurface = "xdg_surface"
cXdgToplevel :: Text
cXdgToplevel = "xdg_toplevel"
cXdgPopup :: Text
cXdgPopup = "xdg_popup"

-- ----------------------------------------------------------------------
-- * Request Handling
-- ----------------------------------------------------------------------
-- ** Interface: WlDisplay - core global object

-- | Request opc: 0 - asynchronous roundtrip
wlDisplaySync :: WObj -> Text -> ClMonad WObj
wlDisplaySync wobj callback = do
    callback' <- createNewId callback
    addRequest $ runByteString $ do
        put wobj
        put $ WOpc 0
        putWord16host 12
        put callback'
    pure callback'

-- | Request opc: 1 - get global registry object
wlDisplayGetRegistry :: WObj -> Text -> ClMonad WObj
wlDisplayGetRegistry wobj registry = do
    registry' <- createNewId registry
    addRequest $ runByteString $ do
        put wobj
        put $ WOpc 1
        putWord16host 12
        put registry'
    pure registry'

-- ** Interface: WlRegistry - global registry object

-- | Request opc: 0 - bind an object to the display
wlRegistryBind :: WObj -> WUint -> Text -> ClMonad WObj
wlRegistryBind wobj name xid = do
    xid' <- createNewId xid
    addRequest $ runByteString $ do
        put wobj
        put $ WOpc 0
        putWord16host 16
        put name
        put xid'
    pure xid'

-- ** Interface: WlCompositor - the compositor singleton

-- | Request opc: 0 - create new surface
wlCompositorCreateSurface :: WObj -> Text -> ClMonad WObj
wlCompositorCreateSurface wobj xid = do
    xid' <- createNewId xid
    addRequest $ runByteString $ do
        put wobj
        put $ WOpc 0
        putWord16host 12
        put xid'
    pure xid'

-- | Request opc: 1 - create new region
wlCompositorCreateRegion :: WObj -> Text -> ClMonad WObj
wlCompositorCreateRegion wobj xid = do
    xid' <- createNewId xid
    addRequest $ runByteString $ do
        put wobj
        put $ WOpc 1
        putWord16host 12
        put xid'
    pure xid'

-- ** Interface: WlShmPool - a shared memory pool

-- | Request opc: 0 - create a buffer from the pool
wlShmPoolCreateBuffer :: WObj -> Text -> WInt -> WInt -> WInt -> WInt -> WUint -> ClMonad WObj
wlShmPoolCreateBuffer wobj xid offset width height stride format = do
    xid' <- createNewId xid
    addRequest $ runByteString $ do
        put wobj
        put $ WOpc 0
        putWord16host 32
        put xid'
        put offset
        put width
        put height
        put stride
        put format
    pure xid'

-- | Request opc: 1 - destroy the pool
wlShmPoolDestroy :: WObj -> ClMonad ()
wlShmPoolDestroy wobj = do
    addRequest $ runByteString $ do
        put wobj
        put $ WOpc 1
        putWord16host 8

-- | Request opc: 2 - change the size of the pool mapping
wlShmPoolResize :: WObj -> WInt -> ClMonad ()
wlShmPoolResize wobj size = do
    addRequest $ runByteString $ do
        put wobj
        put $ WOpc 2
        putWord16host 12
        put size

-- ** Interface: WlShm - shared memory support

-- | Request opc: 0 - create a shm pool
wlShmCreatePool :: WObj -> Text -> Fd -> WInt -> ClMonad WObj
wlShmCreatePool wobj xid fd size = do
    xid' <- createNewId xid
    addFd fd
    addRequest $ runByteString $ do
        put wobj
        put $ WOpc 0
        putWord16host 16
        put xid'
        put fd
        put size
    pure xid'

-- ** Interface: WlBuffer - content for a wl_surface

-- | Request opc: 0 - destroy a buffer
wlBufferDestroy :: WObj -> ClMonad ()
wlBufferDestroy wobj = do
    addRequest $ runByteString $ do
        put wobj
        put $ WOpc 0
        putWord16host 8

-- ** Interface: WlDataOffer - offer to transfer data

-- | Request opc: 0 - accept one of the offered mime types
wlDataOfferAccept :: WObj -> WUint -> WString -> ClMonad ()
wlDataOfferAccept wobj serial mimeType = do
    addRequest $ runByteString $ do
        put wobj
        put $ WOpc 0
        putWord16host $ fromIntegral len
        put serial
        put mimeType
  where len = 12 + sum (calcWStringLength <$> [mimeType])  + sum (calcWArrayLength  <$> []) 

-- | Request opc: 1 - request that the data is transferred
wlDataOfferReceive :: WObj -> WString -> Fd -> ClMonad ()
wlDataOfferReceive wobj mimeType fd = do
    addFd fd
    addRequest $ runByteString $ do
        put wobj
        put $ WOpc 1
        putWord16host $ fromIntegral len
        put mimeType
        put fd
  where len = 8 + sum (calcWStringLength <$> [mimeType])  + sum (calcWArrayLength  <$> []) 

-- | Request opc: 2 - destroy data offer
wlDataOfferDestroy :: WObj -> ClMonad ()
wlDataOfferDestroy wobj = do
    addRequest $ runByteString $ do
        put wobj
        put $ WOpc 2
        putWord16host 8

-- | Request opc: 3 - the offer will no longer be used
wlDataOfferFinish :: WObj -> ClMonad ()
wlDataOfferFinish wobj = do
    addRequest $ runByteString $ do
        put wobj
        put $ WOpc 3
        putWord16host 8

-- | Request opc: 4 - set the available/preferred drag-and-drop actions
wlDataOfferSetActions :: WObj -> WUint -> WUint -> ClMonad ()
wlDataOfferSetActions wobj dndActions preferredAction = do
    addRequest $ runByteString $ do
        put wobj
        put $ WOpc 4
        putWord16host 16
        put dndActions
        put preferredAction

-- ** Interface: WlDataSource - offer to transfer data

-- | Request opc: 0 - add an offered mime type
wlDataSourceOffer :: WObj -> WString -> ClMonad ()
wlDataSourceOffer wobj mimeType = do
    addRequest $ runByteString $ do
        put wobj
        put $ WOpc 0
        putWord16host $ fromIntegral len
        put mimeType
  where len = 8 + sum (calcWStringLength <$> [mimeType])  + sum (calcWArrayLength  <$> []) 

-- | Request opc: 1 - destroy the data source
wlDataSourceDestroy :: WObj -> ClMonad ()
wlDataSourceDestroy wobj = do
    addRequest $ runByteString $ do
        put wobj
        put $ WOpc 1
        putWord16host 8

-- | Request opc: 2 - set the available drag-and-drop actions
wlDataSourceSetActions :: WObj -> WUint -> ClMonad ()
wlDataSourceSetActions wobj dndActions = do
    addRequest $ runByteString $ do
        put wobj
        put $ WOpc 2
        putWord16host 12
        put dndActions

-- ** Interface: WlDataDevice - data transfer device

-- | Request opc: 0 - start drag-and-drop operation
wlDataDeviceStartDrag :: WObj -> WObj -> WObj -> WObj -> WUint -> ClMonad ()
wlDataDeviceStartDrag wobj source origin icon serial = do
    addRequest $ runByteString $ do
        put wobj
        put $ WOpc 0
        putWord16host 24
        put source
        put origin
        put icon
        put serial

-- | Request opc: 1 - copy data to the selection
wlDataDeviceSetSelection :: WObj -> WObj -> WUint -> ClMonad ()
wlDataDeviceSetSelection wobj source serial = do
    addRequest $ runByteString $ do
        put wobj
        put $ WOpc 1
        putWord16host 16
        put source
        put serial

-- | Request opc: 2 - destroy data device
wlDataDeviceRelease :: WObj -> ClMonad ()
wlDataDeviceRelease wobj = do
    addRequest $ runByteString $ do
        put wobj
        put $ WOpc 2
        putWord16host 8

-- ** Interface: WlDataDeviceManager - data transfer interface

-- | Request opc: 0 - create a new data source
wlDataDeviceManagerCreateDataSource :: WObj -> Text -> ClMonad WObj
wlDataDeviceManagerCreateDataSource wobj xid = do
    xid' <- createNewId xid
    addRequest $ runByteString $ do
        put wobj
        put $ WOpc 0
        putWord16host 12
        put xid'
    pure xid'

-- | Request opc: 1 - create a new data device
wlDataDeviceManagerGetDataDevice :: WObj -> Text -> WObj -> ClMonad WObj
wlDataDeviceManagerGetDataDevice wobj xid seat = do
    xid' <- createNewId xid
    addRequest $ runByteString $ do
        put wobj
        put $ WOpc 1
        putWord16host 16
        put xid'
        put seat
    pure xid'

-- ** Interface: WlShell - create desktop-style surfaces

-- | Request opc: 0 - create a shell surface from a surface
wlShellGetShellSurface :: WObj -> Text -> WObj -> ClMonad WObj
wlShellGetShellSurface wobj xid surface = do
    xid' <- createNewId xid
    addRequest $ runByteString $ do
        put wobj
        put $ WOpc 0
        putWord16host 16
        put xid'
        put surface
    pure xid'

-- ** Interface: WlShellSurface - desktop-style metadata interface

-- | Request opc: 0 - respond to a ping event
wlShellSurfacePong :: WObj -> WUint -> ClMonad ()
wlShellSurfacePong wobj serial = do
    addRequest $ runByteString $ do
        put wobj
        put $ WOpc 0
        putWord16host 12
        put serial

-- | Request opc: 1 - start an interactive move
wlShellSurfaceMove :: WObj -> WObj -> WUint -> ClMonad ()
wlShellSurfaceMove wobj seat serial = do
    addRequest $ runByteString $ do
        put wobj
        put $ WOpc 1
        putWord16host 16
        put seat
        put serial

-- | Request opc: 2 - start an interactive resize
wlShellSurfaceResize :: WObj -> WObj -> WUint -> WUint -> ClMonad ()
wlShellSurfaceResize wobj seat serial edges = do
    addRequest $ runByteString $ do
        put wobj
        put $ WOpc 2
        putWord16host 20
        put seat
        put serial
        put edges

-- | Request opc: 3 - make the surface a toplevel surface
wlShellSurfaceSetToplevel :: WObj -> ClMonad ()
wlShellSurfaceSetToplevel wobj = do
    addRequest $ runByteString $ do
        put wobj
        put $ WOpc 3
        putWord16host 8

-- | Request opc: 4 - make the surface a transient surface
wlShellSurfaceSetTransient :: WObj -> WObj -> WInt -> WInt -> WUint -> ClMonad ()
wlShellSurfaceSetTransient wobj parent x y flags = do
    addRequest $ runByteString $ do
        put wobj
        put $ WOpc 4
        putWord16host 24
        put parent
        put x
        put y
        put flags

-- | Request opc: 5 - make the surface a fullscreen surface
wlShellSurfaceSetFullscreen :: WObj -> WUint -> WUint -> WObj -> ClMonad ()
wlShellSurfaceSetFullscreen wobj method framerate output = do
    addRequest $ runByteString $ do
        put wobj
        put $ WOpc 5
        putWord16host 20
        put method
        put framerate
        put output

-- | Request opc: 6 - make the surface a popup surface
wlShellSurfaceSetPopup :: WObj -> WObj -> WUint -> WObj -> WInt -> WInt -> WUint -> ClMonad ()
wlShellSurfaceSetPopup wobj seat serial parent x y flags = do
    addRequest $ runByteString $ do
        put wobj
        put $ WOpc 6
        putWord16host 32
        put seat
        put serial
        put parent
        put x
        put y
        put flags

-- | Request opc: 7 - make the surface a maximized surface
wlShellSurfaceSetMaximized :: WObj -> WObj -> ClMonad ()
wlShellSurfaceSetMaximized wobj output = do
    addRequest $ runByteString $ do
        put wobj
        put $ WOpc 7
        putWord16host 12
        put output

-- | Request opc: 8 - set surface title
wlShellSurfaceSetTitle :: WObj -> WString -> ClMonad ()
wlShellSurfaceSetTitle wobj title = do
    addRequest $ runByteString $ do
        put wobj
        put $ WOpc 8
        putWord16host $ fromIntegral len
        put title
  where len = 8 + sum (calcWStringLength <$> [title])  + sum (calcWArrayLength  <$> []) 

-- | Request opc: 9 - set surface class
wlShellSurfaceSetClass :: WObj -> WString -> ClMonad ()
wlShellSurfaceSetClass wobj xclass = do
    addRequest $ runByteString $ do
        put wobj
        put $ WOpc 9
        putWord16host $ fromIntegral len
        put xclass
  where len = 8 + sum (calcWStringLength <$> [xclass])  + sum (calcWArrayLength  <$> []) 

-- ** Interface: WlSurface - an onscreen surface

-- | Request opc: 0 - delete surface
wlSurfaceDestroy :: WObj -> ClMonad ()
wlSurfaceDestroy wobj = do
    addRequest $ runByteString $ do
        put wobj
        put $ WOpc 0
        putWord16host 8

-- | Request opc: 1 - set the surface contents
wlSurfaceAttach :: WObj -> WObj -> WInt -> WInt -> ClMonad ()
wlSurfaceAttach wobj buffer x y = do
    addRequest $ runByteString $ do
        put wobj
        put $ WOpc 1
        putWord16host 20
        put buffer
        put x
        put y

-- | Request opc: 2 - mark part of the surface damaged
wlSurfaceDamage :: WObj -> WInt -> WInt -> WInt -> WInt -> ClMonad ()
wlSurfaceDamage wobj x y width height = do
    addRequest $ runByteString $ do
        put wobj
        put $ WOpc 2
        putWord16host 24
        put x
        put y
        put width
        put height

-- | Request opc: 3 - request a frame throttling hint
wlSurfaceFrame :: WObj -> Text -> ClMonad WObj
wlSurfaceFrame wobj callback = do
    callback' <- createNewId callback
    addRequest $ runByteString $ do
        put wobj
        put $ WOpc 3
        putWord16host 12
        put callback'
    pure callback'

-- | Request opc: 4 - set opaque region
wlSurfaceSetOpaqueRegion :: WObj -> WObj -> ClMonad ()
wlSurfaceSetOpaqueRegion wobj region = do
    addRequest $ runByteString $ do
        put wobj
        put $ WOpc 4
        putWord16host 12
        put region

-- | Request opc: 5 - set input region
wlSurfaceSetInputRegion :: WObj -> WObj -> ClMonad ()
wlSurfaceSetInputRegion wobj region = do
    addRequest $ runByteString $ do
        put wobj
        put $ WOpc 5
        putWord16host 12
        put region

-- | Request opc: 6 - commit pending surface state
wlSurfaceCommit :: WObj -> ClMonad ()
wlSurfaceCommit wobj = do
    addRequest $ runByteString $ do
        put wobj
        put $ WOpc 6
        putWord16host 8

-- | Request opc: 7 - sets the buffer transformation
wlSurfaceSetBufferTransform :: WObj -> WInt -> ClMonad ()
wlSurfaceSetBufferTransform wobj transform = do
    addRequest $ runByteString $ do
        put wobj
        put $ WOpc 7
        putWord16host 12
        put transform

-- | Request opc: 8 - sets the buffer scaling factor
wlSurfaceSetBufferScale :: WObj -> WInt -> ClMonad ()
wlSurfaceSetBufferScale wobj scale = do
    addRequest $ runByteString $ do
        put wobj
        put $ WOpc 8
        putWord16host 12
        put scale

-- | Request opc: 9 - mark part of the surface damaged using buffer coordinates
wlSurfaceDamageBuffer :: WObj -> WInt -> WInt -> WInt -> WInt -> ClMonad ()
wlSurfaceDamageBuffer wobj x y width height = do
    addRequest $ runByteString $ do
        put wobj
        put $ WOpc 9
        putWord16host 24
        put x
        put y
        put width
        put height

-- ** Interface: WlSeat - group of input devices

-- | Request opc: 0 - return pointer object
wlSeatGetPointer :: WObj -> Text -> ClMonad WObj
wlSeatGetPointer wobj xid = do
    xid' <- createNewId xid
    addRequest $ runByteString $ do
        put wobj
        put $ WOpc 0
        putWord16host 12
        put xid'
    pure xid'

-- | Request opc: 1 - return keyboard object
wlSeatGetKeyboard :: WObj -> Text -> ClMonad WObj
wlSeatGetKeyboard wobj xid = do
    xid' <- createNewId xid
    addRequest $ runByteString $ do
        put wobj
        put $ WOpc 1
        putWord16host 12
        put xid'
    pure xid'

-- | Request opc: 2 - return touch object
wlSeatGetTouch :: WObj -> Text -> ClMonad WObj
wlSeatGetTouch wobj xid = do
    xid' <- createNewId xid
    addRequest $ runByteString $ do
        put wobj
        put $ WOpc 2
        putWord16host 12
        put xid'
    pure xid'

-- | Request opc: 3 - release the seat object
wlSeatRelease :: WObj -> ClMonad ()
wlSeatRelease wobj = do
    addRequest $ runByteString $ do
        put wobj
        put $ WOpc 3
        putWord16host 8

-- ** Interface: WlPointer - pointer input device

-- | Request opc: 0 - set the pointer surface
wlPointerSetCursor :: WObj -> WUint -> WObj -> WInt -> WInt -> ClMonad ()
wlPointerSetCursor wobj serial surface hotspotX hotspotY = do
    addRequest $ runByteString $ do
        put wobj
        put $ WOpc 0
        putWord16host 24
        put serial
        put surface
        put hotspotX
        put hotspotY

-- | Request opc: 1 - release the pointer object
wlPointerRelease :: WObj -> ClMonad ()
wlPointerRelease wobj = do
    addRequest $ runByteString $ do
        put wobj
        put $ WOpc 1
        putWord16host 8

-- ** Interface: WlKeyboard - keyboard input device

-- | Request opc: 0 - release the keyboard object
wlKeyboardRelease :: WObj -> ClMonad ()
wlKeyboardRelease wobj = do
    addRequest $ runByteString $ do
        put wobj
        put $ WOpc 0
        putWord16host 8

-- ** Interface: WlTouch - touchscreen input device

-- | Request opc: 0 - release the touch object
wlTouchRelease :: WObj -> ClMonad ()
wlTouchRelease wobj = do
    addRequest $ runByteString $ do
        put wobj
        put $ WOpc 0
        putWord16host 8

-- ** Interface: WlOutput - compositor output region

-- | Request opc: 0 - release the output object
wlOutputRelease :: WObj -> ClMonad ()
wlOutputRelease wobj = do
    addRequest $ runByteString $ do
        put wobj
        put $ WOpc 0
        putWord16host 8

-- ** Interface: WlRegion - region interface

-- | Request opc: 0 - destroy region
wlRegionDestroy :: WObj -> ClMonad ()
wlRegionDestroy wobj = do
    addRequest $ runByteString $ do
        put wobj
        put $ WOpc 0
        putWord16host 8

-- | Request opc: 1 - add rectangle to region
wlRegionAdd :: WObj -> WInt -> WInt -> WInt -> WInt -> ClMonad ()
wlRegionAdd wobj x y width height = do
    addRequest $ runByteString $ do
        put wobj
        put $ WOpc 1
        putWord16host 24
        put x
        put y
        put width
        put height

-- | Request opc: 2 - subtract rectangle from region
wlRegionSubtract :: WObj -> WInt -> WInt -> WInt -> WInt -> ClMonad ()
wlRegionSubtract wobj x y width height = do
    addRequest $ runByteString $ do
        put wobj
        put $ WOpc 2
        putWord16host 24
        put x
        put y
        put width
        put height

-- ** Interface: WlSubcompositor - sub-surface compositing

-- | Request opc: 0 - unbind from the subcompositor interface
wlSubcompositorDestroy :: WObj -> ClMonad ()
wlSubcompositorDestroy wobj = do
    addRequest $ runByteString $ do
        put wobj
        put $ WOpc 0
        putWord16host 8

-- | Request opc: 1 - give a surface the role sub-surface
wlSubcompositorGetSubsurface :: WObj -> Text -> WObj -> WObj -> ClMonad WObj
wlSubcompositorGetSubsurface wobj xid surface parent = do
    xid' <- createNewId xid
    addRequest $ runByteString $ do
        put wobj
        put $ WOpc 1
        putWord16host 20
        put xid'
        put surface
        put parent
    pure xid'

-- ** Interface: WlSubsurface - sub-surface interface to a wl_surface

-- | Request opc: 0 - remove sub-surface interface
wlSubsurfaceDestroy :: WObj -> ClMonad ()
wlSubsurfaceDestroy wobj = do
    addRequest $ runByteString $ do
        put wobj
        put $ WOpc 0
        putWord16host 8

-- | Request opc: 1 - reposition the sub-surface
wlSubsurfaceSetPosition :: WObj -> WInt -> WInt -> ClMonad ()
wlSubsurfaceSetPosition wobj x y = do
    addRequest $ runByteString $ do
        put wobj
        put $ WOpc 1
        putWord16host 16
        put x
        put y

-- | Request opc: 2 - restack the sub-surface
wlSubsurfacePlaceAbove :: WObj -> WObj -> ClMonad ()
wlSubsurfacePlaceAbove wobj sibling = do
    addRequest $ runByteString $ do
        put wobj
        put $ WOpc 2
        putWord16host 12
        put sibling

-- | Request opc: 3 - restack the sub-surface
wlSubsurfacePlaceBelow :: WObj -> WObj -> ClMonad ()
wlSubsurfacePlaceBelow wobj sibling = do
    addRequest $ runByteString $ do
        put wobj
        put $ WOpc 3
        putWord16host 12
        put sibling

-- | Request opc: 4 - set sub-surface to synchronized mode
wlSubsurfaceSetSync :: WObj -> ClMonad ()
wlSubsurfaceSetSync wobj = do
    addRequest $ runByteString $ do
        put wobj
        put $ WOpc 4
        putWord16host 8

-- | Request opc: 5 - set sub-surface to desynchronized mode
wlSubsurfaceSetDesync :: WObj -> ClMonad ()
wlSubsurfaceSetDesync wobj = do
    addRequest $ runByteString $ do
        put wobj
        put $ WOpc 5
        putWord16host 8

-- ** Interface: XdgWmBase - create desktop-style surfaces

-- | Request opc: 0 - destroy xdg_wm_base
xdgWmBaseDestroy :: WObj -> ClMonad ()
xdgWmBaseDestroy wobj = do
    addRequest $ runByteString $ do
        put wobj
        put $ WOpc 0
        putWord16host 8

-- | Request opc: 1 - create a positioner object
xdgWmBaseCreatePositioner :: WObj -> Text -> ClMonad WObj
xdgWmBaseCreatePositioner wobj xid = do
    xid' <- createNewId xid
    addRequest $ runByteString $ do
        put wobj
        put $ WOpc 1
        putWord16host 12
        put xid'
    pure xid'

-- | Request opc: 2 - create a shell surface from a surface
xdgWmBaseGetXdgSurface :: WObj -> Text -> WObj -> ClMonad WObj
xdgWmBaseGetXdgSurface wobj xid surface = do
    xid' <- createNewId xid
    addRequest $ runByteString $ do
        put wobj
        put $ WOpc 2
        putWord16host 16
        put xid'
        put surface
    pure xid'

-- | Request opc: 3 - respond to a ping event
xdgWmBasePong :: WObj -> WUint -> ClMonad ()
xdgWmBasePong wobj serial = do
    addRequest $ runByteString $ do
        put wobj
        put $ WOpc 3
        putWord16host 12
        put serial

-- ** Interface: XdgPositioner - child surface positioner

-- | Request opc: 0 - destroy the xdg_positioner object
xdgPositionerDestroy :: WObj -> ClMonad ()
xdgPositionerDestroy wobj = do
    addRequest $ runByteString $ do
        put wobj
        put $ WOpc 0
        putWord16host 8

-- | Request opc: 1 - set the size of the to-be positioned rectangle
xdgPositionerSetSize :: WObj -> WInt -> WInt -> ClMonad ()
xdgPositionerSetSize wobj width height = do
    addRequest $ runByteString $ do
        put wobj
        put $ WOpc 1
        putWord16host 16
        put width
        put height

-- | Request opc: 2 - set the anchor rectangle within the parent surface
xdgPositionerSetAnchorRect :: WObj -> WInt -> WInt -> WInt -> WInt -> ClMonad ()
xdgPositionerSetAnchorRect wobj x y width height = do
    addRequest $ runByteString $ do
        put wobj
        put $ WOpc 2
        putWord16host 24
        put x
        put y
        put width
        put height

-- | Request opc: 3 - set anchor rectangle anchor
xdgPositionerSetAnchor :: WObj -> WUint -> ClMonad ()
xdgPositionerSetAnchor wobj anchor = do
    addRequest $ runByteString $ do
        put wobj
        put $ WOpc 3
        putWord16host 12
        put anchor

-- | Request opc: 4 - set child surface gravity
xdgPositionerSetGravity :: WObj -> WUint -> ClMonad ()
xdgPositionerSetGravity wobj gravity = do
    addRequest $ runByteString $ do
        put wobj
        put $ WOpc 4
        putWord16host 12
        put gravity

-- | Request opc: 5 - set the adjustment to be done when constrained
xdgPositionerSetConstraintAdjustment :: WObj -> WUint -> ClMonad ()
xdgPositionerSetConstraintAdjustment wobj constraintAdjustment = do
    addRequest $ runByteString $ do
        put wobj
        put $ WOpc 5
        putWord16host 12
        put constraintAdjustment

-- | Request opc: 6 - set surface position offset
xdgPositionerSetOffset :: WObj -> WInt -> WInt -> ClMonad ()
xdgPositionerSetOffset wobj x y = do
    addRequest $ runByteString $ do
        put wobj
        put $ WOpc 6
        putWord16host 16
        put x
        put y

-- | Request opc: 7 - continuously reconstrain the surface
xdgPositionerSetReactive :: WObj -> ClMonad ()
xdgPositionerSetReactive wobj = do
    addRequest $ runByteString $ do
        put wobj
        put $ WOpc 7
        putWord16host 8

-- | Request opc: 8 - 
xdgPositionerSetParentSize :: WObj -> WInt -> WInt -> ClMonad ()
xdgPositionerSetParentSize wobj parentWidth parentHeight = do
    addRequest $ runByteString $ do
        put wobj
        put $ WOpc 8
        putWord16host 16
        put parentWidth
        put parentHeight

-- | Request opc: 9 - set parent configure this is a response to
xdgPositionerSetParentConfigure :: WObj -> WUint -> ClMonad ()
xdgPositionerSetParentConfigure wobj serial = do
    addRequest $ runByteString $ do
        put wobj
        put $ WOpc 9
        putWord16host 12
        put serial

-- ** Interface: XdgSurface - desktop user interface surface base interface

-- | Request opc: 0 - destroy the xdg_surface
xdgSurfaceDestroy :: WObj -> ClMonad ()
xdgSurfaceDestroy wobj = do
    addRequest $ runByteString $ do
        put wobj
        put $ WOpc 0
        putWord16host 8

-- | Request opc: 1 - assign the xdg_toplevel surface role
xdgSurfaceGetToplevel :: WObj -> Text -> ClMonad WObj
xdgSurfaceGetToplevel wobj xid = do
    xid' <- createNewId xid
    addRequest $ runByteString $ do
        put wobj
        put $ WOpc 1
        putWord16host 12
        put xid'
    pure xid'

-- | Request opc: 2 - assign the xdg_popup surface role
xdgSurfaceGetPopup :: WObj -> Text -> WObj -> WObj -> ClMonad WObj
xdgSurfaceGetPopup wobj xid parent positioner = do
    xid' <- createNewId xid
    addRequest $ runByteString $ do
        put wobj
        put $ WOpc 2
        putWord16host 20
        put xid'
        put parent
        put positioner
    pure xid'

-- | Request opc: 3 - set the new window geometry
xdgSurfaceSetWindowGeometry :: WObj -> WInt -> WInt -> WInt -> WInt -> ClMonad ()
xdgSurfaceSetWindowGeometry wobj x y width height = do
    addRequest $ runByteString $ do
        put wobj
        put $ WOpc 3
        putWord16host 24
        put x
        put y
        put width
        put height

-- | Request opc: 4 - ack a configure event
xdgSurfaceAckConfigure :: WObj -> WUint -> ClMonad ()
xdgSurfaceAckConfigure wobj serial = do
    addRequest $ runByteString $ do
        put wobj
        put $ WOpc 4
        putWord16host 12
        put serial

-- ** Interface: XdgToplevel - toplevel surface

-- | Request opc: 0 - destroy the xdg_toplevel
xdgToplevelDestroy :: WObj -> ClMonad ()
xdgToplevelDestroy wobj = do
    addRequest $ runByteString $ do
        put wobj
        put $ WOpc 0
        putWord16host 8

-- | Request opc: 1 - set the parent of this surface
xdgToplevelSetParent :: WObj -> WObj -> ClMonad ()
xdgToplevelSetParent wobj parent = do
    addRequest $ runByteString $ do
        put wobj
        put $ WOpc 1
        putWord16host 12
        put parent

-- | Request opc: 2 - set surface title
xdgToplevelSetTitle :: WObj -> WString -> ClMonad ()
xdgToplevelSetTitle wobj title = do
    addRequest $ runByteString $ do
        put wobj
        put $ WOpc 2
        putWord16host $ fromIntegral len
        put title
  where len = 8 + sum (calcWStringLength <$> [title])  + sum (calcWArrayLength  <$> []) 

-- | Request opc: 3 - set application ID
xdgToplevelSetAppId :: WObj -> WString -> ClMonad ()
xdgToplevelSetAppId wobj appId = do
    addRequest $ runByteString $ do
        put wobj
        put $ WOpc 3
        putWord16host $ fromIntegral len
        put appId
  where len = 8 + sum (calcWStringLength <$> [appId])  + sum (calcWArrayLength  <$> []) 

-- | Request opc: 4 - show the window menu
xdgToplevelShowWindowMenu :: WObj -> WObj -> WUint -> WInt -> WInt -> ClMonad ()
xdgToplevelShowWindowMenu wobj seat serial x y = do
    addRequest $ runByteString $ do
        put wobj
        put $ WOpc 4
        putWord16host 24
        put seat
        put serial
        put x
        put y

-- | Request opc: 5 - start an interactive move
xdgToplevelMove :: WObj -> WObj -> WUint -> ClMonad ()
xdgToplevelMove wobj seat serial = do
    addRequest $ runByteString $ do
        put wobj
        put $ WOpc 5
        putWord16host 16
        put seat
        put serial

-- | Request opc: 6 - start an interactive resize
xdgToplevelResize :: WObj -> WObj -> WUint -> WUint -> ClMonad ()
xdgToplevelResize wobj seat serial edges = do
    addRequest $ runByteString $ do
        put wobj
        put $ WOpc 6
        putWord16host 20
        put seat
        put serial
        put edges

-- | Request opc: 7 - set the maximum size
xdgToplevelSetMaxSize :: WObj -> WInt -> WInt -> ClMonad ()
xdgToplevelSetMaxSize wobj width height = do
    addRequest $ runByteString $ do
        put wobj
        put $ WOpc 7
        putWord16host 16
        put width
        put height

-- | Request opc: 8 - set the minimum size
xdgToplevelSetMinSize :: WObj -> WInt -> WInt -> ClMonad ()
xdgToplevelSetMinSize wobj width height = do
    addRequest $ runByteString $ do
        put wobj
        put $ WOpc 8
        putWord16host 16
        put width
        put height

-- | Request opc: 9 - maximize the window
xdgToplevelSetMaximized :: WObj -> ClMonad ()
xdgToplevelSetMaximized wobj = do
    addRequest $ runByteString $ do
        put wobj
        put $ WOpc 9
        putWord16host 8

-- | Request opc: 10 - unmaximize the window
xdgToplevelUnsetMaximized :: WObj -> ClMonad ()
xdgToplevelUnsetMaximized wobj = do
    addRequest $ runByteString $ do
        put wobj
        put $ WOpc 10
        putWord16host 8

-- | Request opc: 11 - set the window as fullscreen on an output
xdgToplevelSetFullscreen :: WObj -> WObj -> ClMonad ()
xdgToplevelSetFullscreen wobj output = do
    addRequest $ runByteString $ do
        put wobj
        put $ WOpc 11
        putWord16host 12
        put output

-- | Request opc: 12 - unset the window as fullscreen
xdgToplevelUnsetFullscreen :: WObj -> ClMonad ()
xdgToplevelUnsetFullscreen wobj = do
    addRequest $ runByteString $ do
        put wobj
        put $ WOpc 12
        putWord16host 8

-- | Request opc: 13 - set the window as minimized
xdgToplevelSetMinimized :: WObj -> ClMonad ()
xdgToplevelSetMinimized wobj = do
    addRequest $ runByteString $ do
        put wobj
        put $ WOpc 13
        putWord16host 8

-- ** Interface: XdgPopup - short-lived, popup surfaces for menus

-- | Request opc: 0 - remove xdg_popup interface
xdgPopupDestroy :: WObj -> ClMonad ()
xdgPopupDestroy wobj = do
    addRequest $ runByteString $ do
        put wobj
        put $ WOpc 0
        putWord16host 8

-- | Request opc: 1 - make the popup take an explicit grab
xdgPopupGrab :: WObj -> WObj -> WUint -> ClMonad ()
xdgPopupGrab wobj seat serial = do
    addRequest $ runByteString $ do
        put wobj
        put $ WOpc 1
        putWord16host 16
        put seat
        put serial

-- | Request opc: 2 - recalculate the popup's location
xdgPopupReposition :: WObj -> WObj -> WUint -> ClMonad ()
xdgPopupReposition wobj positioner token = do
    addRequest $ runByteString $ do
        put wobj
        put $ WOpc 2
        putWord16host 16
        put positioner
        put token

-- ----------------------------------------------------------------------
-- * Event Handling
-- ----------------------------------------------------------------------
-- ** Event Handling Function Types
type TwlDisplayError = WObj -> WObj -> WUint -> WString -> ClMonad ()
type TwlDisplayDeleteId = WObj -> WUint -> ClMonad ()
type TwlRegistryGlobal = WObj -> WUint -> WString -> WUint -> ClMonad ()
type TwlRegistryGlobalRemove = WObj -> WUint -> ClMonad ()
type TwlCallbackDone = WObj -> WUint -> ClMonad ()
type TwlShmFormat = WObj -> WUint -> ClMonad ()
type TwlBufferRelease = WObj -> ClMonad ()
type TwlDataOfferOffer = WObj -> WString -> ClMonad ()
type TwlDataOfferSourceActions = WObj -> WUint -> ClMonad ()
type TwlDataOfferAction = WObj -> WUint -> ClMonad ()
type TwlDataSourceTarget = WObj -> WString -> ClMonad ()
type TwlDataSourceSend = WObj -> WString -> Fd -> ClMonad ()
type TwlDataSourceCancelled = WObj -> ClMonad ()
type TwlDataSourceDndDropPerformed = WObj -> ClMonad ()
type TwlDataSourceDndFinished = WObj -> ClMonad ()
type TwlDataSourceAction = WObj -> WUint -> ClMonad ()
type TwlDataDeviceDataOffer = WObj -> WNewId -> ClMonad ()
type TwlDataDeviceEnter = WObj -> WUint -> WObj -> WFixed -> WFixed -> WObj -> ClMonad ()
type TwlDataDeviceLeave = WObj -> ClMonad ()
type TwlDataDeviceMotion = WObj -> WUint -> WFixed -> WFixed -> ClMonad ()
type TwlDataDeviceDrop = WObj -> ClMonad ()
type TwlDataDeviceSelection = WObj -> WObj -> ClMonad ()
type TwlShellSurfacePing = WObj -> WUint -> ClMonad ()
type TwlShellSurfaceConfigure = WObj -> WUint -> WInt -> WInt -> ClMonad ()
type TwlShellSurfacePopupDone = WObj -> ClMonad ()
type TwlSurfaceEnter = WObj -> WObj -> ClMonad ()
type TwlSurfaceLeave = WObj -> WObj -> ClMonad ()
type TwlSeatCapabilities = WObj -> WUint -> ClMonad ()
type TwlSeatName = WObj -> WString -> ClMonad ()
type TwlPointerEnter = WObj -> WUint -> WObj -> WFixed -> WFixed -> ClMonad ()
type TwlPointerLeave = WObj -> WUint -> WObj -> ClMonad ()
type TwlPointerMotion = WObj -> WUint -> WFixed -> WFixed -> ClMonad ()
type TwlPointerButton = WObj -> WUint -> WUint -> WUint -> WUint -> ClMonad ()
type TwlPointerAxis = WObj -> WUint -> WUint -> WFixed -> ClMonad ()
type TwlPointerFrame = WObj -> ClMonad ()
type TwlPointerAxisSource = WObj -> WUint -> ClMonad ()
type TwlPointerAxisStop = WObj -> WUint -> WUint -> ClMonad ()
type TwlPointerAxisDiscrete = WObj -> WUint -> WInt -> ClMonad ()
type TwlKeyboardKeymap = WObj -> WUint -> Fd -> WUint -> ClMonad ()
type TwlKeyboardEnter = WObj -> WUint -> WObj -> WArray -> ClMonad ()
type TwlKeyboardLeave = WObj -> WUint -> WObj -> ClMonad ()
type TwlKeyboardKey = WObj -> WUint -> WUint -> WUint -> WUint -> ClMonad ()
type TwlKeyboardModifiers = WObj -> WUint -> WUint -> WUint -> WUint -> WUint -> ClMonad ()
type TwlKeyboardRepeatInfo = WObj -> WInt -> WInt -> ClMonad ()
type TwlTouchDown = WObj -> WUint -> WUint -> WObj -> WInt -> WFixed -> WFixed -> ClMonad ()
type TwlTouchUp = WObj -> WUint -> WUint -> WInt -> ClMonad ()
type TwlTouchMotion = WObj -> WUint -> WInt -> WFixed -> WFixed -> ClMonad ()
type TwlTouchFrame = WObj -> ClMonad ()
type TwlTouchCancel = WObj -> ClMonad ()
type TwlTouchShape = WObj -> WInt -> WFixed -> WFixed -> ClMonad ()
type TwlTouchOrientation = WObj -> WInt -> WFixed -> ClMonad ()
type TwlOutputGeometry = WObj -> WInt -> WInt -> WInt -> WInt -> WInt -> WString -> WString -> WInt -> ClMonad ()
type TwlOutputMode = WObj -> WUint -> WInt -> WInt -> WInt -> ClMonad ()
type TwlOutputDone = WObj -> ClMonad ()
type TwlOutputScale = WObj -> WInt -> ClMonad ()
type TxdgWmBasePing = WObj -> WUint -> ClMonad ()
type TxdgSurfaceConfigure = WObj -> WUint -> ClMonad ()
type TxdgToplevelConfigure = WObj -> WInt -> WInt -> WArray -> ClMonad ()
type TxdgToplevelClose = WObj -> ClMonad ()
type TxdgPopupConfigure = WObj -> WInt -> WInt -> WInt -> WInt -> ClMonad ()
type TxdgPopupPopupDone = WObj -> ClMonad ()
type TxdgPopupRepositioned = WObj -> WUint -> ClMonad ()

-- ** Proxy Functions for all Events

pwlDisplayError :: Maybe TwlDisplayError -> WInputMsg -> ClMonad ()
pwlDisplayError Nothing _ = pure ()
pwlDisplayError (Just f) msg = runGet g $ BL.fromStrict $ winpData msg
    where g = f (winpObj msg) <$> get <*> get <*> get

pwlDisplayDeleteId :: Maybe TwlDisplayDeleteId -> WInputMsg -> ClMonad ()
pwlDisplayDeleteId Nothing _ = pure ()
pwlDisplayDeleteId (Just f) msg = runGet g $ BL.fromStrict $ winpData msg
    where g = f (winpObj msg) <$> get

pwlRegistryGlobal :: Maybe TwlRegistryGlobal -> WInputMsg -> ClMonad ()
pwlRegistryGlobal Nothing _ = pure ()
pwlRegistryGlobal (Just f) msg = runGet g $ BL.fromStrict $ winpData msg
    where g = f (winpObj msg) <$> get <*> get <*> get

pwlRegistryGlobalRemove :: Maybe TwlRegistryGlobalRemove -> WInputMsg -> ClMonad ()
pwlRegistryGlobalRemove Nothing _ = pure ()
pwlRegistryGlobalRemove (Just f) msg = runGet g $ BL.fromStrict $ winpData msg
    where g = f (winpObj msg) <$> get

pwlCallbackDone :: Maybe TwlCallbackDone -> WInputMsg -> ClMonad ()
pwlCallbackDone Nothing _ = pure ()
pwlCallbackDone (Just f) msg = runGet g $ BL.fromStrict $ winpData msg
    where g = f (winpObj msg) <$> get

pwlShmFormat :: Maybe TwlShmFormat -> WInputMsg -> ClMonad ()
pwlShmFormat Nothing _ = pure ()
pwlShmFormat (Just f) msg = runGet g $ BL.fromStrict $ winpData msg
    where g = f (winpObj msg) <$> get

pwlBufferRelease :: Maybe TwlBufferRelease -> WInputMsg -> ClMonad ()
pwlBufferRelease Nothing _ = pure ()
pwlBufferRelease (Just f) msg = f (winpObj msg)

pwlDataOfferOffer :: Maybe TwlDataOfferOffer -> WInputMsg -> ClMonad ()
pwlDataOfferOffer Nothing _ = pure ()
pwlDataOfferOffer (Just f) msg = runGet g $ BL.fromStrict $ winpData msg
    where g = f (winpObj msg) <$> get

pwlDataOfferSourceActions :: Maybe TwlDataOfferSourceActions -> WInputMsg -> ClMonad ()
pwlDataOfferSourceActions Nothing _ = pure ()
pwlDataOfferSourceActions (Just f) msg = runGet g $ BL.fromStrict $ winpData msg
    where g = f (winpObj msg) <$> get

pwlDataOfferAction :: Maybe TwlDataOfferAction -> WInputMsg -> ClMonad ()
pwlDataOfferAction Nothing _ = pure ()
pwlDataOfferAction (Just f) msg = runGet g $ BL.fromStrict $ winpData msg
    where g = f (winpObj msg) <$> get

pwlDataSourceTarget :: Maybe TwlDataSourceTarget -> WInputMsg -> ClMonad ()
pwlDataSourceTarget Nothing _ = pure ()
pwlDataSourceTarget (Just f) msg = runGet g $ BL.fromStrict $ winpData msg
    where g = f (winpObj msg) <$> get

pwlDataSourceSend :: Maybe TwlDataSourceSend -> WInputMsg -> ClMonad ()
pwlDataSourceSend Nothing _ = pure ()
pwlDataSourceSend (Just f) msg = runGet g $ BL.fromStrict $ winpData msg
    where g = f (winpObj msg) <$> get <*> get

pwlDataSourceCancelled :: Maybe TwlDataSourceCancelled -> WInputMsg -> ClMonad ()
pwlDataSourceCancelled Nothing _ = pure ()
pwlDataSourceCancelled (Just f) msg = f (winpObj msg)

pwlDataSourceDndDropPerformed :: Maybe TwlDataSourceDndDropPerformed -> WInputMsg -> ClMonad ()
pwlDataSourceDndDropPerformed Nothing _ = pure ()
pwlDataSourceDndDropPerformed (Just f) msg = f (winpObj msg)

pwlDataSourceDndFinished :: Maybe TwlDataSourceDndFinished -> WInputMsg -> ClMonad ()
pwlDataSourceDndFinished Nothing _ = pure ()
pwlDataSourceDndFinished (Just f) msg = f (winpObj msg)

pwlDataSourceAction :: Maybe TwlDataSourceAction -> WInputMsg -> ClMonad ()
pwlDataSourceAction Nothing _ = pure ()
pwlDataSourceAction (Just f) msg = runGet g $ BL.fromStrict $ winpData msg
    where g = f (winpObj msg) <$> get

pwlDataDeviceDataOffer :: Maybe TwlDataDeviceDataOffer -> WInputMsg -> ClMonad ()
pwlDataDeviceDataOffer Nothing _ = pure ()
pwlDataDeviceDataOffer (Just f) msg = runGet g $ BL.fromStrict $ winpData msg
    where g = f (winpObj msg) <$> get

pwlDataDeviceEnter :: Maybe TwlDataDeviceEnter -> WInputMsg -> ClMonad ()
pwlDataDeviceEnter Nothing _ = pure ()
pwlDataDeviceEnter (Just f) msg = runGet g $ BL.fromStrict $ winpData msg
    where g = f (winpObj msg) <$> get <*> get <*> get <*> get <*> get

pwlDataDeviceLeave :: Maybe TwlDataDeviceLeave -> WInputMsg -> ClMonad ()
pwlDataDeviceLeave Nothing _ = pure ()
pwlDataDeviceLeave (Just f) msg = f (winpObj msg)

pwlDataDeviceMotion :: Maybe TwlDataDeviceMotion -> WInputMsg -> ClMonad ()
pwlDataDeviceMotion Nothing _ = pure ()
pwlDataDeviceMotion (Just f) msg = runGet g $ BL.fromStrict $ winpData msg
    where g = f (winpObj msg) <$> get <*> get <*> get

pwlDataDeviceDrop :: Maybe TwlDataDeviceDrop -> WInputMsg -> ClMonad ()
pwlDataDeviceDrop Nothing _ = pure ()
pwlDataDeviceDrop (Just f) msg = f (winpObj msg)

pwlDataDeviceSelection :: Maybe TwlDataDeviceSelection -> WInputMsg -> ClMonad ()
pwlDataDeviceSelection Nothing _ = pure ()
pwlDataDeviceSelection (Just f) msg = runGet g $ BL.fromStrict $ winpData msg
    where g = f (winpObj msg) <$> get

pwlShellSurfacePing :: Maybe TwlShellSurfacePing -> WInputMsg -> ClMonad ()
pwlShellSurfacePing Nothing _ = pure ()
pwlShellSurfacePing (Just f) msg = runGet g $ BL.fromStrict $ winpData msg
    where g = f (winpObj msg) <$> get

pwlShellSurfaceConfigure :: Maybe TwlShellSurfaceConfigure -> WInputMsg -> ClMonad ()
pwlShellSurfaceConfigure Nothing _ = pure ()
pwlShellSurfaceConfigure (Just f) msg = runGet g $ BL.fromStrict $ winpData msg
    where g = f (winpObj msg) <$> get <*> get <*> get

pwlShellSurfacePopupDone :: Maybe TwlShellSurfacePopupDone -> WInputMsg -> ClMonad ()
pwlShellSurfacePopupDone Nothing _ = pure ()
pwlShellSurfacePopupDone (Just f) msg = f (winpObj msg)

pwlSurfaceEnter :: Maybe TwlSurfaceEnter -> WInputMsg -> ClMonad ()
pwlSurfaceEnter Nothing _ = pure ()
pwlSurfaceEnter (Just f) msg = runGet g $ BL.fromStrict $ winpData msg
    where g = f (winpObj msg) <$> get

pwlSurfaceLeave :: Maybe TwlSurfaceLeave -> WInputMsg -> ClMonad ()
pwlSurfaceLeave Nothing _ = pure ()
pwlSurfaceLeave (Just f) msg = runGet g $ BL.fromStrict $ winpData msg
    where g = f (winpObj msg) <$> get

pwlSeatCapabilities :: Maybe TwlSeatCapabilities -> WInputMsg -> ClMonad ()
pwlSeatCapabilities Nothing _ = pure ()
pwlSeatCapabilities (Just f) msg = runGet g $ BL.fromStrict $ winpData msg
    where g = f (winpObj msg) <$> get

pwlSeatName :: Maybe TwlSeatName -> WInputMsg -> ClMonad ()
pwlSeatName Nothing _ = pure ()
pwlSeatName (Just f) msg = runGet g $ BL.fromStrict $ winpData msg
    where g = f (winpObj msg) <$> get

pwlPointerEnter :: Maybe TwlPointerEnter -> WInputMsg -> ClMonad ()
pwlPointerEnter Nothing _ = pure ()
pwlPointerEnter (Just f) msg = runGet g $ BL.fromStrict $ winpData msg
    where g = f (winpObj msg) <$> get <*> get <*> get <*> get

pwlPointerLeave :: Maybe TwlPointerLeave -> WInputMsg -> ClMonad ()
pwlPointerLeave Nothing _ = pure ()
pwlPointerLeave (Just f) msg = runGet g $ BL.fromStrict $ winpData msg
    where g = f (winpObj msg) <$> get <*> get

pwlPointerMotion :: Maybe TwlPointerMotion -> WInputMsg -> ClMonad ()
pwlPointerMotion Nothing _ = pure ()
pwlPointerMotion (Just f) msg = runGet g $ BL.fromStrict $ winpData msg
    where g = f (winpObj msg) <$> get <*> get <*> get

pwlPointerButton :: Maybe TwlPointerButton -> WInputMsg -> ClMonad ()
pwlPointerButton Nothing _ = pure ()
pwlPointerButton (Just f) msg = runGet g $ BL.fromStrict $ winpData msg
    where g = f (winpObj msg) <$> get <*> get <*> get <*> get

pwlPointerAxis :: Maybe TwlPointerAxis -> WInputMsg -> ClMonad ()
pwlPointerAxis Nothing _ = pure ()
pwlPointerAxis (Just f) msg = runGet g $ BL.fromStrict $ winpData msg
    where g = f (winpObj msg) <$> get <*> get <*> get

pwlPointerFrame :: Maybe TwlPointerFrame -> WInputMsg -> ClMonad ()
pwlPointerFrame Nothing _ = pure ()
pwlPointerFrame (Just f) msg = f (winpObj msg)

pwlPointerAxisSource :: Maybe TwlPointerAxisSource -> WInputMsg -> ClMonad ()
pwlPointerAxisSource Nothing _ = pure ()
pwlPointerAxisSource (Just f) msg = runGet g $ BL.fromStrict $ winpData msg
    where g = f (winpObj msg) <$> get

pwlPointerAxisStop :: Maybe TwlPointerAxisStop -> WInputMsg -> ClMonad ()
pwlPointerAxisStop Nothing _ = pure ()
pwlPointerAxisStop (Just f) msg = runGet g $ BL.fromStrict $ winpData msg
    where g = f (winpObj msg) <$> get <*> get

pwlPointerAxisDiscrete :: Maybe TwlPointerAxisDiscrete -> WInputMsg -> ClMonad ()
pwlPointerAxisDiscrete Nothing _ = pure ()
pwlPointerAxisDiscrete (Just f) msg = runGet g $ BL.fromStrict $ winpData msg
    where g = f (winpObj msg) <$> get <*> get

pwlKeyboardKeymap :: Maybe TwlKeyboardKeymap -> WInputMsg -> ClMonad ()
pwlKeyboardKeymap Nothing _ = pure ()
pwlKeyboardKeymap (Just f) msg = runGet g $ BL.fromStrict $ winpData msg
    where g = f (winpObj msg) <$> get <*> get <*> get

pwlKeyboardEnter :: Maybe TwlKeyboardEnter -> WInputMsg -> ClMonad ()
pwlKeyboardEnter Nothing _ = pure ()
pwlKeyboardEnter (Just f) msg = runGet g $ BL.fromStrict $ winpData msg
    where g = f (winpObj msg) <$> get <*> get <*> get

pwlKeyboardLeave :: Maybe TwlKeyboardLeave -> WInputMsg -> ClMonad ()
pwlKeyboardLeave Nothing _ = pure ()
pwlKeyboardLeave (Just f) msg = runGet g $ BL.fromStrict $ winpData msg
    where g = f (winpObj msg) <$> get <*> get

pwlKeyboardKey :: Maybe TwlKeyboardKey -> WInputMsg -> ClMonad ()
pwlKeyboardKey Nothing _ = pure ()
pwlKeyboardKey (Just f) msg = runGet g $ BL.fromStrict $ winpData msg
    where g = f (winpObj msg) <$> get <*> get <*> get <*> get

pwlKeyboardModifiers :: Maybe TwlKeyboardModifiers -> WInputMsg -> ClMonad ()
pwlKeyboardModifiers Nothing _ = pure ()
pwlKeyboardModifiers (Just f) msg = runGet g $ BL.fromStrict $ winpData msg
    where g = f (winpObj msg) <$> get <*> get <*> get <*> get <*> get

pwlKeyboardRepeatInfo :: Maybe TwlKeyboardRepeatInfo -> WInputMsg -> ClMonad ()
pwlKeyboardRepeatInfo Nothing _ = pure ()
pwlKeyboardRepeatInfo (Just f) msg = runGet g $ BL.fromStrict $ winpData msg
    where g = f (winpObj msg) <$> get <*> get

pwlTouchDown :: Maybe TwlTouchDown -> WInputMsg -> ClMonad ()
pwlTouchDown Nothing _ = pure ()
pwlTouchDown (Just f) msg = runGet g $ BL.fromStrict $ winpData msg
    where g = f (winpObj msg) <$> get <*> get <*> get <*> get <*> get <*> get

pwlTouchUp :: Maybe TwlTouchUp -> WInputMsg -> ClMonad ()
pwlTouchUp Nothing _ = pure ()
pwlTouchUp (Just f) msg = runGet g $ BL.fromStrict $ winpData msg
    where g = f (winpObj msg) <$> get <*> get <*> get

pwlTouchMotion :: Maybe TwlTouchMotion -> WInputMsg -> ClMonad ()
pwlTouchMotion Nothing _ = pure ()
pwlTouchMotion (Just f) msg = runGet g $ BL.fromStrict $ winpData msg
    where g = f (winpObj msg) <$> get <*> get <*> get <*> get

pwlTouchFrame :: Maybe TwlTouchFrame -> WInputMsg -> ClMonad ()
pwlTouchFrame Nothing _ = pure ()
pwlTouchFrame (Just f) msg = f (winpObj msg)

pwlTouchCancel :: Maybe TwlTouchCancel -> WInputMsg -> ClMonad ()
pwlTouchCancel Nothing _ = pure ()
pwlTouchCancel (Just f) msg = f (winpObj msg)

pwlTouchShape :: Maybe TwlTouchShape -> WInputMsg -> ClMonad ()
pwlTouchShape Nothing _ = pure ()
pwlTouchShape (Just f) msg = runGet g $ BL.fromStrict $ winpData msg
    where g = f (winpObj msg) <$> get <*> get <*> get

pwlTouchOrientation :: Maybe TwlTouchOrientation -> WInputMsg -> ClMonad ()
pwlTouchOrientation Nothing _ = pure ()
pwlTouchOrientation (Just f) msg = runGet g $ BL.fromStrict $ winpData msg
    where g = f (winpObj msg) <$> get <*> get

pwlOutputGeometry :: Maybe TwlOutputGeometry -> WInputMsg -> ClMonad ()
pwlOutputGeometry Nothing _ = pure ()
pwlOutputGeometry (Just f) msg = runGet g $ BL.fromStrict $ winpData msg
    where g = f (winpObj msg) <$> get <*> get <*> get <*> get <*> get <*> get <*> get <*> get

pwlOutputMode :: Maybe TwlOutputMode -> WInputMsg -> ClMonad ()
pwlOutputMode Nothing _ = pure ()
pwlOutputMode (Just f) msg = runGet g $ BL.fromStrict $ winpData msg
    where g = f (winpObj msg) <$> get <*> get <*> get <*> get

pwlOutputDone :: Maybe TwlOutputDone -> WInputMsg -> ClMonad ()
pwlOutputDone Nothing _ = pure ()
pwlOutputDone (Just f) msg = f (winpObj msg)

pwlOutputScale :: Maybe TwlOutputScale -> WInputMsg -> ClMonad ()
pwlOutputScale Nothing _ = pure ()
pwlOutputScale (Just f) msg = runGet g $ BL.fromStrict $ winpData msg
    where g = f (winpObj msg) <$> get

pxdgWmBasePing :: Maybe TxdgWmBasePing -> WInputMsg -> ClMonad ()
pxdgWmBasePing Nothing _ = pure ()
pxdgWmBasePing (Just f) msg = runGet g $ BL.fromStrict $ winpData msg
    where g = f (winpObj msg) <$> get

pxdgSurfaceConfigure :: Maybe TxdgSurfaceConfigure -> WInputMsg -> ClMonad ()
pxdgSurfaceConfigure Nothing _ = pure ()
pxdgSurfaceConfigure (Just f) msg = runGet g $ BL.fromStrict $ winpData msg
    where g = f (winpObj msg) <$> get

pxdgToplevelConfigure :: Maybe TxdgToplevelConfigure -> WInputMsg -> ClMonad ()
pxdgToplevelConfigure Nothing _ = pure ()
pxdgToplevelConfigure (Just f) msg = runGet g $ BL.fromStrict $ winpData msg
    where g = f (winpObj msg) <$> get <*> get <*> get

pxdgToplevelClose :: Maybe TxdgToplevelClose -> WInputMsg -> ClMonad ()
pxdgToplevelClose Nothing _ = pure ()
pxdgToplevelClose (Just f) msg = f (winpObj msg)

pxdgPopupConfigure :: Maybe TxdgPopupConfigure -> WInputMsg -> ClMonad ()
pxdgPopupConfigure Nothing _ = pure ()
pxdgPopupConfigure (Just f) msg = runGet g $ BL.fromStrict $ winpData msg
    where g = f (winpObj msg) <$> get <*> get <*> get <*> get

pxdgPopupPopupDone :: Maybe TxdgPopupPopupDone -> WInputMsg -> ClMonad ()
pxdgPopupPopupDone Nothing _ = pure ()
pxdgPopupPopupDone (Just f) msg = f (winpObj msg)

pxdgPopupRepositioned :: Maybe TxdgPopupRepositioned -> WInputMsg -> ClMonad ()
pxdgPopupRepositioned Nothing _ = pure ()
pxdgPopupRepositioned (Just f) msg = runGet g $ BL.fromStrict $ winpData msg
    where g = f (winpObj msg) <$> get

-- ** Listeners for all Interfaces

data WlDisplayListener = WlDisplayListener
    {wlDisplayError :: Maybe TwlDisplayError
    , wlDisplayDeleteId :: Maybe TwlDisplayDeleteId
    }
data WlRegistryListener = WlRegistryListener
    {wlRegistryGlobal :: Maybe TwlRegistryGlobal
    , wlRegistryGlobalRemove :: Maybe TwlRegistryGlobalRemove
    }
newtype WlCallbackListener = WlCallbackListener
    {wlCallbackDone :: Maybe TwlCallbackDone
    }
newtype WlShmListener = WlShmListener
    {wlShmFormat :: Maybe TwlShmFormat
    }
newtype WlBufferListener = WlBufferListener
    {wlBufferRelease :: Maybe TwlBufferRelease
    }
data WlDataOfferListener = WlDataOfferListener
    {wlDataOfferOffer :: Maybe TwlDataOfferOffer
    , wlDataOfferSourceActions :: Maybe TwlDataOfferSourceActions
    , wlDataOfferAction :: Maybe TwlDataOfferAction
    }
data WlDataSourceListener = WlDataSourceListener
    {wlDataSourceTarget :: Maybe TwlDataSourceTarget
    , wlDataSourceSend :: Maybe TwlDataSourceSend
    , wlDataSourceCancelled :: Maybe TwlDataSourceCancelled
    , wlDataSourceDndDropPerformed :: Maybe TwlDataSourceDndDropPerformed
    , wlDataSourceDndFinished :: Maybe TwlDataSourceDndFinished
    , wlDataSourceAction :: Maybe TwlDataSourceAction
    }
data WlDataDeviceListener = WlDataDeviceListener
    {wlDataDeviceDataOffer :: Maybe TwlDataDeviceDataOffer
    , wlDataDeviceEnter :: Maybe TwlDataDeviceEnter
    , wlDataDeviceLeave :: Maybe TwlDataDeviceLeave
    , wlDataDeviceMotion :: Maybe TwlDataDeviceMotion
    , wlDataDeviceDrop :: Maybe TwlDataDeviceDrop
    , wlDataDeviceSelection :: Maybe TwlDataDeviceSelection
    }
data WlShellSurfaceListener = WlShellSurfaceListener
    {wlShellSurfacePing :: Maybe TwlShellSurfacePing
    , wlShellSurfaceConfigure :: Maybe TwlShellSurfaceConfigure
    , wlShellSurfacePopupDone :: Maybe TwlShellSurfacePopupDone
    }
data WlSurfaceListener = WlSurfaceListener
    {wlSurfaceEnter :: Maybe TwlSurfaceEnter
    , wlSurfaceLeave :: Maybe TwlSurfaceLeave
    }
data WlSeatListener = WlSeatListener
    {wlSeatCapabilities :: Maybe TwlSeatCapabilities
    , wlSeatName :: Maybe TwlSeatName
    }
data WlPointerListener = WlPointerListener
    {wlPointerEnter :: Maybe TwlPointerEnter
    , wlPointerLeave :: Maybe TwlPointerLeave
    , wlPointerMotion :: Maybe TwlPointerMotion
    , wlPointerButton :: Maybe TwlPointerButton
    , wlPointerAxis :: Maybe TwlPointerAxis
    , wlPointerFrame :: Maybe TwlPointerFrame
    , wlPointerAxisSource :: Maybe TwlPointerAxisSource
    , wlPointerAxisStop :: Maybe TwlPointerAxisStop
    , wlPointerAxisDiscrete :: Maybe TwlPointerAxisDiscrete
    }
data WlKeyboardListener = WlKeyboardListener
    {wlKeyboardKeymap :: Maybe TwlKeyboardKeymap
    , wlKeyboardEnter :: Maybe TwlKeyboardEnter
    , wlKeyboardLeave :: Maybe TwlKeyboardLeave
    , wlKeyboardKey :: Maybe TwlKeyboardKey
    , wlKeyboardModifiers :: Maybe TwlKeyboardModifiers
    , wlKeyboardRepeatInfo :: Maybe TwlKeyboardRepeatInfo
    }
data WlTouchListener = WlTouchListener
    {wlTouchDown :: Maybe TwlTouchDown
    , wlTouchUp :: Maybe TwlTouchUp
    , wlTouchMotion :: Maybe TwlTouchMotion
    , wlTouchFrame :: Maybe TwlTouchFrame
    , wlTouchCancel :: Maybe TwlTouchCancel
    , wlTouchShape :: Maybe TwlTouchShape
    , wlTouchOrientation :: Maybe TwlTouchOrientation
    }
data WlOutputListener = WlOutputListener
    {wlOutputGeometry :: Maybe TwlOutputGeometry
    , wlOutputMode :: Maybe TwlOutputMode
    , wlOutputDone :: Maybe TwlOutputDone
    , wlOutputScale :: Maybe TwlOutputScale
    }
newtype XdgWmBaseListener = XdgWmBaseListener
    {xdgWmBasePing :: Maybe TxdgWmBasePing
    }
newtype XdgSurfaceListener = XdgSurfaceListener
    {xdgSurfaceConfigure :: Maybe TxdgSurfaceConfigure
    }
data XdgToplevelListener = XdgToplevelListener
    {xdgToplevelConfigure :: Maybe TxdgToplevelConfigure
    , xdgToplevelClose :: Maybe TxdgToplevelClose
    }
data XdgPopupListener = XdgPopupListener
    {xdgPopupConfigure :: Maybe TxdgPopupConfigure
    , xdgPopupPopupDone :: Maybe TxdgPopupPopupDone
    , xdgPopupRepositioned :: Maybe TxdgPopupRepositioned
    }

-- data type ClState

data ClState = ClState {
    clDisplayListener :: Maybe WlDisplayListener,
    clRegistryListener :: Maybe WlRegistryListener,
    clCallbackListener :: Maybe WlCallbackListener,
    clShmListener :: Maybe WlShmListener,
    clBufferListener :: Maybe WlBufferListener,
    clDataOfferListener :: Maybe WlDataOfferListener,
    clDataSourceListener :: Maybe WlDataSourceListener,
    clDataDeviceListener :: Maybe WlDataDeviceListener,
    clShellSurfaceListener :: Maybe WlShellSurfaceListener,
    clSurfaceListener :: Maybe WlSurfaceListener,
    clSeatListener :: Maybe WlSeatListener,
    clPointerListener :: Maybe WlPointerListener,
    clKeyboardListener :: Maybe WlKeyboardListener,
    clTouchListener :: Maybe WlTouchListener,
    clOutputListener :: Maybe WlOutputListener,
    clgWmBaseListener :: Maybe XdgWmBaseListener,
    clgSurfaceListener :: Maybe XdgSurfaceListener,
    clgToplevelListener :: Maybe XdgToplevelListener,
    clgPopupListener :: Maybe XdgPopupListener,
    clActiveIfaces :: [IfacKey],
    clReqs :: [BS.ByteString],
    clFds :: [Fd]}

-- Initializer for ClState

initClState :: ClState
initClState = ClState {    clDisplayListener = Nothing,
    clRegistryListener = Nothing,
    clCallbackListener = Nothing,
    clShmListener = Nothing,
    clBufferListener = Nothing,
    clDataOfferListener = Nothing,
    clDataSourceListener = Nothing,
    clDataDeviceListener = Nothing,
    clShellSurfaceListener = Nothing,
    clSurfaceListener = Nothing,
    clSeatListener = Nothing,
    clPointerListener = Nothing,
    clKeyboardListener = Nothing,
    clTouchListener = Nothing,
    clOutputListener = Nothing,
    clgWmBaseListener = Nothing,
    clgSurfaceListener = Nothing,
    clgToplevelListener = Nothing,
    clgPopupListener = Nothing,
    clActiveIfaces = initActiveIfaces,
    clReqs = [],
    clFds = []}

-- Generate the setter functions for the listeners

setDisplayListener :: WlDisplayListener -> ClMonad ()
setDisplayListener lisnr = do
    st <- ST.get
    ST.put st { clDisplayListener = Just lisnr }

setRegistryListener :: WlRegistryListener -> ClMonad ()
setRegistryListener lisnr = do
    st <- ST.get
    ST.put st { clRegistryListener = Just lisnr }

setCallbackListener :: WlCallbackListener -> ClMonad ()
setCallbackListener lisnr = do
    st <- ST.get
    ST.put st { clCallbackListener = Just lisnr }

setShmListener :: WlShmListener -> ClMonad ()
setShmListener lisnr = do
    st <- ST.get
    ST.put st { clShmListener = Just lisnr }

setBufferListener :: WlBufferListener -> ClMonad ()
setBufferListener lisnr = do
    st <- ST.get
    ST.put st { clBufferListener = Just lisnr }

setDataOfferListener :: WlDataOfferListener -> ClMonad ()
setDataOfferListener lisnr = do
    st <- ST.get
    ST.put st { clDataOfferListener = Just lisnr }

setDataSourceListener :: WlDataSourceListener -> ClMonad ()
setDataSourceListener lisnr = do
    st <- ST.get
    ST.put st { clDataSourceListener = Just lisnr }

setDataDeviceListener :: WlDataDeviceListener -> ClMonad ()
setDataDeviceListener lisnr = do
    st <- ST.get
    ST.put st { clDataDeviceListener = Just lisnr }

setShellSurfaceListener :: WlShellSurfaceListener -> ClMonad ()
setShellSurfaceListener lisnr = do
    st <- ST.get
    ST.put st { clShellSurfaceListener = Just lisnr }

setSurfaceListener :: WlSurfaceListener -> ClMonad ()
setSurfaceListener lisnr = do
    st <- ST.get
    ST.put st { clSurfaceListener = Just lisnr }

setSeatListener :: WlSeatListener -> ClMonad ()
setSeatListener lisnr = do
    st <- ST.get
    ST.put st { clSeatListener = Just lisnr }

setPointerListener :: WlPointerListener -> ClMonad ()
setPointerListener lisnr = do
    st <- ST.get
    ST.put st { clPointerListener = Just lisnr }

setKeyboardListener :: WlKeyboardListener -> ClMonad ()
setKeyboardListener lisnr = do
    st <- ST.get
    ST.put st { clKeyboardListener = Just lisnr }

setTouchListener :: WlTouchListener -> ClMonad ()
setTouchListener lisnr = do
    st <- ST.get
    ST.put st { clTouchListener = Just lisnr }

setOutputListener :: WlOutputListener -> ClMonad ()
setOutputListener lisnr = do
    st <- ST.get
    ST.put st { clOutputListener = Just lisnr }

setgWmBaseListener :: XdgWmBaseListener -> ClMonad ()
setgWmBaseListener lisnr = do
    st <- ST.get
    ST.put st { clgWmBaseListener = Just lisnr }

setgSurfaceListener :: XdgSurfaceListener -> ClMonad ()
setgSurfaceListener lisnr = do
    st <- ST.get
    ST.put st { clgSurfaceListener = Just lisnr }

setgToplevelListener :: XdgToplevelListener -> ClMonad ()
setgToplevelListener lisnr = do
    st <- ST.get
    ST.put st { clgToplevelListener = Just lisnr }

setgPopupListener :: XdgPopupListener -> ClMonad ()
setgPopupListener lisnr = do
    st <- ST.get
    ST.put st { clgPopupListener = Just lisnr }


-- Function dispatchEvent

dispatchEvent :: WInputMsg -> ClMonad ()
dispatchEvent msg = do
    st <- ST.get
    ST.liftIO $ print msg
    let wopc = winpOpc msg
        ifName = fromMaybe T.empty (lookup (winpObj msg) (clActiveIfaces st))
    ST.liftIO $ M.when (T.null ifName)
        $ unhandledEv ((T.pack . show) (winpObj msg)) wopc
    case ifName of
        "wl_display" -> do
          let listener = clDisplayListener st
          M.when (isNothing listener) $ ST.liftIO (unhandledEv ((T.pack . show) (winpObj msg)) wopc)
          M.when (isJust listener) $ do
            case winpOpc msg of
              0 -> pwlDisplayError (wlDisplayError (fromJust listener)) msg
              1 -> pwlDisplayDeleteId (wlDisplayDeleteId (fromJust listener)) msg
              _ -> error (T.unpack ifName <> "Unknown op-code:" <> show wopc)
        "wl_registry" -> do
          let listener = clRegistryListener st
          M.when (isNothing listener) $ ST.liftIO (unhandledEv ((T.pack . show) (winpObj msg)) wopc)
          M.when (isJust listener) $ do
            case winpOpc msg of
              0 -> pwlRegistryGlobal (wlRegistryGlobal (fromJust listener)) msg
              1 -> pwlRegistryGlobalRemove (wlRegistryGlobalRemove (fromJust listener)) msg
              _ -> error (T.unpack ifName <> "Unknown op-code:" <> show wopc)
        "wl_callback" -> do
          let listener = clCallbackListener st
          M.when (isNothing listener) $ ST.liftIO (unhandledEv ((T.pack . show) (winpObj msg)) wopc)
          M.when (isJust listener) $ do
            case winpOpc msg of
              0 -> pwlCallbackDone (wlCallbackDone (fromJust listener)) msg
              _ -> error (T.unpack ifName <> "Unknown op-code:" <> show wopc)
        "wl_shm" -> do
          let listener = clShmListener st
          M.when (isNothing listener) $ ST.liftIO (unhandledEv ((T.pack . show) (winpObj msg)) wopc)
          M.when (isJust listener) $ do
            case winpOpc msg of
              0 -> pwlShmFormat (wlShmFormat (fromJust listener)) msg
              _ -> error (T.unpack ifName <> "Unknown op-code:" <> show wopc)
        "wl_buffer" -> do
          let listener = clBufferListener st
          M.when (isNothing listener) $ ST.liftIO (unhandledEv ((T.pack . show) (winpObj msg)) wopc)
          M.when (isJust listener) $ do
            case winpOpc msg of
              0 -> pwlBufferRelease (wlBufferRelease (fromJust listener)) msg
              _ -> error (T.unpack ifName <> "Unknown op-code:" <> show wopc)
        "wl_data_offer" -> do
          let listener = clDataOfferListener st
          M.when (isNothing listener) $ ST.liftIO (unhandledEv ((T.pack . show) (winpObj msg)) wopc)
          M.when (isJust listener) $ do
            case winpOpc msg of
              0 -> pwlDataOfferOffer (wlDataOfferOffer (fromJust listener)) msg
              1 -> pwlDataOfferSourceActions (wlDataOfferSourceActions (fromJust listener)) msg
              2 -> pwlDataOfferAction (wlDataOfferAction (fromJust listener)) msg
              _ -> error (T.unpack ifName <> "Unknown op-code:" <> show wopc)
        "wl_data_source" -> do
          let listener = clDataSourceListener st
          M.when (isNothing listener) $ ST.liftIO (unhandledEv ((T.pack . show) (winpObj msg)) wopc)
          M.when (isJust listener) $ do
            case winpOpc msg of
              0 -> pwlDataSourceTarget (wlDataSourceTarget (fromJust listener)) msg
              1 -> pwlDataSourceSend (wlDataSourceSend (fromJust listener)) msg
              2 -> pwlDataSourceCancelled (wlDataSourceCancelled (fromJust listener)) msg
              3 -> pwlDataSourceDndDropPerformed (wlDataSourceDndDropPerformed (fromJust listener)) msg
              4 -> pwlDataSourceDndFinished (wlDataSourceDndFinished (fromJust listener)) msg
              5 -> pwlDataSourceAction (wlDataSourceAction (fromJust listener)) msg
              _ -> error (T.unpack ifName <> "Unknown op-code:" <> show wopc)
        "wl_data_device" -> do
          let listener = clDataDeviceListener st
          M.when (isNothing listener) $ ST.liftIO (unhandledEv ((T.pack . show) (winpObj msg)) wopc)
          M.when (isJust listener) $ do
            case winpOpc msg of
              0 -> pwlDataDeviceDataOffer (wlDataDeviceDataOffer (fromJust listener)) msg
              1 -> pwlDataDeviceEnter (wlDataDeviceEnter (fromJust listener)) msg
              2 -> pwlDataDeviceLeave (wlDataDeviceLeave (fromJust listener)) msg
              3 -> pwlDataDeviceMotion (wlDataDeviceMotion (fromJust listener)) msg
              4 -> pwlDataDeviceDrop (wlDataDeviceDrop (fromJust listener)) msg
              5 -> pwlDataDeviceSelection (wlDataDeviceSelection (fromJust listener)) msg
              _ -> error (T.unpack ifName <> "Unknown op-code:" <> show wopc)
        "wl_shell_surface" -> do
          let listener = clShellSurfaceListener st
          M.when (isNothing listener) $ ST.liftIO (unhandledEv ((T.pack . show) (winpObj msg)) wopc)
          M.when (isJust listener) $ do
            case winpOpc msg of
              0 -> pwlShellSurfacePing (wlShellSurfacePing (fromJust listener)) msg
              1 -> pwlShellSurfaceConfigure (wlShellSurfaceConfigure (fromJust listener)) msg
              2 -> pwlShellSurfacePopupDone (wlShellSurfacePopupDone (fromJust listener)) msg
              _ -> error (T.unpack ifName <> "Unknown op-code:" <> show wopc)
        "wl_surface" -> do
          let listener = clSurfaceListener st
          M.when (isNothing listener) $ ST.liftIO (unhandledEv ((T.pack . show) (winpObj msg)) wopc)
          M.when (isJust listener) $ do
            case winpOpc msg of
              0 -> pwlSurfaceEnter (wlSurfaceEnter (fromJust listener)) msg
              1 -> pwlSurfaceLeave (wlSurfaceLeave (fromJust listener)) msg
              _ -> error (T.unpack ifName <> "Unknown op-code:" <> show wopc)
        "wl_seat" -> do
          let listener = clSeatListener st
          M.when (isNothing listener) $ ST.liftIO (unhandledEv ((T.pack . show) (winpObj msg)) wopc)
          M.when (isJust listener) $ do
            case winpOpc msg of
              0 -> pwlSeatCapabilities (wlSeatCapabilities (fromJust listener)) msg
              1 -> pwlSeatName (wlSeatName (fromJust listener)) msg
              _ -> error (T.unpack ifName <> "Unknown op-code:" <> show wopc)
        "wl_pointer" -> do
          let listener = clPointerListener st
          M.when (isNothing listener) $ ST.liftIO (unhandledEv ((T.pack . show) (winpObj msg)) wopc)
          M.when (isJust listener) $ do
            case winpOpc msg of
              0 -> pwlPointerEnter (wlPointerEnter (fromJust listener)) msg
              1 -> pwlPointerLeave (wlPointerLeave (fromJust listener)) msg
              2 -> pwlPointerMotion (wlPointerMotion (fromJust listener)) msg
              3 -> pwlPointerButton (wlPointerButton (fromJust listener)) msg
              4 -> pwlPointerAxis (wlPointerAxis (fromJust listener)) msg
              5 -> pwlPointerFrame (wlPointerFrame (fromJust listener)) msg
              6 -> pwlPointerAxisSource (wlPointerAxisSource (fromJust listener)) msg
              7 -> pwlPointerAxisStop (wlPointerAxisStop (fromJust listener)) msg
              8 -> pwlPointerAxisDiscrete (wlPointerAxisDiscrete (fromJust listener)) msg
              _ -> error (T.unpack ifName <> "Unknown op-code:" <> show wopc)
        "wl_keyboard" -> do
          let listener = clKeyboardListener st
          M.when (isNothing listener) $ ST.liftIO (unhandledEv ((T.pack . show) (winpObj msg)) wopc)
          M.when (isJust listener) $ do
            case winpOpc msg of
              0 -> pwlKeyboardKeymap (wlKeyboardKeymap (fromJust listener)) msg
              1 -> pwlKeyboardEnter (wlKeyboardEnter (fromJust listener)) msg
              2 -> pwlKeyboardLeave (wlKeyboardLeave (fromJust listener)) msg
              3 -> pwlKeyboardKey (wlKeyboardKey (fromJust listener)) msg
              4 -> pwlKeyboardModifiers (wlKeyboardModifiers (fromJust listener)) msg
              5 -> pwlKeyboardRepeatInfo (wlKeyboardRepeatInfo (fromJust listener)) msg
              _ -> error (T.unpack ifName <> "Unknown op-code:" <> show wopc)
        "wl_touch" -> do
          let listener = clTouchListener st
          M.when (isNothing listener) $ ST.liftIO (unhandledEv ((T.pack . show) (winpObj msg)) wopc)
          M.when (isJust listener) $ do
            case winpOpc msg of
              0 -> pwlTouchDown (wlTouchDown (fromJust listener)) msg
              1 -> pwlTouchUp (wlTouchUp (fromJust listener)) msg
              2 -> pwlTouchMotion (wlTouchMotion (fromJust listener)) msg
              3 -> pwlTouchFrame (wlTouchFrame (fromJust listener)) msg
              4 -> pwlTouchCancel (wlTouchCancel (fromJust listener)) msg
              5 -> pwlTouchShape (wlTouchShape (fromJust listener)) msg
              6 -> pwlTouchOrientation (wlTouchOrientation (fromJust listener)) msg
              _ -> error (T.unpack ifName <> "Unknown op-code:" <> show wopc)
        "wl_output" -> do
          let listener = clOutputListener st
          M.when (isNothing listener) $ ST.liftIO (unhandledEv ((T.pack . show) (winpObj msg)) wopc)
          M.when (isJust listener) $ do
            case winpOpc msg of
              0 -> pwlOutputGeometry (wlOutputGeometry (fromJust listener)) msg
              1 -> pwlOutputMode (wlOutputMode (fromJust listener)) msg
              2 -> pwlOutputDone (wlOutputDone (fromJust listener)) msg
              3 -> pwlOutputScale (wlOutputScale (fromJust listener)) msg
              _ -> error (T.unpack ifName <> "Unknown op-code:" <> show wopc)
        "xdg_wm_base" -> do
          let listener = clgWmBaseListener st
          M.when (isNothing listener) $ ST.liftIO (unhandledEv ((T.pack . show) (winpObj msg)) wopc)
          M.when (isJust listener) $ do
            case winpOpc msg of
              0 -> pxdgWmBasePing (xdgWmBasePing (fromJust listener)) msg
              _ -> error (T.unpack ifName <> "Unknown op-code:" <> show wopc)
        "xdg_surface" -> do
          let listener = clgSurfaceListener st
          M.when (isNothing listener) $ ST.liftIO (unhandledEv ((T.pack . show) (winpObj msg)) wopc)
          M.when (isJust listener) $ do
            case winpOpc msg of
              0 -> pxdgSurfaceConfigure (xdgSurfaceConfigure (fromJust listener)) msg
              _ -> error (T.unpack ifName <> "Unknown op-code:" <> show wopc)
        "xdg_toplevel" -> do
          let listener = clgToplevelListener st
          M.when (isNothing listener) $ ST.liftIO (unhandledEv ((T.pack . show) (winpObj msg)) wopc)
          M.when (isJust listener) $ do
            case winpOpc msg of
              0 -> pxdgToplevelConfigure (xdgToplevelConfigure (fromJust listener)) msg
              1 -> pxdgToplevelClose (xdgToplevelClose (fromJust listener)) msg
              _ -> error (T.unpack ifName <> "Unknown op-code:" <> show wopc)
        "xdg_popup" -> do
          let listener = clgPopupListener st
          M.when (isNothing listener) $ ST.liftIO (unhandledEv ((T.pack . show) (winpObj msg)) wopc)
          M.when (isJust listener) $ do
            case winpOpc msg of
              0 -> pxdgPopupConfigure (xdgPopupConfigure (fromJust listener)) msg
              1 -> pxdgPopupPopupDone (xdgPopupPopupDone (fromJust listener)) msg
              2 -> pxdgPopupRepositioned (xdgPopupRepositioned (fromJust listener)) msg
              _ -> error (T.unpack ifName <> "Unknown op-code:" <> show wopc)
        _ ->  error ("dispatchEvent: No case for interface object " <> T.unpack ifName)


-- Create a new Id
createNewId :: Text -> ClMonad WNewId
createNewId txt = do
    st <- ST.get
    let usedObj = map fst $ clActiveIfaces st
        newObj = head $ filter(`notElem` usedObj) [1..]
        newActives = (newObj, txt) : clActiveIfaces st
    ST.liftIO $ putStrLn ("CREATE new WOBJ for " <> T.unpack txt <> ": " <> show newObj)
    ST.put $ st { clActiveIfaces = newActives }
    pure $ fromIntegral newObj

-- Add a request to the queue
addRequest :: BS.ByteString -> ClMonad ()
addRequest bs = do
    st <- ST.get
    ST.liftIO $ putStrLn $ "Add Request: " <> toHexString bs
    ST.put $ st {clReqs = bs : clReqs st}

-- Add a FileDescriptor to the state. Goes into the ancillary data of the socket
addFd :: Fd -> ClMonad ()
addFd fd = do
    st <- ST.get
    ST.liftIO $ putStrLn $ "Add Fd: " <> show fd
    ST.put $ st {clFds = fd : clFds st}

-- Get an WObj from the interface text name
getObjectId :: Text -> ClMonad WObj
getObjectId txt = do
    st <- ST.get
    pure $ fst $ fromMaybe (0, T.empty) (find ((==) txt . snd ) (clActiveIfaces st))

