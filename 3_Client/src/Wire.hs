{-# LANGUAGE OverloadedStrings #-}

-- *** ATTENTION *** Generated Code *** DO NOT MODIFY

module Wire where
import Types
import WireSupport

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
-- Request Handling
-- ----------------------------------------------------------------------
--  Interface: WlDisplay - core global object

--  Req: asynchronous roundtrip opc:0
wlDisplaySync :: WObj -> WNewId -> BS.ByteString
wlDisplaySync wobj callback = runByteString $ do
    put wobj
    put $ WOpc 0
    putWord16host 12
    put callback

--  Req: get global registry object opc:1
wlDisplayGetRegistry :: WObj -> WNewId -> BS.ByteString
wlDisplayGetRegistry wobj registry = runByteString $ do
    put wobj
    put $ WOpc 1
    putWord16host 12
    put registry

--  Interface: WlRegistry - global registry object

--  Req: bind an object to the display opc:0
wlRegistryBind :: WObj -> WUint -> WNewId -> BS.ByteString
wlRegistryBind wobj name xid = runByteString $ do
    put wobj
    put $ WOpc 0
    putWord16host 16
    put name
    put xid

--  Interface: WlCompositor - the compositor singleton

--  Req: create new surface opc:0
wlCompositorCreateSurface :: WObj -> WNewId -> BS.ByteString
wlCompositorCreateSurface wobj xid = runByteString $ do
    put wobj
    put $ WOpc 0
    putWord16host 12
    put xid

--  Req: create new region opc:1
wlCompositorCreateRegion :: WObj -> WNewId -> BS.ByteString
wlCompositorCreateRegion wobj xid = runByteString $ do
    put wobj
    put $ WOpc 1
    putWord16host 12
    put xid

--  Interface: WlShmPool - a shared memory pool

--  Req: create a buffer from the pool opc:0
wlShmPoolCreateBuffer :: WObj -> WNewId -> WInt -> WInt -> WInt -> WInt -> WUint -> BS.ByteString
wlShmPoolCreateBuffer wobj xid offset width height stride format = runByteString $ do
    put wobj
    put $ WOpc 0
    putWord16host 32
    put xid
    put offset
    put width
    put height
    put stride
    put format

--  Req: destroy the pool opc:1
wlShmPoolDestroy :: WObj -> BS.ByteString
wlShmPoolDestroy wobj  = runByteString $ do
    put wobj
    put $ WOpc 1
    putWord16host 8

--  Req: change the size of the pool mapping opc:2
wlShmPoolResize :: WObj -> WInt -> BS.ByteString
wlShmPoolResize wobj size = runByteString $ do
    put wobj
    put $ WOpc 2
    putWord16host 12
    put size

--  Interface: WlShm - shared memory support

--  Req: create a shm pool opc:0
wlShmCreatePool :: WObj -> WNewId -> WFd -> WInt -> BS.ByteString
wlShmCreatePool wobj xid fd size = runByteString $ do
    put wobj
    put $ WOpc 0
    putWord16host 16
    put xid
    put fd
    put size

--  Interface: WlBuffer - content for a wl_surface

--  Req: destroy a buffer opc:0
wlBufferDestroy :: WObj -> BS.ByteString
wlBufferDestroy wobj  = runByteString $ do
    put wobj
    put $ WOpc 0
    putWord16host 8

--  Interface: WlDataOffer - offer to transfer data

--  Req: accept one of the offered mime types opc:0
wlDataOfferAccept :: WObj -> WUint -> WString -> BS.ByteString
wlDataOfferAccept wobj serial mimeType = runByteString $ do
    put wobj
    put $ WOpc 0
    putWord16host  $ fromIntegral len
    put serial
    put mimeType
  where len = 12 + sum (calcWStringLength <$> [mimeType])  + sum (calcWArrayLength  <$> []) 

--  Req: request that the data is transferred opc:1
wlDataOfferReceive :: WObj -> WString -> WFd -> BS.ByteString
wlDataOfferReceive wobj mimeType fd = runByteString $ do
    put wobj
    put $ WOpc 1
    putWord16host  $ fromIntegral len
    put mimeType
    put fd
  where len = 8 + sum (calcWStringLength <$> [mimeType])  + sum (calcWArrayLength  <$> []) 

--  Req: destroy data offer opc:2
wlDataOfferDestroy :: WObj -> BS.ByteString
wlDataOfferDestroy wobj  = runByteString $ do
    put wobj
    put $ WOpc 2
    putWord16host 8

--  Req: the offer will no longer be used opc:3
wlDataOfferFinish :: WObj -> BS.ByteString
wlDataOfferFinish wobj  = runByteString $ do
    put wobj
    put $ WOpc 3
    putWord16host 8

--  Req: set the available/preferred drag-and-drop actions opc:4
wlDataOfferSetActions :: WObj -> WUint -> WUint -> BS.ByteString
wlDataOfferSetActions wobj dndActions preferredAction = runByteString $ do
    put wobj
    put $ WOpc 4
    putWord16host 16
    put dndActions
    put preferredAction

--  Interface: WlDataSource - offer to transfer data

--  Req: add an offered mime type opc:0
wlDataSourceOffer :: WObj -> WString -> BS.ByteString
wlDataSourceOffer wobj mimeType = runByteString $ do
    put wobj
    put $ WOpc 0
    putWord16host  $ fromIntegral len
    put mimeType
  where len = 8 + sum (calcWStringLength <$> [mimeType])  + sum (calcWArrayLength  <$> []) 

--  Req: destroy the data source opc:1
wlDataSourceDestroy :: WObj -> BS.ByteString
wlDataSourceDestroy wobj  = runByteString $ do
    put wobj
    put $ WOpc 1
    putWord16host 8

--  Req: set the available drag-and-drop actions opc:2
wlDataSourceSetActions :: WObj -> WUint -> BS.ByteString
wlDataSourceSetActions wobj dndActions = runByteString $ do
    put wobj
    put $ WOpc 2
    putWord16host 12
    put dndActions

--  Interface: WlDataDevice - data transfer device

--  Req: start drag-and-drop operation opc:0
wlDataDeviceStartDrag :: WObj -> WObj -> WObj -> WObj -> WUint -> BS.ByteString
wlDataDeviceStartDrag wobj source origin icon serial = runByteString $ do
    put wobj
    put $ WOpc 0
    putWord16host 24
    put source
    put origin
    put icon
    put serial

--  Req: copy data to the selection opc:1
wlDataDeviceSetSelection :: WObj -> WObj -> WUint -> BS.ByteString
wlDataDeviceSetSelection wobj source serial = runByteString $ do
    put wobj
    put $ WOpc 1
    putWord16host 16
    put source
    put serial

--  Req: destroy data device opc:2
wlDataDeviceRelease :: WObj -> BS.ByteString
wlDataDeviceRelease wobj  = runByteString $ do
    put wobj
    put $ WOpc 2
    putWord16host 8

--  Interface: WlDataDeviceManager - data transfer interface

--  Req: create a new data source opc:0
wlDataDeviceManagerCreateDataSource :: WObj -> WNewId -> BS.ByteString
wlDataDeviceManagerCreateDataSource wobj xid = runByteString $ do
    put wobj
    put $ WOpc 0
    putWord16host 12
    put xid

--  Req: create a new data device opc:1
wlDataDeviceManagerGetDataDevice :: WObj -> WNewId -> WObj -> BS.ByteString
wlDataDeviceManagerGetDataDevice wobj xid seat = runByteString $ do
    put wobj
    put $ WOpc 1
    putWord16host 16
    put xid
    put seat

--  Interface: WlShell - create desktop-style surfaces

--  Req: create a shell surface from a surface opc:0
wlShellGetShellSurface :: WObj -> WNewId -> WObj -> BS.ByteString
wlShellGetShellSurface wobj xid surface = runByteString $ do
    put wobj
    put $ WOpc 0
    putWord16host 16
    put xid
    put surface

--  Interface: WlShellSurface - desktop-style metadata interface

--  Req: respond to a ping event opc:0
wlShellSurfacePong :: WObj -> WUint -> BS.ByteString
wlShellSurfacePong wobj serial = runByteString $ do
    put wobj
    put $ WOpc 0
    putWord16host 12
    put serial

--  Req: start an interactive move opc:1
wlShellSurfaceMove :: WObj -> WObj -> WUint -> BS.ByteString
wlShellSurfaceMove wobj seat serial = runByteString $ do
    put wobj
    put $ WOpc 1
    putWord16host 16
    put seat
    put serial

--  Req: start an interactive resize opc:2
wlShellSurfaceResize :: WObj -> WObj -> WUint -> WUint -> BS.ByteString
wlShellSurfaceResize wobj seat serial edges = runByteString $ do
    put wobj
    put $ WOpc 2
    putWord16host 20
    put seat
    put serial
    put edges

--  Req: make the surface a toplevel surface opc:3
wlShellSurfaceSetToplevel :: WObj -> BS.ByteString
wlShellSurfaceSetToplevel wobj  = runByteString $ do
    put wobj
    put $ WOpc 3
    putWord16host 8

--  Req: make the surface a transient surface opc:4
wlShellSurfaceSetTransient :: WObj -> WObj -> WInt -> WInt -> WUint -> BS.ByteString
wlShellSurfaceSetTransient wobj parent x y flags = runByteString $ do
    put wobj
    put $ WOpc 4
    putWord16host 24
    put parent
    put x
    put y
    put flags

--  Req: make the surface a fullscreen surface opc:5
wlShellSurfaceSetFullscreen :: WObj -> WUint -> WUint -> WObj -> BS.ByteString
wlShellSurfaceSetFullscreen wobj method framerate output = runByteString $ do
    put wobj
    put $ WOpc 5
    putWord16host 20
    put method
    put framerate
    put output

--  Req: make the surface a popup surface opc:6
wlShellSurfaceSetPopup :: WObj -> WObj -> WUint -> WObj -> WInt -> WInt -> WUint -> BS.ByteString
wlShellSurfaceSetPopup wobj seat serial parent x y flags = runByteString $ do
    put wobj
    put $ WOpc 6
    putWord16host 32
    put seat
    put serial
    put parent
    put x
    put y
    put flags

--  Req: make the surface a maximized surface opc:7
wlShellSurfaceSetMaximized :: WObj -> WObj -> BS.ByteString
wlShellSurfaceSetMaximized wobj output = runByteString $ do
    put wobj
    put $ WOpc 7
    putWord16host 12
    put output

--  Req: set surface title opc:8
wlShellSurfaceSetTitle :: WObj -> WString -> BS.ByteString
wlShellSurfaceSetTitle wobj title = runByteString $ do
    put wobj
    put $ WOpc 8
    putWord16host  $ fromIntegral len
    put title
  where len = 8 + sum (calcWStringLength <$> [title])  + sum (calcWArrayLength  <$> []) 

--  Req: set surface class opc:9
wlShellSurfaceSetClass :: WObj -> WString -> BS.ByteString
wlShellSurfaceSetClass wobj xclass = runByteString $ do
    put wobj
    put $ WOpc 9
    putWord16host  $ fromIntegral len
    put xclass
  where len = 8 + sum (calcWStringLength <$> [xclass])  + sum (calcWArrayLength  <$> []) 

--  Interface: WlSurface - an onscreen surface

--  Req: delete surface opc:0
wlSurfaceDestroy :: WObj -> BS.ByteString
wlSurfaceDestroy wobj  = runByteString $ do
    put wobj
    put $ WOpc 0
    putWord16host 8

--  Req: set the surface contents opc:1
wlSurfaceAttach :: WObj -> WObj -> WInt -> WInt -> BS.ByteString
wlSurfaceAttach wobj buffer x y = runByteString $ do
    put wobj
    put $ WOpc 1
    putWord16host 20
    put buffer
    put x
    put y

--  Req: mark part of the surface damaged opc:2
wlSurfaceDamage :: WObj -> WInt -> WInt -> WInt -> WInt -> BS.ByteString
wlSurfaceDamage wobj x y width height = runByteString $ do
    put wobj
    put $ WOpc 2
    putWord16host 24
    put x
    put y
    put width
    put height

--  Req: request a frame throttling hint opc:3
wlSurfaceFrame :: WObj -> WNewId -> BS.ByteString
wlSurfaceFrame wobj callback = runByteString $ do
    put wobj
    put $ WOpc 3
    putWord16host 12
    put callback

--  Req: set opaque region opc:4
wlSurfaceSetOpaqueRegion :: WObj -> WObj -> BS.ByteString
wlSurfaceSetOpaqueRegion wobj region = runByteString $ do
    put wobj
    put $ WOpc 4
    putWord16host 12
    put region

--  Req: set input region opc:5
wlSurfaceSetInputRegion :: WObj -> WObj -> BS.ByteString
wlSurfaceSetInputRegion wobj region = runByteString $ do
    put wobj
    put $ WOpc 5
    putWord16host 12
    put region

--  Req: commit pending surface state opc:6
wlSurfaceCommit :: WObj -> BS.ByteString
wlSurfaceCommit wobj  = runByteString $ do
    put wobj
    put $ WOpc 6
    putWord16host 8

--  Req: sets the buffer transformation opc:7
wlSurfaceSetBufferTransform :: WObj -> WInt -> BS.ByteString
wlSurfaceSetBufferTransform wobj transform = runByteString $ do
    put wobj
    put $ WOpc 7
    putWord16host 12
    put transform

--  Req: sets the buffer scaling factor opc:8
wlSurfaceSetBufferScale :: WObj -> WInt -> BS.ByteString
wlSurfaceSetBufferScale wobj scale = runByteString $ do
    put wobj
    put $ WOpc 8
    putWord16host 12
    put scale

--  Req: mark part of the surface damaged using buffer coordinates opc:9
wlSurfaceDamageBuffer :: WObj -> WInt -> WInt -> WInt -> WInt -> BS.ByteString
wlSurfaceDamageBuffer wobj x y width height = runByteString $ do
    put wobj
    put $ WOpc 9
    putWord16host 24
    put x
    put y
    put width
    put height

--  Interface: WlSeat - group of input devices

--  Req: return pointer object opc:0
wlSeatGetPointer :: WObj -> WNewId -> BS.ByteString
wlSeatGetPointer wobj xid = runByteString $ do
    put wobj
    put $ WOpc 0
    putWord16host 12
    put xid

--  Req: return keyboard object opc:1
wlSeatGetKeyboard :: WObj -> WNewId -> BS.ByteString
wlSeatGetKeyboard wobj xid = runByteString $ do
    put wobj
    put $ WOpc 1
    putWord16host 12
    put xid

--  Req: return touch object opc:2
wlSeatGetTouch :: WObj -> WNewId -> BS.ByteString
wlSeatGetTouch wobj xid = runByteString $ do
    put wobj
    put $ WOpc 2
    putWord16host 12
    put xid

--  Req: release the seat object opc:3
wlSeatRelease :: WObj -> BS.ByteString
wlSeatRelease wobj  = runByteString $ do
    put wobj
    put $ WOpc 3
    putWord16host 8

--  Interface: WlPointer - pointer input device

--  Req: set the pointer surface opc:0
wlPointerSetCursor :: WObj -> WUint -> WObj -> WInt -> WInt -> BS.ByteString
wlPointerSetCursor wobj serial surface hotspotX hotspotY = runByteString $ do
    put wobj
    put $ WOpc 0
    putWord16host 24
    put serial
    put surface
    put hotspotX
    put hotspotY

--  Req: release the pointer object opc:1
wlPointerRelease :: WObj -> BS.ByteString
wlPointerRelease wobj  = runByteString $ do
    put wobj
    put $ WOpc 1
    putWord16host 8

--  Interface: WlKeyboard - keyboard input device

--  Req: release the keyboard object opc:0
wlKeyboardRelease :: WObj -> BS.ByteString
wlKeyboardRelease wobj  = runByteString $ do
    put wobj
    put $ WOpc 0
    putWord16host 8

--  Interface: WlTouch - touchscreen input device

--  Req: release the touch object opc:0
wlTouchRelease :: WObj -> BS.ByteString
wlTouchRelease wobj  = runByteString $ do
    put wobj
    put $ WOpc 0
    putWord16host 8

--  Interface: WlOutput - compositor output region

--  Req: release the output object opc:0
wlOutputRelease :: WObj -> BS.ByteString
wlOutputRelease wobj  = runByteString $ do
    put wobj
    put $ WOpc 0
    putWord16host 8

--  Interface: WlRegion - region interface

--  Req: destroy region opc:0
wlRegionDestroy :: WObj -> BS.ByteString
wlRegionDestroy wobj  = runByteString $ do
    put wobj
    put $ WOpc 0
    putWord16host 8

--  Req: add rectangle to region opc:1
wlRegionAdd :: WObj -> WInt -> WInt -> WInt -> WInt -> BS.ByteString
wlRegionAdd wobj x y width height = runByteString $ do
    put wobj
    put $ WOpc 1
    putWord16host 24
    put x
    put y
    put width
    put height

--  Req: subtract rectangle from region opc:2
wlRegionSubtract :: WObj -> WInt -> WInt -> WInt -> WInt -> BS.ByteString
wlRegionSubtract wobj x y width height = runByteString $ do
    put wobj
    put $ WOpc 2
    putWord16host 24
    put x
    put y
    put width
    put height

--  Interface: WlSubcompositor - sub-surface compositing

--  Req: unbind from the subcompositor interface opc:0
wlSubcompositorDestroy :: WObj -> BS.ByteString
wlSubcompositorDestroy wobj  = runByteString $ do
    put wobj
    put $ WOpc 0
    putWord16host 8

--  Req: give a surface the role sub-surface opc:1
wlSubcompositorGetSubsurface :: WObj -> WNewId -> WObj -> WObj -> BS.ByteString
wlSubcompositorGetSubsurface wobj xid surface parent = runByteString $ do
    put wobj
    put $ WOpc 1
    putWord16host 20
    put xid
    put surface
    put parent

--  Interface: WlSubsurface - sub-surface interface to a wl_surface

--  Req: remove sub-surface interface opc:0
wlSubsurfaceDestroy :: WObj -> BS.ByteString
wlSubsurfaceDestroy wobj  = runByteString $ do
    put wobj
    put $ WOpc 0
    putWord16host 8

--  Req: reposition the sub-surface opc:1
wlSubsurfaceSetPosition :: WObj -> WInt -> WInt -> BS.ByteString
wlSubsurfaceSetPosition wobj x y = runByteString $ do
    put wobj
    put $ WOpc 1
    putWord16host 16
    put x
    put y

--  Req: restack the sub-surface opc:2
wlSubsurfacePlaceAbove :: WObj -> WObj -> BS.ByteString
wlSubsurfacePlaceAbove wobj sibling = runByteString $ do
    put wobj
    put $ WOpc 2
    putWord16host 12
    put sibling

--  Req: restack the sub-surface opc:3
wlSubsurfacePlaceBelow :: WObj -> WObj -> BS.ByteString
wlSubsurfacePlaceBelow wobj sibling = runByteString $ do
    put wobj
    put $ WOpc 3
    putWord16host 12
    put sibling

--  Req: set sub-surface to synchronized mode opc:4
wlSubsurfaceSetSync :: WObj -> BS.ByteString
wlSubsurfaceSetSync wobj  = runByteString $ do
    put wobj
    put $ WOpc 4
    putWord16host 8

--  Req: set sub-surface to desynchronized mode opc:5
wlSubsurfaceSetDesync :: WObj -> BS.ByteString
wlSubsurfaceSetDesync wobj  = runByteString $ do
    put wobj
    put $ WOpc 5
    putWord16host 8

--  Interface: XdgWmBase - create desktop-style surfaces

--  Req: destroy xdg_wm_base opc:0
xdgWmBaseDestroy :: WObj -> BS.ByteString
xdgWmBaseDestroy wobj  = runByteString $ do
    put wobj
    put $ WOpc 0
    putWord16host 8

--  Req: create a positioner object opc:1
xdgWmBaseCreatePositioner :: WObj -> WNewId -> BS.ByteString
xdgWmBaseCreatePositioner wobj xid = runByteString $ do
    put wobj
    put $ WOpc 1
    putWord16host 12
    put xid

--  Req: create a shell surface from a surface opc:2
xdgWmBaseGetXdgSurface :: WObj -> WNewId -> WObj -> BS.ByteString
xdgWmBaseGetXdgSurface wobj xid surface = runByteString $ do
    put wobj
    put $ WOpc 2
    putWord16host 16
    put xid
    put surface

--  Req: respond to a ping event opc:3
xdgWmBasePong :: WObj -> WUint -> BS.ByteString
xdgWmBasePong wobj serial = runByteString $ do
    put wobj
    put $ WOpc 3
    putWord16host 12
    put serial

--  Interface: XdgPositioner - child surface positioner

--  Req: destroy the xdg_positioner object opc:0
xdgPositionerDestroy :: WObj -> BS.ByteString
xdgPositionerDestroy wobj  = runByteString $ do
    put wobj
    put $ WOpc 0
    putWord16host 8

--  Req: set the size of the to-be positioned rectangle opc:1
xdgPositionerSetSize :: WObj -> WInt -> WInt -> BS.ByteString
xdgPositionerSetSize wobj width height = runByteString $ do
    put wobj
    put $ WOpc 1
    putWord16host 16
    put width
    put height

--  Req: set the anchor rectangle within the parent surface opc:2
xdgPositionerSetAnchorRect :: WObj -> WInt -> WInt -> WInt -> WInt -> BS.ByteString
xdgPositionerSetAnchorRect wobj x y width height = runByteString $ do
    put wobj
    put $ WOpc 2
    putWord16host 24
    put x
    put y
    put width
    put height

--  Req: set anchor rectangle anchor opc:3
xdgPositionerSetAnchor :: WObj -> WUint -> BS.ByteString
xdgPositionerSetAnchor wobj anchor = runByteString $ do
    put wobj
    put $ WOpc 3
    putWord16host 12
    put anchor

--  Req: set child surface gravity opc:4
xdgPositionerSetGravity :: WObj -> WUint -> BS.ByteString
xdgPositionerSetGravity wobj gravity = runByteString $ do
    put wobj
    put $ WOpc 4
    putWord16host 12
    put gravity

--  Req: set the adjustment to be done when constrained opc:5
xdgPositionerSetConstraintAdjustment :: WObj -> WUint -> BS.ByteString
xdgPositionerSetConstraintAdjustment wobj constraintAdjustment = runByteString $ do
    put wobj
    put $ WOpc 5
    putWord16host 12
    put constraintAdjustment

--  Req: set surface position offset opc:6
xdgPositionerSetOffset :: WObj -> WInt -> WInt -> BS.ByteString
xdgPositionerSetOffset wobj x y = runByteString $ do
    put wobj
    put $ WOpc 6
    putWord16host 16
    put x
    put y

--  Req: continuously reconstrain the surface opc:7
xdgPositionerSetReactive :: WObj -> BS.ByteString
xdgPositionerSetReactive wobj  = runByteString $ do
    put wobj
    put $ WOpc 7
    putWord16host 8

--  Req:  opc:8
xdgPositionerSetParentSize :: WObj -> WInt -> WInt -> BS.ByteString
xdgPositionerSetParentSize wobj parentWidth parentHeight = runByteString $ do
    put wobj
    put $ WOpc 8
    putWord16host 16
    put parentWidth
    put parentHeight

--  Req: set parent configure this is a response to opc:9
xdgPositionerSetParentConfigure :: WObj -> WUint -> BS.ByteString
xdgPositionerSetParentConfigure wobj serial = runByteString $ do
    put wobj
    put $ WOpc 9
    putWord16host 12
    put serial

--  Interface: XdgSurface - desktop user interface surface base interface

--  Req: destroy the xdg_surface opc:0
xdgSurfaceDestroy :: WObj -> BS.ByteString
xdgSurfaceDestroy wobj  = runByteString $ do
    put wobj
    put $ WOpc 0
    putWord16host 8

--  Req: assign the xdg_toplevel surface role opc:1
xdgSurfaceGetToplevel :: WObj -> WNewId -> BS.ByteString
xdgSurfaceGetToplevel wobj xid = runByteString $ do
    put wobj
    put $ WOpc 1
    putWord16host 12
    put xid

--  Req: assign the xdg_popup surface role opc:2
xdgSurfaceGetPopup :: WObj -> WNewId -> WObj -> WObj -> BS.ByteString
xdgSurfaceGetPopup wobj xid parent positioner = runByteString $ do
    put wobj
    put $ WOpc 2
    putWord16host 20
    put xid
    put parent
    put positioner

--  Req: set the new window geometry opc:3
xdgSurfaceSetWindowGeometry :: WObj -> WInt -> WInt -> WInt -> WInt -> BS.ByteString
xdgSurfaceSetWindowGeometry wobj x y width height = runByteString $ do
    put wobj
    put $ WOpc 3
    putWord16host 24
    put x
    put y
    put width
    put height

--  Req: ack a configure event opc:4
xdgSurfaceAckConfigure :: WObj -> WUint -> BS.ByteString
xdgSurfaceAckConfigure wobj serial = runByteString $ do
    put wobj
    put $ WOpc 4
    putWord16host 12
    put serial

--  Interface: XdgToplevel - toplevel surface

--  Req: destroy the xdg_toplevel opc:0
xdgToplevelDestroy :: WObj -> BS.ByteString
xdgToplevelDestroy wobj  = runByteString $ do
    put wobj
    put $ WOpc 0
    putWord16host 8

--  Req: set the parent of this surface opc:1
xdgToplevelSetParent :: WObj -> WObj -> BS.ByteString
xdgToplevelSetParent wobj parent = runByteString $ do
    put wobj
    put $ WOpc 1
    putWord16host 12
    put parent

--  Req: set surface title opc:2
xdgToplevelSetTitle :: WObj -> WString -> BS.ByteString
xdgToplevelSetTitle wobj title = runByteString $ do
    put wobj
    put $ WOpc 2
    putWord16host  $ fromIntegral len
    put title
  where len = 8 + sum (calcWStringLength <$> [title])  + sum (calcWArrayLength  <$> []) 

--  Req: set application ID opc:3
xdgToplevelSetAppId :: WObj -> WString -> BS.ByteString
xdgToplevelSetAppId wobj appId = runByteString $ do
    put wobj
    put $ WOpc 3
    putWord16host  $ fromIntegral len
    put appId
  where len = 8 + sum (calcWStringLength <$> [appId])  + sum (calcWArrayLength  <$> []) 

--  Req: show the window menu opc:4
xdgToplevelShowWindowMenu :: WObj -> WObj -> WUint -> WInt -> WInt -> BS.ByteString
xdgToplevelShowWindowMenu wobj seat serial x y = runByteString $ do
    put wobj
    put $ WOpc 4
    putWord16host 24
    put seat
    put serial
    put x
    put y

--  Req: start an interactive move opc:5
xdgToplevelMove :: WObj -> WObj -> WUint -> BS.ByteString
xdgToplevelMove wobj seat serial = runByteString $ do
    put wobj
    put $ WOpc 5
    putWord16host 16
    put seat
    put serial

--  Req: start an interactive resize opc:6
xdgToplevelResize :: WObj -> WObj -> WUint -> WUint -> BS.ByteString
xdgToplevelResize wobj seat serial edges = runByteString $ do
    put wobj
    put $ WOpc 6
    putWord16host 20
    put seat
    put serial
    put edges

--  Req: set the maximum size opc:7
xdgToplevelSetMaxSize :: WObj -> WInt -> WInt -> BS.ByteString
xdgToplevelSetMaxSize wobj width height = runByteString $ do
    put wobj
    put $ WOpc 7
    putWord16host 16
    put width
    put height

--  Req: set the minimum size opc:8
xdgToplevelSetMinSize :: WObj -> WInt -> WInt -> BS.ByteString
xdgToplevelSetMinSize wobj width height = runByteString $ do
    put wobj
    put $ WOpc 8
    putWord16host 16
    put width
    put height

--  Req: maximize the window opc:9
xdgToplevelSetMaximized :: WObj -> BS.ByteString
xdgToplevelSetMaximized wobj  = runByteString $ do
    put wobj
    put $ WOpc 9
    putWord16host 8

--  Req: unmaximize the window opc:10
xdgToplevelUnsetMaximized :: WObj -> BS.ByteString
xdgToplevelUnsetMaximized wobj  = runByteString $ do
    put wobj
    put $ WOpc 10
    putWord16host 8

--  Req: set the window as fullscreen on an output opc:11
xdgToplevelSetFullscreen :: WObj -> WObj -> BS.ByteString
xdgToplevelSetFullscreen wobj output = runByteString $ do
    put wobj
    put $ WOpc 11
    putWord16host 12
    put output

--  Req: unset the window as fullscreen opc:12
xdgToplevelUnsetFullscreen :: WObj -> BS.ByteString
xdgToplevelUnsetFullscreen wobj  = runByteString $ do
    put wobj
    put $ WOpc 12
    putWord16host 8

--  Req: set the window as minimized opc:13
xdgToplevelSetMinimized :: WObj -> BS.ByteString
xdgToplevelSetMinimized wobj  = runByteString $ do
    put wobj
    put $ WOpc 13
    putWord16host 8

--  Interface: XdgPopup - short-lived, popup surfaces for menus

--  Req: remove xdg_popup interface opc:0
xdgPopupDestroy :: WObj -> BS.ByteString
xdgPopupDestroy wobj  = runByteString $ do
    put wobj
    put $ WOpc 0
    putWord16host 8

--  Req: make the popup take an explicit grab opc:1
xdgPopupGrab :: WObj -> WObj -> WUint -> BS.ByteString
xdgPopupGrab wobj seat serial = runByteString $ do
    put wobj
    put $ WOpc 1
    putWord16host 16
    put seat
    put serial

--  Req: recalculate the popup's location opc:2
xdgPopupReposition :: WObj -> WObj -> WUint -> BS.ByteString
xdgPopupReposition wobj positioner token = runByteString $ do
    put wobj
    put $ WOpc 2
    putWord16host 16
    put positioner
    put token

-- ----------------------------------------------------------------------
-- Event Handling
-- ----------------------------------------------------------------------
-- Event Handling Function Types
type TwlDisplayError = WObj -> WUint -> WString -> ClMonad ()
type TwlDisplayDeleteId = WUint -> ClMonad ()
type TwlRegistryGlobal = WUint -> WString -> WUint -> ClMonad ()
type TwlRegistryGlobalRemove = WUint -> ClMonad ()
type TwlCallbackDone = WUint -> ClMonad ()
type TwlShmFormat = WUint -> ClMonad ()
type TwlBufferRelease = ClMonad ()
type TwlDataOfferOffer = WString -> ClMonad ()
type TwlDataOfferSourceActions = WUint -> ClMonad ()
type TwlDataOfferAction = WUint -> ClMonad ()
type TwlDataSourceTarget = WString -> ClMonad ()
type TwlDataSourceSend = WString -> WFd -> ClMonad ()
type TwlDataSourceCancelled = ClMonad ()
type TwlDataSourceDndDropPerformed = ClMonad ()
type TwlDataSourceDndFinished = ClMonad ()
type TwlDataSourceAction = WUint -> ClMonad ()
type TwlDataDeviceDataOffer = WNewId -> ClMonad ()
type TwlDataDeviceEnter = WUint -> WObj -> WFixed -> WFixed -> WObj -> ClMonad ()
type TwlDataDeviceLeave = ClMonad ()
type TwlDataDeviceMotion = WUint -> WFixed -> WFixed -> ClMonad ()
type TwlDataDeviceDrop = ClMonad ()
type TwlDataDeviceSelection = WObj -> ClMonad ()
type TwlShellSurfacePing = WUint -> ClMonad ()
type TwlShellSurfaceConfigure = WUint -> WInt -> WInt -> ClMonad ()
type TwlShellSurfacePopupDone = ClMonad ()
type TwlSurfaceEnter = WObj -> ClMonad ()
type TwlSurfaceLeave = WObj -> ClMonad ()
type TwlSeatCapabilities = WUint -> ClMonad ()
type TwlSeatName = WString -> ClMonad ()
type TwlPointerEnter = WUint -> WObj -> WFixed -> WFixed -> ClMonad ()
type TwlPointerLeave = WUint -> WObj -> ClMonad ()
type TwlPointerMotion = WUint -> WFixed -> WFixed -> ClMonad ()
type TwlPointerButton = WUint -> WUint -> WUint -> WUint -> ClMonad ()
type TwlPointerAxis = WUint -> WUint -> WFixed -> ClMonad ()
type TwlPointerFrame = ClMonad ()
type TwlPointerAxisSource = WUint -> ClMonad ()
type TwlPointerAxisStop = WUint -> WUint -> ClMonad ()
type TwlPointerAxisDiscrete = WUint -> WInt -> ClMonad ()
type TwlKeyboardKeymap = WUint -> WFd -> WUint -> ClMonad ()
type TwlKeyboardEnter = WUint -> WObj -> WArray -> ClMonad ()
type TwlKeyboardLeave = WUint -> WObj -> ClMonad ()
type TwlKeyboardKey = WUint -> WUint -> WUint -> WUint -> ClMonad ()
type TwlKeyboardModifiers = WUint -> WUint -> WUint -> WUint -> WUint -> ClMonad ()
type TwlKeyboardRepeatInfo = WInt -> WInt -> ClMonad ()
type TwlTouchDown = WUint -> WUint -> WObj -> WInt -> WFixed -> WFixed -> ClMonad ()
type TwlTouchUp = WUint -> WUint -> WInt -> ClMonad ()
type TwlTouchMotion = WUint -> WInt -> WFixed -> WFixed -> ClMonad ()
type TwlTouchFrame = ClMonad ()
type TwlTouchCancel = ClMonad ()
type TwlTouchShape = WInt -> WFixed -> WFixed -> ClMonad ()
type TwlTouchOrientation = WInt -> WFixed -> ClMonad ()
type TwlOutputGeometry = WInt -> WInt -> WInt -> WInt -> WInt -> WString -> WString -> WInt -> ClMonad ()
type TwlOutputMode = WUint -> WInt -> WInt -> WInt -> ClMonad ()
type TwlOutputDone = ClMonad ()
type TwlOutputScale = WInt -> ClMonad ()
type TxdgWmBasePing = WUint -> ClMonad ()
type TxdgSurfaceConfigure = WUint -> ClMonad ()
type TxdgToplevelConfigure = WInt -> WInt -> WArray -> ClMonad ()
type TxdgToplevelClose = ClMonad ()
type TxdgPopupConfigure = WInt -> WInt -> WInt -> WInt -> ClMonad ()
type TxdgPopupPopupDone = ClMonad ()
type TxdgPopupRepositioned = WUint -> ClMonad ()

-- Proxy Functions for all Events

pwlDisplayError :: Maybe TwlDisplayError -> WInputMsg -> ClMonad ()
pwlDisplayError Nothing _ = pure ()
pwlDisplayError (Just f) msg = runGet g $ BL.fromStrict $ winpData msg
    where g = f <$> get <*> get <*> get

pwlDisplayDeleteId :: Maybe TwlDisplayDeleteId -> WInputMsg -> ClMonad ()
pwlDisplayDeleteId Nothing _ = pure ()
pwlDisplayDeleteId (Just f) msg = runGet g $ BL.fromStrict $ winpData msg
    where g = f <$> get

pwlRegistryGlobal :: Maybe TwlRegistryGlobal -> WInputMsg -> ClMonad ()
pwlRegistryGlobal Nothing _ = pure ()
pwlRegistryGlobal (Just f) msg = runGet g $ BL.fromStrict $ winpData msg
    where g = f <$> get <*> get <*> get

pwlRegistryGlobalRemove :: Maybe TwlRegistryGlobalRemove -> WInputMsg -> ClMonad ()
pwlRegistryGlobalRemove Nothing _ = pure ()
pwlRegistryGlobalRemove (Just f) msg = runGet g $ BL.fromStrict $ winpData msg
    where g = f <$> get

pwlCallbackDone :: Maybe TwlCallbackDone -> WInputMsg -> ClMonad ()
pwlCallbackDone Nothing _ = pure ()
pwlCallbackDone (Just f) msg = runGet g $ BL.fromStrict $ winpData msg
    where g = f <$> get

pwlShmFormat :: Maybe TwlShmFormat -> WInputMsg -> ClMonad ()
pwlShmFormat Nothing _ = pure ()
pwlShmFormat (Just f) msg = runGet g $ BL.fromStrict $ winpData msg
    where g = f <$> get

pwlBufferRelease :: Maybe TwlBufferRelease -> WInputMsg -> ClMonad ()
pwlBufferRelease Nothing _ = pure ()
pwlBufferRelease (Just f) _msg = f

pwlDataOfferOffer :: Maybe TwlDataOfferOffer -> WInputMsg -> ClMonad ()
pwlDataOfferOffer Nothing _ = pure ()
pwlDataOfferOffer (Just f) msg = runGet g $ BL.fromStrict $ winpData msg
    where g = f <$> get

pwlDataOfferSourceActions :: Maybe TwlDataOfferSourceActions -> WInputMsg -> ClMonad ()
pwlDataOfferSourceActions Nothing _ = pure ()
pwlDataOfferSourceActions (Just f) msg = runGet g $ BL.fromStrict $ winpData msg
    where g = f <$> get

pwlDataOfferAction :: Maybe TwlDataOfferAction -> WInputMsg -> ClMonad ()
pwlDataOfferAction Nothing _ = pure ()
pwlDataOfferAction (Just f) msg = runGet g $ BL.fromStrict $ winpData msg
    where g = f <$> get

pwlDataSourceTarget :: Maybe TwlDataSourceTarget -> WInputMsg -> ClMonad ()
pwlDataSourceTarget Nothing _ = pure ()
pwlDataSourceTarget (Just f) msg = runGet g $ BL.fromStrict $ winpData msg
    where g = f <$> get

pwlDataSourceSend :: Maybe TwlDataSourceSend -> WInputMsg -> ClMonad ()
pwlDataSourceSend Nothing _ = pure ()
pwlDataSourceSend (Just f) msg = runGet g $ BL.fromStrict $ winpData msg
    where g = f <$> get <*> get

pwlDataSourceCancelled :: Maybe TwlDataSourceCancelled -> WInputMsg -> ClMonad ()
pwlDataSourceCancelled Nothing _ = pure ()
pwlDataSourceCancelled (Just f) _msg = f

pwlDataSourceDndDropPerformed :: Maybe TwlDataSourceDndDropPerformed -> WInputMsg -> ClMonad ()
pwlDataSourceDndDropPerformed Nothing _ = pure ()
pwlDataSourceDndDropPerformed (Just f) _msg = f

pwlDataSourceDndFinished :: Maybe TwlDataSourceDndFinished -> WInputMsg -> ClMonad ()
pwlDataSourceDndFinished Nothing _ = pure ()
pwlDataSourceDndFinished (Just f) _msg = f

pwlDataSourceAction :: Maybe TwlDataSourceAction -> WInputMsg -> ClMonad ()
pwlDataSourceAction Nothing _ = pure ()
pwlDataSourceAction (Just f) msg = runGet g $ BL.fromStrict $ winpData msg
    where g = f <$> get

pwlDataDeviceDataOffer :: Maybe TwlDataDeviceDataOffer -> WInputMsg -> ClMonad ()
pwlDataDeviceDataOffer Nothing _ = pure ()
pwlDataDeviceDataOffer (Just f) msg = runGet g $ BL.fromStrict $ winpData msg
    where g = f <$> get

pwlDataDeviceEnter :: Maybe TwlDataDeviceEnter -> WInputMsg -> ClMonad ()
pwlDataDeviceEnter Nothing _ = pure ()
pwlDataDeviceEnter (Just f) msg = runGet g $ BL.fromStrict $ winpData msg
    where g = f <$> get <*> get <*> get <*> get <*> get

pwlDataDeviceLeave :: Maybe TwlDataDeviceLeave -> WInputMsg -> ClMonad ()
pwlDataDeviceLeave Nothing _ = pure ()
pwlDataDeviceLeave (Just f) _msg = f

pwlDataDeviceMotion :: Maybe TwlDataDeviceMotion -> WInputMsg -> ClMonad ()
pwlDataDeviceMotion Nothing _ = pure ()
pwlDataDeviceMotion (Just f) msg = runGet g $ BL.fromStrict $ winpData msg
    where g = f <$> get <*> get <*> get

pwlDataDeviceDrop :: Maybe TwlDataDeviceDrop -> WInputMsg -> ClMonad ()
pwlDataDeviceDrop Nothing _ = pure ()
pwlDataDeviceDrop (Just f) _msg = f

pwlDataDeviceSelection :: Maybe TwlDataDeviceSelection -> WInputMsg -> ClMonad ()
pwlDataDeviceSelection Nothing _ = pure ()
pwlDataDeviceSelection (Just f) msg = runGet g $ BL.fromStrict $ winpData msg
    where g = f <$> get

pwlShellSurfacePing :: Maybe TwlShellSurfacePing -> WInputMsg -> ClMonad ()
pwlShellSurfacePing Nothing _ = pure ()
pwlShellSurfacePing (Just f) msg = runGet g $ BL.fromStrict $ winpData msg
    where g = f <$> get

pwlShellSurfaceConfigure :: Maybe TwlShellSurfaceConfigure -> WInputMsg -> ClMonad ()
pwlShellSurfaceConfigure Nothing _ = pure ()
pwlShellSurfaceConfigure (Just f) msg = runGet g $ BL.fromStrict $ winpData msg
    where g = f <$> get <*> get <*> get

pwlShellSurfacePopupDone :: Maybe TwlShellSurfacePopupDone -> WInputMsg -> ClMonad ()
pwlShellSurfacePopupDone Nothing _ = pure ()
pwlShellSurfacePopupDone (Just f) _msg = f

pwlSurfaceEnter :: Maybe TwlSurfaceEnter -> WInputMsg -> ClMonad ()
pwlSurfaceEnter Nothing _ = pure ()
pwlSurfaceEnter (Just f) msg = runGet g $ BL.fromStrict $ winpData msg
    where g = f <$> get

pwlSurfaceLeave :: Maybe TwlSurfaceLeave -> WInputMsg -> ClMonad ()
pwlSurfaceLeave Nothing _ = pure ()
pwlSurfaceLeave (Just f) msg = runGet g $ BL.fromStrict $ winpData msg
    where g = f <$> get

pwlSeatCapabilities :: Maybe TwlSeatCapabilities -> WInputMsg -> ClMonad ()
pwlSeatCapabilities Nothing _ = pure ()
pwlSeatCapabilities (Just f) msg = runGet g $ BL.fromStrict $ winpData msg
    where g = f <$> get

pwlSeatName :: Maybe TwlSeatName -> WInputMsg -> ClMonad ()
pwlSeatName Nothing _ = pure ()
pwlSeatName (Just f) msg = runGet g $ BL.fromStrict $ winpData msg
    where g = f <$> get

pwlPointerEnter :: Maybe TwlPointerEnter -> WInputMsg -> ClMonad ()
pwlPointerEnter Nothing _ = pure ()
pwlPointerEnter (Just f) msg = runGet g $ BL.fromStrict $ winpData msg
    where g = f <$> get <*> get <*> get <*> get

pwlPointerLeave :: Maybe TwlPointerLeave -> WInputMsg -> ClMonad ()
pwlPointerLeave Nothing _ = pure ()
pwlPointerLeave (Just f) msg = runGet g $ BL.fromStrict $ winpData msg
    where g = f <$> get <*> get

pwlPointerMotion :: Maybe TwlPointerMotion -> WInputMsg -> ClMonad ()
pwlPointerMotion Nothing _ = pure ()
pwlPointerMotion (Just f) msg = runGet g $ BL.fromStrict $ winpData msg
    where g = f <$> get <*> get <*> get

pwlPointerButton :: Maybe TwlPointerButton -> WInputMsg -> ClMonad ()
pwlPointerButton Nothing _ = pure ()
pwlPointerButton (Just f) msg = runGet g $ BL.fromStrict $ winpData msg
    where g = f <$> get <*> get <*> get <*> get

pwlPointerAxis :: Maybe TwlPointerAxis -> WInputMsg -> ClMonad ()
pwlPointerAxis Nothing _ = pure ()
pwlPointerAxis (Just f) msg = runGet g $ BL.fromStrict $ winpData msg
    where g = f <$> get <*> get <*> get

pwlPointerFrame :: Maybe TwlPointerFrame -> WInputMsg -> ClMonad ()
pwlPointerFrame Nothing _ = pure ()
pwlPointerFrame (Just f) _msg = f

pwlPointerAxisSource :: Maybe TwlPointerAxisSource -> WInputMsg -> ClMonad ()
pwlPointerAxisSource Nothing _ = pure ()
pwlPointerAxisSource (Just f) msg = runGet g $ BL.fromStrict $ winpData msg
    where g = f <$> get

pwlPointerAxisStop :: Maybe TwlPointerAxisStop -> WInputMsg -> ClMonad ()
pwlPointerAxisStop Nothing _ = pure ()
pwlPointerAxisStop (Just f) msg = runGet g $ BL.fromStrict $ winpData msg
    where g = f <$> get <*> get

pwlPointerAxisDiscrete :: Maybe TwlPointerAxisDiscrete -> WInputMsg -> ClMonad ()
pwlPointerAxisDiscrete Nothing _ = pure ()
pwlPointerAxisDiscrete (Just f) msg = runGet g $ BL.fromStrict $ winpData msg
    where g = f <$> get <*> get

pwlKeyboardKeymap :: Maybe TwlKeyboardKeymap -> WInputMsg -> ClMonad ()
pwlKeyboardKeymap Nothing _ = pure ()
pwlKeyboardKeymap (Just f) msg = runGet g $ BL.fromStrict $ winpData msg
    where g = f <$> get <*> get <*> get

pwlKeyboardEnter :: Maybe TwlKeyboardEnter -> WInputMsg -> ClMonad ()
pwlKeyboardEnter Nothing _ = pure ()
pwlKeyboardEnter (Just f) msg = runGet g $ BL.fromStrict $ winpData msg
    where g = f <$> get <*> get <*> get

pwlKeyboardLeave :: Maybe TwlKeyboardLeave -> WInputMsg -> ClMonad ()
pwlKeyboardLeave Nothing _ = pure ()
pwlKeyboardLeave (Just f) msg = runGet g $ BL.fromStrict $ winpData msg
    where g = f <$> get <*> get

pwlKeyboardKey :: Maybe TwlKeyboardKey -> WInputMsg -> ClMonad ()
pwlKeyboardKey Nothing _ = pure ()
pwlKeyboardKey (Just f) msg = runGet g $ BL.fromStrict $ winpData msg
    where g = f <$> get <*> get <*> get <*> get

pwlKeyboardModifiers :: Maybe TwlKeyboardModifiers -> WInputMsg -> ClMonad ()
pwlKeyboardModifiers Nothing _ = pure ()
pwlKeyboardModifiers (Just f) msg = runGet g $ BL.fromStrict $ winpData msg
    where g = f <$> get <*> get <*> get <*> get <*> get

pwlKeyboardRepeatInfo :: Maybe TwlKeyboardRepeatInfo -> WInputMsg -> ClMonad ()
pwlKeyboardRepeatInfo Nothing _ = pure ()
pwlKeyboardRepeatInfo (Just f) msg = runGet g $ BL.fromStrict $ winpData msg
    where g = f <$> get <*> get

pwlTouchDown :: Maybe TwlTouchDown -> WInputMsg -> ClMonad ()
pwlTouchDown Nothing _ = pure ()
pwlTouchDown (Just f) msg = runGet g $ BL.fromStrict $ winpData msg
    where g = f <$> get <*> get <*> get <*> get <*> get <*> get

pwlTouchUp :: Maybe TwlTouchUp -> WInputMsg -> ClMonad ()
pwlTouchUp Nothing _ = pure ()
pwlTouchUp (Just f) msg = runGet g $ BL.fromStrict $ winpData msg
    where g = f <$> get <*> get <*> get

pwlTouchMotion :: Maybe TwlTouchMotion -> WInputMsg -> ClMonad ()
pwlTouchMotion Nothing _ = pure ()
pwlTouchMotion (Just f) msg = runGet g $ BL.fromStrict $ winpData msg
    where g = f <$> get <*> get <*> get <*> get

pwlTouchFrame :: Maybe TwlTouchFrame -> WInputMsg -> ClMonad ()
pwlTouchFrame Nothing _ = pure ()
pwlTouchFrame (Just f) _msg = f

pwlTouchCancel :: Maybe TwlTouchCancel -> WInputMsg -> ClMonad ()
pwlTouchCancel Nothing _ = pure ()
pwlTouchCancel (Just f) _msg = f

pwlTouchShape :: Maybe TwlTouchShape -> WInputMsg -> ClMonad ()
pwlTouchShape Nothing _ = pure ()
pwlTouchShape (Just f) msg = runGet g $ BL.fromStrict $ winpData msg
    where g = f <$> get <*> get <*> get

pwlTouchOrientation :: Maybe TwlTouchOrientation -> WInputMsg -> ClMonad ()
pwlTouchOrientation Nothing _ = pure ()
pwlTouchOrientation (Just f) msg = runGet g $ BL.fromStrict $ winpData msg
    where g = f <$> get <*> get

pwlOutputGeometry :: Maybe TwlOutputGeometry -> WInputMsg -> ClMonad ()
pwlOutputGeometry Nothing _ = pure ()
pwlOutputGeometry (Just f) msg = runGet g $ BL.fromStrict $ winpData msg
    where g = f <$> get <*> get <*> get <*> get <*> get <*> get <*> get <*> get

pwlOutputMode :: Maybe TwlOutputMode -> WInputMsg -> ClMonad ()
pwlOutputMode Nothing _ = pure ()
pwlOutputMode (Just f) msg = runGet g $ BL.fromStrict $ winpData msg
    where g = f <$> get <*> get <*> get <*> get

pwlOutputDone :: Maybe TwlOutputDone -> WInputMsg -> ClMonad ()
pwlOutputDone Nothing _ = pure ()
pwlOutputDone (Just f) _msg = f

pwlOutputScale :: Maybe TwlOutputScale -> WInputMsg -> ClMonad ()
pwlOutputScale Nothing _ = pure ()
pwlOutputScale (Just f) msg = runGet g $ BL.fromStrict $ winpData msg
    where g = f <$> get

pxdgWmBasePing :: Maybe TxdgWmBasePing -> WInputMsg -> ClMonad ()
pxdgWmBasePing Nothing _ = pure ()
pxdgWmBasePing (Just f) msg = runGet g $ BL.fromStrict $ winpData msg
    where g = f <$> get

pxdgSurfaceConfigure :: Maybe TxdgSurfaceConfigure -> WInputMsg -> ClMonad ()
pxdgSurfaceConfigure Nothing _ = pure ()
pxdgSurfaceConfigure (Just f) msg = runGet g $ BL.fromStrict $ winpData msg
    where g = f <$> get

pxdgToplevelConfigure :: Maybe TxdgToplevelConfigure -> WInputMsg -> ClMonad ()
pxdgToplevelConfigure Nothing _ = pure ()
pxdgToplevelConfigure (Just f) msg = runGet g $ BL.fromStrict $ winpData msg
    where g = f <$> get <*> get <*> get

pxdgToplevelClose :: Maybe TxdgToplevelClose -> WInputMsg -> ClMonad ()
pxdgToplevelClose Nothing _ = pure ()
pxdgToplevelClose (Just f) _msg = f

pxdgPopupConfigure :: Maybe TxdgPopupConfigure -> WInputMsg -> ClMonad ()
pxdgPopupConfigure Nothing _ = pure ()
pxdgPopupConfigure (Just f) msg = runGet g $ BL.fromStrict $ winpData msg
    where g = f <$> get <*> get <*> get <*> get

pxdgPopupPopupDone :: Maybe TxdgPopupPopupDone -> WInputMsg -> ClMonad ()
pxdgPopupPopupDone Nothing _ = pure ()
pxdgPopupPopupDone (Just f) _msg = f

pxdgPopupRepositioned :: Maybe TxdgPopupRepositioned -> WInputMsg -> ClMonad ()
pxdgPopupRepositioned Nothing _ = pure ()
pxdgPopupRepositioned (Just f) msg = runGet g $ BL.fromStrict $ winpData msg
    where g = f <$> get

-- Listeners for all Interfaces

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
    clReqs :: [BS.ByteString] }

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
    clReqs = [] }
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
    ST.liftIO $ putStrLn $ "dispatchEvent for " <> 
        show ifName <> " obj:" <> show (winpObj msg) <> " opc:" <> show wopc
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


