{-# LANGUAGE OverloadedStrings #-}

module Client where

import ClientSupport
import Wire
import WireSupport

import Types
import WaylandSocket

import qualified Network.Socket             as Socket
import qualified Data.ByteString            as BS
import           Control.Monad.State.Strict
import qualified Data.Text as T


bindToInterface :: WInt -> WString-> WUint -> ClMonad ()
bindToInterface name interface version = do
    wobj <- getObjectId cWlRegistry
    newId <- createNewId $ unWString interface
    addRequest $ rsxRegistryBind  wobj (fromIntegral name) interface version (fromIntegral newId)



displayGetRegistry :: ClMonad ()
displayGetRegistry = do
    wobj <- getObjectId cWlDisplay
    newId <- createNewId cWlRegistry
    addRequest $  wlDisplayGetRegistry wobj newId

displaySync :: ClMonad ()
displaySync = do
    wobj <- getObjectId cWlDisplay
    newId <- createNewId cWlCallback
    addRequest $ wlDisplaySync wobj newId


compositorCreateSurface :: ClMonad WObj
compositorCreateSurface = do
    wobj <- getObjectId cWlCompositor
    newId <- createNewId cWlSurface
    addRequest $ wlCompositorCreateSurface wobj newId
    pure $ fromIntegral newId                       -- TODO fromIntegral

{-}
    state.xdg_surface = xdg_wm_base_get_xdg_surface(
            state.xdg_wm_base, state.wl_surface);
-}

xdgGetXdgSurface :: WObj -> ClMonad WObj
xdgGetXdgSurface surface = do
    wobj <- getObjectId cXdgWmBase
    newId <- createNewId cXdgSurface
    addRequest $ xdgWmBaseGetXdgSurface wobj newId surface
    pure $ fromIntegral newId                       -- TODO fromIntegral


xdgSendPongAnswer :: WUint -> ClMonad ()
xdgSendPongAnswer serial = do
    wobj <- getObjectId cXdgWmBase
    addRequest $ xdgWmBasePong wobj serial


surfaceAckConfigure :: WUint -> ClMonad ()
surfaceAckConfigure serial = do
    wobj <- getObjectId cXdgSurface
    addRequest $ xdgSurfaceAckConfigure wobj serial

surfaceAssignToplevel ::  ClMonad WObj
surfaceAssignToplevel = do
    wobj <- getObjectId cXdgSurface
    newId <- createNewId cXdgToplevel
    addRequest $ xdgSurfaceGetToplevel wobj newId
    pure $ fromIntegral newId                       -- TODO fromIntegral


surfaceCommit :: ClMonad ()
surfaceCommit = do
    wobj <- getObjectId cWlSurface
    addRequest $ wlSurfaceCommit wobj


toplevelSetTitle :: WString -> ClMonad ()
toplevelSetTitle title = do
    wobj <- getObjectId cXdgToplevel
    addRequest $ xdgToplevelSetTitle wobj title


removeActiveIfac :: WObj -> ClMonad ()
removeActiveIfac obj  = do
    st <- get
    -- put $ st {clActiveIfaces = filter ((/=) obj . fst) (clActiveIfaces st) }
    -- let actives = st {clActiveIfaces = filter ((/=) obj . fst) (clActiveIfaces st) }
    let newacts = filter oks (clActiveIfaces st)
    liftIO $ printActiveIfaces newacts
    put $ st {clActiveIfaces = newacts }
  where
    oks (o,_) = o /= obj

printActiveIfaces :: [IfacKey] -> IO ()
printActiveIfaces keys = do
    putStrLn ("ACTIVE Interfaces: " <> concatMap showIfac keys)
  where
    showIfac :: IfacKey -> String
    showIfac (obj, txt ) = "(" <> show obj <> ", "  <> T.unpack txt  <> ")"

collectRequests :: ClMonad BS.ByteString
collectRequests = do mconcat. reverse . clReqs <$> get

sendRequests :: Socket.Socket -> ClMonad ()
sendRequests serverSock = do
    reqs <- collectRequests
    _ <- liftIO $ sendToWayland serverSock reqs []
    st <- get
    put st {clReqs = []}
