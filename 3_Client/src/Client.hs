

module Client where

import Wire
-- import Mayland



import Types
import WaylandSocket
import qualified Network.Socket             as Socket

import qualified Data.ByteString            as BS

import Control.Monad.State.Strict

import Data.Text (Text)
import ClientSupport (createNewId, addRequest)
-- import qualified Data.Text as T

{-
-- add C-Code to translate here !!


-}


bindToInterface :: WInt -> WString -> ClMonad ()
bindToInterface name (WString txt) = do
    st <- get
    let newObj = 1 + clMaxUsedIface st
        newActives  = (newObj, txt) : clActiveIfaces st
        reqs = wlRegistryBind cWlDisplay st (fromIntegral name) (fromIntegral newObj) : clReqs st
    put $ st { clMaxUsedIface = newObj, clActiveIfaces = newActives, clReqs = reqs}


displayGetRegistry :: ClMonad ()
displayGetRegistry = do
    st <- get
    newId <- createNewId cWlRegistry
    let req = wlDisplayGetRegistry cWlDisplay st newId
    addRequest req

{-
displayGetRegistry :: ClMonad ()
displayGetRegistry = do
    st <- get
    let newObj = 1 + clMaxUsedIface st
        newActives  = (newObj, cWlRegistry) : clActiveIfaces st
        reqs = wlDisplayGetRegistry cWlDisplay st (fromIntegral newObj) : clReqs st
    put $ st { clMaxUsedIface = newObj, clActiveIfaces = newActives, clReqs = reqs}
-}

displaySync :: ClMonad ()
displaySync = do
    st <- get
    let newObj = 1 + clMaxUsedIface st
        newActives  = (newObj, cWlCallback) : clActiveIfaces st
        reqs = wlDisplaySync cWlDisplay st (fromIntegral newObj) : clReqs st
    put st { clMaxUsedIface = newObj, clActiveIfaces = newActives, clReqs = reqs}

removeActiveIfac :: WObj -> ClMonad ()
removeActiveIfac obj  = do
    st <- get
    put $ st {clActiveIfaces = filter ((/=) obj . fst) (clActiveIfaces st) }

collectRequests :: ClMonad BS.ByteString
collectRequests = do mconcat. reverse . clReqs <$> get

sendRequests :: Socket.Socket -> ClMonad ()
sendRequests serverSock = do
    reqs <- collectRequests
    _ <- liftIO $ sendToWayland serverSock reqs []
    st <- get
    put st {clReqs = []}
