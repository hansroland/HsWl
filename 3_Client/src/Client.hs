

module Client where

import Wire
-- import Mayland



import Types
import WaylandSocket
import qualified Network.Socket             as Socket

import qualified Data.ByteString            as BS

import Control.Monad.State.Strict

import Data.Text (Text)
import ClientSupport
-- import qualified Data.Text as T

{-
-- add C-Code to translate here !!


-}


bindToInterface :: WInt -> WString -> ClMonad ()
bindToInterface name (WString txt) = do
    wobj <- getObjectId cWlDisplay
    newId <- createNewId txt
    addRequest $ wlRegistryBind  wobj (fromIntegral name) (fromIntegral newId)


--    let newObj = 1 + clMaxUsedIface st
--        newActives  = (newObj, txt) : clActiveIfaces st
--        reqs = wlRegistryBind cWlDisplay st (fromIntegral name) (fromIntegral newObj) : clReqs st
--    put $ st { clMaxUsedIface = newObj, clActiveIfaces = newActives, clReqs = reqs}


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
