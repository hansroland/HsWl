{-# LANGUAGE OverloadedStrings #-}

module Client where

-- import ClientSupport
import Protocol
import ProtocolSupport

import Types
import Socket
-- import Shm

import qualified Data.Binary                as BP
import qualified Data.Binary.Put            as BP


import qualified Network.Socket             as Socket
import qualified Data.ByteString            as BS
import           Control.Monad.State.Strict
import qualified Data.Text as T
import           System.Posix.Types
import           System.Posix.IO


-- Note the wlRegistryBind function in the xml file is wrong
rsxRegistryBind :: WUint -> WString -> WUint -> T.Text -> ClMonad WObj
rsxRegistryBind name interface version xid = do
   wobj <- getObjectId cWlRegistry
   xid' <- createNewId xid
   addRequest $ runByteString $ do
      BP.put wobj
      BP.put $ WOpc 0
      BP.putWord16host $ fromIntegral $ 20 + calcWStringLength interface
      BP.put name
      BP.put interface
      BP.put version
      BP.put xid'
   pure xid'

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

-- | sendRequests - send all requests from the request list
sendRequests :: Socket.Socket -> ClMonad ()
sendRequests serverSock = do
    _ <- liftIO $ putStrLn "sendRequest"
    fds  <- collectFds
    reqs <- collectRequests
    _ <- liftIO $ Socket.withFdSocket serverSock (socketSend reqs fds )
    liftIO $ mapM_ closeFd fds
    st <- get
    -- liftIO $ printActiveIfaces (clActiveIfaces st)
    put st {clReqs = [], clFds = []}
  where
    collectFds :: ClMonad [Fd]
    collectFds = do reverse . clFds <$> get
    collectRequests :: ClMonad BS.ByteString
    collectRequests = do mconcat . reverse . clReqs <$> get
