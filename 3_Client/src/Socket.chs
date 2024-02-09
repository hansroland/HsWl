{-# Language CApiFFI #-}
{-# Language ScopedTypeVariables #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

module Socket where

-- import Network.Socket
import qualified Data.ByteString            as BS
import qualified Network.Socket             as Socket
import qualified Control.Concurrent         as CC
import           Data.ByteString.Internal

import           System.Posix.Types (Fd(..))
import           Data.Word
import           Control.Monad

import           Foreign.C.Types
import           Foreign.Ptr
import           Foreign.ForeignPtr
import           Foreign.Storable
import           Foreign.Marshal.Alloc

#include <sys/types.h>
#include <sys/socket.h>

-- | Send the data contained in the bytestring to the specified address.
-- We allocate one single buffer filled as follows:
-- +-----------+-----------+-----------+
-- |   IOVec   |   MsgHdr  |  Cmsg     |
-- +-----------+-----------+-----------+
socketSend :: BS.ByteString -> [Fd] -> CInt -> IO Int
socketSend bs fds socketFd = do
    let iovlen = {#sizeof iovec #}
        msglen = {#sizeof msghdr#}
        cmsgLen = calcCmsgLen
    allocaBytes (iovlen + msglen + cmsgLen) $ \bufPtr -> do
        let ptrIov = castPtr bufPtr
            ptrMsg = plusPtr ptrIov iovlen
            ptrCmsg = plusPtr ptrMsg msglen
            -- Build iovec c-structure
        let (fptrBs, bsLen) = toForeignPtr0 bs
        putStrLn ("Number of bytes to send:" <> show bsLen)
        withForeignPtr fptrBs $ \ptrBs -> do
            poke ptrIov IOVec  { iovBase = castPtr ptrBs
                               , iovLen = bsLen }
            -- Build msghdr c-structure
            poke ptrMsg MsgHdr { msgName = nullPtr
                               , msgNameLen = 0
                               , msgIov = ptrIov
                               , msgIovLen = 1
                               , msgControl = ptrCmsg
                               , msgControlLen = cmsgLen
                               , msgFlags = 0 }
            -- Build Cmsg structure
            -- I don't have a `good` Cmsg structure in c-code. Therefore this little hack!
            let wcmsgLen :: Word64 = fromIntegral cmsgLen
                wfds :: [Word32] = fromIntegral <$> fds
                plusPtrCmsg = plusPtr ptrCmsg
            poke (plusPtrCmsg 0)  wcmsgLen
            poke (plusPtrCmsg 8)  solSocket
            poke (plusPtrCmsg 12) scmRights
            let ptrLoop = plusPtrCmsg 16
                ixFds = zip [0..] wfds
            mapM_ (\(ix,fd) ->
                poke (plusPtr ptrLoop (ix * 4)) fd)
                ixFds
            len <- c_sendmsg socketFd ptrMsg 0    -- noSignal <> Socket.MSG_DONTWAIT)
            putStrLn $ "RSX socketSend len:" <> show len
            when (len < 0) $ ioError $ userError "sendmsg failed"
            pure len
  where
    -- Calculate the length of the cmsg_buf
    calcCmsgLen :: Int
    calcCmsgLen = 16 + 4 * length fds

-- | Socket receive data
--   At the moment, we ignore the Fds. We need an example to test...
--  +-----------+-----------+-----------------+
--  |   IOVec   |   MsgHdr  |  Inputdata (bs) |
--  +-----------+-----------+-----------------+
socketReceive :: Socket.Socket -> IO BS.ByteString
socketReceive serverSock = Socket.withFdSocket serverSock socketReceive'

socketReceive' :: CInt -> IO BS.ByteString
socketReceive' socketFd = do
    let iovlen = {#sizeof iovec #}
        msglen = {#sizeof msghdr#}
    let bufflen = iovlen + msglen + 4096
    CC.threadWaitRead $ fromIntegral socketFd
    allocaBytes bufflen $ \bufPtr -> do
        let ptrIov = castPtr bufPtr
            ptrMsg = plusPtr ptrIov iovlen
            ptrBs = plusPtr ptrMsg msglen
        -- Build iovec c-structure
        poke ptrIov IOVec  { iovBase = ptrBs
                           , iovLen = 4096 }
        poke ptrMsg MsgHdr { msgName = nullPtr
                           , msgNameLen = 0
                           , msgIov = ptrIov
                           , msgIovLen = 1
                           , msgControl = nullPtr        -- TODO add adress of Control Buffer
                           , msgControlLen = 0
                           , msgFlags = 0 }
        len <- c_rcvmsg socketFd ptrMsg 0
        putStrLn $ "RSX socketReceive len:" <> show len
        if len < 0
            then ioError $ userError "recvmsg failed"
            else do
                BS.packCStringLen (ptrBs, fromIntegral len)

-- | The Haskell version of the c-msghdr structure
data MsgHdr = MsgHdr
    { msgName       :: !(Ptr Word8)
    , msgNameLen    :: !Int                   -- (#type socklen_t)
    , msgIov        :: !(Ptr IOVec)
    , msgIovLen     :: !Int                  -- CSize or CInt
    , msgControl    :: !(Ptr Word8)
    , msgControlLen :: !Int                  -- or CInt
    , msgFlags      :: !CInt
    }

instance Storable MsgHdr where
  sizeOf    _ = {#sizeof msghdr#}               -- (#const sizeof(struct msghdr))
  alignment _ = 0

  peek p = do
    name       <- {#get struct msghdr.msg_name#}       p
    nameLen    <- {#get struct msghdr.msg_namelen#}    p
    iov        <- {#get struct msghdr.msg_iov#}        p
    iovLn      <- {#get struct msghdr.msg_iovlen#}     p
    ctrl       <- {#get struct msghdr.msg_control#}    p
    ctrlLen    <- {#get struct msghdr.msg_controllen#} p
    flags      <- {#get struct msghdr.msg_flags#}      p
    return $ MsgHdr (castPtr name) (fromIntegral nameLen)
              (castPtr iov) (fromIntegral iovLn)
              (castPtr ctrl) (fromIntegral ctrlLen) flags

  poke p mh = do
    {#set struct msghdr.msg_name#}       p (castPtr         (msgName mh))
    {#set struct msghdr.msg_namelen#}    p (fromIntegral    (msgNameLen mh))
    {#set struct msghdr.msg_iov#}        p (castPtr         (msgIov mh))
    {#set struct msghdr.msg_iovlen#}     p (fromIntegral    (msgIovLen mh))
    {#set struct msghdr.msg_control#}    p (castPtr         (msgControl mh))
    {#set struct msghdr.msg_controllen#} p (fromIntegral    (msgControlLen mh))
    {#set struct msghdr.msg_flags#}      p (fromIntegral    (msgFlags mh))

-- | The Haskell version of the c-iovec structure
data IOVec = IOVec
    { iovBase :: !(Ptr ())
    , iovLen  :: !Int
    }

instance Storable IOVec where
  sizeOf    _ = {#sizeof iovec #}
  alignment _ = 0

  peek p = do
    base <- {#get struct iovec.iov_base#} p
    len  <- {#get struct iovec.iov_len#} p
    return $ IOVec (castPtr base) (fromIntegral len)

  poke p iov = do
     {#set iovec.iov_base#} p (castPtr      (iovBase iov))
     {#set iovec.iov_len#}  p (fromIntegral (iovLen iov))


solSocket :: Word32
solSocket = 1

scmRights :: Word32
scmRights = 1


foreign import ccall unsafe "stdio.h sendmsg"
    c_sendmsg :: CInt -> Ptr Word64 -> CInt -> IO Int
--      Socket.MSG_NOSIGNAL <> Socket.MSG_DONTWAIT;   --msgflag

foreign import ccall unsafe "stdio.h recvmsg"
    c_rcvmsg :: CInt -> Ptr Word64 -> CInt -> IO Int

