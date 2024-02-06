{-# Language ScopedTypeVariables #-}
{-# Language CApiFFI #-}

module Socket where

-- See: https://ro-che.info/articles/2017-08-06-manage-allocated-memory-haskell

import qualified Data.ByteString            as BS
import           Data.ByteString.Internal
import           Foreign.Ptr
import           Foreign.ForeignPtr
import           Data.Binary
-- import           Data.Binary.Put
-- import           Data.Coerce
import           Foreign.Marshal.Alloc
import           Foreign
import           Foreign.C.Types
import           Foreign.C.String
import           System.Posix.Types (Fd(..))
-- import           Posix.Socket.Types(noSignal)

-- #include <stdio.h>

-- Delete the Fd's after the send !!!
socketSend :: BS.ByteString -> [Fd] -> CInt -> IO (Int)
socketSend bs fds socketFd = do
    --Allocate Buffer for IOVec MsgHdr and Cmsg
    let cmsgLen = calcCmsgLen fds
    let bufflen = msgHdrLen + iovecLen + cmsgLen
    allocaBytes bufflen $ \ptrBuff -> do
        let ptrIovec = ptrBuff
            ptrMsg = plusPtr ptrIovec iovecLen
            ptrCmsg = plusPtr ptrMsg msgHdrLen
        -- Build iovec
        let (fptrBs, bsLen) = toForeignPtr0 bs
        withForeignPtr fptrBs $ \ptrBs -> do
            poke ptrIovec ptrBs                      -- *iov_base
            poke (plusPtr ptrIovec 8) bsLen          -- iov_len
            -- Build Cmsg
            let wcmsgLen :: Word64 = fromIntegral cmsgLen
                wfds :: [Word32] = fromIntegral <$> fds
            pokew64 ptrCmsg  0 wcmsgLen
            pokew32 ptrCmsg 8 1
            pokew32 ptrCmsg 12 1
            let ptrLoop = plusPtr ptrCmsg 16
                ixFds = zip [0..] wfds
            mapM_ (\(ix,fd) ->
                pokew32 ptrLoop (ix * 4) fd)
                ixFds
            -- Build msghdr
            pokew64 ptrMsg 0 0                -- *msg_name
            pokew64 ptrMsg 8 0                -- msg_namelen
            poke (plusPtr ptrMsg 16) (castPtr ptrIovec)        -- *msg_iovec
            pokew64 ptrMsg 24 1               -- msg_iovlen
            poke (plusPtr ptrMsg 32) ptrCmsg  -- *msg_control
            pokew64 ptrMsg 40 wcmsgLen        -- msg_controllen
            pokew64 ptrMsg 48 0               -- msg_flags

            let title1 = "msghdr"
            withCString title1 $ \c_str ->
                hexprint c_str ptrMsg 56

            let title2 = "iovec"
            withCString title2 $ \c_str ->
                hexprint c_str (castPtr ptrIovec) 16

            let title3 = "cmsg"
            withCString title3 $ \c_str ->
                hexprint c_str (castPtr ptrCmsg) $ fromIntegral cmsgLen


            c_sendmsg socketFd ptrMsg 0    -- noSignal <> Socket.MSG_DONTWAIT)



-- Calculate the length of the cmsg_buf
calcCmsgLen :: [Fd] -> Int
calcCmsgLen fds = 16 + 4 * length fds

msgHdrLen :: Int
msgHdrLen = 56

iovecLen :: Int
iovecLen = 16

pokew64 :: Ptr Word64 -> Int -> Word64 -> IO()
pokew64 ptr offset = poke (plusPtr ptr offset)

pokew32 :: Ptr Word64 -> Int -> Word32 -> IO()
pokew32 ptr offset = poke (plusPtr ptr offset)


foreign import capi unsafe "wayland-msg-handling.h hexprint"
    hexprint  :: Ptr CChar   -- titel
        -> Ptr Word64        -- buffer to hexprint
        -> CInt              -- bufsize
        -> IO ()             -- bytes sent

foreign import ccall unsafe "stdio.h sendmsg"
    c_sendmsg :: CInt -> Ptr Word64 -> CInt -> IO Int

--      Socket.MSG_NOSIGNAL <> Socket.MSG_DONTWAIT;   --msgflag
