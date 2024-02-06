{-
Copyright © 2014 Intel Corporation

Permission to use, copy, modify, distribute, and sell this software and its
documentation for any purpose is hereby granted without fee, provided that
the above copyright notice appear in all copies and that both that copyright
notice and this permission notice appear in supporting documentation, and
that the name of the copyright holders not be used in advertising or
publicity pertaining to distribution of the software without specific,
written prior permission.  The copyright holders make no representations
about the suitability of this software for any purpose.  It is provided "as
is" without express or implied warranty.

THE COPYRIGHT HOLDERS DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE,
INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO
EVENT SHALL THE COPYRIGHT HOLDERS BE LIABLE FOR ANY SPECIAL, INDIRECT OR
CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE,
DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER
TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE
OF THIS SOFTWARE.
-}

{-# LANGUAGE CPP                      #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module WaylandSocket (recvFromWayland)

where

import qualified Control.Concurrent as CC
import qualified Data.ByteString    as BS
import           Foreign
import           Foreign.C.Types
import qualified Network.Socket     as Socket
-- import           System.Posix.Types (Fd)

foreign import ccall unsafe "wayland-msg-handling.h recvmsg_wayland"
    c_recvmsg_wayland :: CInt -- fd
        -> Ptr CChar -- buf
        -> CInt -- bufsize
        -> Ptr CInt -- fds
        -> CInt -- fdbufsize
        -> Ptr CInt -- n_fds
        -> IO CInt -- bytes received

recvFromWayland :: Socket.Socket -> IO (BS.ByteString, [Int])
recvFromWayland s = allocaArray 4096 $ \cbuf -> do
    socket <- Socket.unsafeFdSocket s
    CC.threadWaitRead $ fromIntegral socket
    alloca $ \nFds_ptr ->
        allocaArray (4*28) $ \fdArray -> do
            len <- c_recvmsg_wayland socket cbuf 4096 fdArray (4*28) nFds_ptr
            if len < 0
                then ioError $ userError "recvmsg failed"
                else do
                    bs <- BS.packCStringLen (cbuf, fromIntegral len)
                    nFds <- peek nFds_ptr
                    fds <- peekArray (fromIntegral nFds) fdArray
                    return (bs, map fromIntegral fds)
