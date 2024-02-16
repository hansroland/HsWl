{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Wayland.Client.OsShm where

import Test.RandomStrings


import Foreign.C
-- import Foreign.Ptr (Ptr, wordPtrToPtr)

import Foreign (Ptr, wordPtrToPtr)

import System.Posix.SharedMem
import System.Posix.Types
import System.Posix.Files


-- Open Shared memory
createShmFile :: IO Fd
createShmFile = do
  fn <- randomFileName
  let flags = ShmOpenFlags { shmReadWrite = True
                           , shmCreate = True
                           , shmExclusive = True
                           , shmTrunc = False }
  -- TODO: check for success and unlink in case of failure !!
  shmOpen fn flags (CMode 0600)

-- Create a random filename for the Shared memory
randomFileName :: IO String
randomFileName = (<>) <$> pure "/wl_shm-" <*> randomString (onlyLower randomASCII) 6


allocateShmFile :: Int -> IO Fd
allocateShmFile sz = do
  fd <- createShmFile
  setFdSize fd (COff (fromIntegral sz))
  return fd

-- --------------------------------------------------------------------
-- Posix memory mapping
-- --------------------------------------------------------------------
foreign import ccall "mmap"                       -- Using ccall results in a warning
   mmap :: Ptr () -> CSize -> CInt -> CInt-> Fd -> CInt -> IO (Ptr a)

foreign import ccall "munmap"                     -- Using ccall results in a warning
   munmap :: Ptr a -> CSize -> IO ()

cPROT_EXEC = 4
cPROT_EXEC :: (Num a) => a

cPROT_NONE = 0
cPROT_NONE :: (Num a) => a

cPROT_READ = 1
cPROT_READ :: (Num a) => a

cPROT_WRITE = 2
cPROT_WRITE :: (Num a) => a

cMAP_FIXED = 16
cMAP_FIXED :: (Num a) => a

cMAP_PRIVATE = 2
cMAP_PRIVATE :: (Num a) => a

cMAP_SHARED = 1
cMAP_SHARED :: (Num a) => a

cMAP_FAILED = wordPtrToPtr 4294967295
cMAP_FAILED :: Ptr a

wL_SHM_FORMAT_XRGB8888 = 1
wL_SHM_FORMAT_XRGB8888 :: (Num a) => a

-- See https://stackoverflow.com/questions/30446690/how-do-i-read-a-shared-memory-using-haskells-mmap-library
-- See Bindings.Posix.Sys.Mman in bindings-posix
-- Note: Last CInt was Int64 !!
-- See: https://man7.org/linux/man-pages/man2/mmap.2.html

