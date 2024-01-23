{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Shm where

import Test.RandomStrings

-- import Foreign.C.String
import Foreign.Ptr (Ptr, FunPtr, nullPtr)
-- import Foreign.Storable
-- import Foreign.Marshal.Alloc

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
randomFileName = (<>) <$> pure "/wl_shm-" <*> (randomString (onlyLower randomASCII) 6)


allocateShmFile :: Int -> IO Fd
allocateShmFile sz = do
  fd <- createShmFile
  setFdSize fd (COff (fromIntegral sz))
  return fd
