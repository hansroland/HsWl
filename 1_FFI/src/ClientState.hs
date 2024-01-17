module ClientState
where

import WaylandADTs

import Foreign.Ptr (nullPtr)
import Data.IORef ( IORef, atomicModifyIORef', newIORef, readIORef )
import System.IO.Unsafe (unsafePerformIO)

-- --------------------------------------------------------------------
-- Client State
-- --------------------------------------------------------------------
data ClientState = ClientState {
    clCount :: Int
    , clDisplay :: PtrDisplay
    , clCompositor :: PtrCompositor
    , clRegistry :: PtrRegistry
    , clShm :: PtrShm
    , clSurface :: PtrSurface
    , clXdgWmBase :: PtrXdgWmBase
    , clXdgSurface :: PtrXdgSurface
    , clXdgToplevel :: PtrXdgToplevel
    }

initialClientState :: ClientState
initialClientState = ClientState {
    clCount = 0
    -- Globals
    , clDisplay = nullPtr
    , clCompositor = nullPtr
    , clRegistry = nullPtr
    , clShm = nullPtr
    , clXdgWmBase = nullPtr
    -- Objects
    , clSurface = nullPtr
    , clXdgSurface = nullPtr
    , clXdgToplevel = nullPtr}

incrementCount :: ClientState -> ClientState
incrementCount state = state {clCount = clCount state  + 1}

-- IORef stuff from https://jacobstanley.io/how-can-i-create-a-global-variable/
clientStateRef :: IORef ClientState
clientStateRef =
  unsafePerformIO (newIORef initialClientState)
{-# NOINLINE clientStateRef #-}

getClientState :: IO ClientState
getClientState =
  readIORef clientStateRef

setClientState :: ClientState -> IO ()
setClientState stat =
  atomicModifyIORef' clientStateRef (\_ -> (stat, ()))

