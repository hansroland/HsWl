{-# LANGUAGE OverloadedStrings #-}
module Wayland.Client.Client where

import Wayland.Client.Display
import Wayland.Client.Registry
import Wayland.Client.Callback
import Wayland.Client.Buffer
import Wayland.Client.Seat
import Wayland.Client.Shm
import Wayland.Client.Types
import Wayland.Client.Support
import Wayland.Client.XdgSurface
import Wayland.Client.XdgToplevel
import Wayland.Client.XdgWmBase

import Wayland.Wire.Wire
import Wayland.Wire.Protocol

import Control.Monad.State.Strict
import Network.Socket(Socket)

-- ---------------------------------------------------------------------------
-- Real Client Code
-- ---------------------------------------------------------------------------
runClient :: Socket -> ClMonad ()
runClient serverSock = do
    _ <- wlDisplayGetRegistry wlDisplayWObj cWlRegistry
    setDisplayListener wlDisplayListener
    setRegistryListener wlRegistryListener
    setCallbackListener wlCallbackListener
    _ <- wlDisplaySync wlDisplayWObj cWlCallback
    sendRequests serverSock
    socketRead serverSock

    -- https://gitlab.freedesktop.org/wayland/wayland/-/blob/main/src/wayland-shm.c?ref_type=heads
    -- https://en.m.wikipedia.org/wiki/Unix_domain_socket

    -- https://bugaevc.gitbooks.io/writing-wayland-clients/content/about-this-book/what-is-wayland.html


    setShmListener wlShmListener
    -- TODO: cleanup name setgWmBaseListener
    setgWmBaseListener xdgWmBaseListener
    setgToplevelListener xdgToplevelListener
    setgSurfaceListener xdgSurfaceListener
    setBufferListener wlBufferListener

    setSeatListener wlSeatListener
    setPointerListener wlPointerListener

    compositor <- getWObjForGlobal cWlCompositor
    surface <- wlCompositorCreateSurface compositor cWlSurface
    wmBase <- getWObjForGlobal cXdgWmBase
    xdgSurface <- xdgWmBaseGetXdgSurface wmBase cXdgSurface surface
    addLinkTo (surface, cWlSurface)  xdgSurface
    topLevel <- xdgSurfaceGetToplevel xdgSurface cXdgToplevel
    xdgToplevelSetTitle topLevel $ WString "Example client"
    wlSurfaceCommit surface

    let loop = do
          liftIO $ putStrLn "Send - Read Loop"
          sendRequests serverSock
          socketRead serverSock
          loop

    _ <- loop

    -- TODO a nice disconnect from the server
    -- wlDisplayDisconnect ptrDisplay
    liftIO $ putStrLn "disconnected!"


    return ()

