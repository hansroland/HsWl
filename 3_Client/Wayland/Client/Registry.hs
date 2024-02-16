{-# LANGUAGE OverloadedStrings #-}
module Wayland.Client.Registry where

import Wayland.Client.Types
import Wayland.Wire.Protocol
import Wayland.Wire.Support

import           Control.Monad.State.Strict
import qualified Data.Binary                as BP
import qualified Data.Binary.Put            as BP
import qualified Data.Text as T

-- | Define the callbacks for the RegisteryListener events
wlRegistryListener :: WlRegistryListener
wlRegistryListener = WlRegistryListener
    (Just registryGlobal)
    (Just registryGlobalRemove )

-- | Event: announce global object opc:0
  -- wlRegistryGlobal :: ClState ->  WUint -> WString -> WUint -> IO ()
registryGlobal :: TwlRegistryGlobal
registryGlobal wobj name interface version = do
    liftIO $ putStrLn $ "myRegistryGlobal: "
      <> show name
      <> " " <> show name
      <> " " <> show interface
      <> " " <> show version
    case interface of
      WString "wl_compositor" -> do
        liftIO $ putStrLn "GOTCHA REGISTER compositor"
        _ <- rsxRegistryBind wobj (fromIntegral name) interface version cWlCompositor
        pure ()

      WString "wl_shm" -> do
        liftIO $ putStrLn "GOTCHA REGISTER wl_shm"
        _ <- rsxRegistryBind wobj (fromIntegral name) interface version cWlShm
        pure ()

      WString "xdg_wm_base" -> do
        liftIO $ putStrLn "GOTCHA REGISTER xdg_wm_base"
        _ <- rsxRegistryBind wobj (fromIntegral name) interface version cXdgWmBase
        pure ()

      WString "wl_seat" -> do
        liftIO $ putStrLn "GOTCHA REGISTER wl_seat"
        _ <- rsxRegistryBind wobj (fromIntegral name) interface version cWlSeat
        pure ()

{-
      WString "zxdg_decoration_manager_v1" -> do
        ST.liftIO $ putStrLn "GOTCHA REGISTER zxdg_decoration_manager_v1"
        _ <- rsxRegistryBind wobj (fromIntegral name) interface version deora
        pure ()
-}


      _ -> pure()

    pure ()

registryGlobalRemove :: TwlRegistryGlobalRemove
registryGlobalRemove _ obj = do
    liftIO $ putStrLn $ "GOT myRegistryGlobalRemove " <> show obj

-- Note the wlRegistryBind function in the xml file is wrong
rsxRegistryBind :: WObj -> WUint -> WString -> WUint -> T.Text -> ClMonad WObj
rsxRegistryBind wobj name interface version xid = do
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

