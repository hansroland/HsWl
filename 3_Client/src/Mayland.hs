{-# LANGUAGE OverloadedStrings #-}

-- Mayland.hs = Manual Wayland.hs
-- This is the drawing board for the code to generate

module Mayland where

import Types
import WireSupport

import Data.Binary
import Data.Binary.Put
import Data.Binary.Get
import qualified Control.Monad.State.Strict as ST
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Lazy       as BL
import qualified Control.Monad              as M

import Data.Maybe (fromMaybe, isJust, fromJust)
import Data.List (find)

import qualified Data.Text as T
import Data.Text (Text)

-- Constants for Interface Names
cWlDisplay :: Text
cWlDisplay = "wl_display"

cWlRegistry :: Text
cWlRegistry = "wl_registry"

cWlCallback :: Text
cWlCallback = "wl_callback"

-- ----------------------------------------------------------------------
-- Request Handling
-- ----------------------------------------------------------------------
-- | Req: asynchronous roundtrip opc:0
wlDisplaySync :: Text -> ClState -> WNewId -> BS.ByteString
wlDisplaySync ifac st callback  = runByteString $ do
    putTObjId ifac st
    put $ WOpc 0
    putWord16host 12
    put callback

-- | Req: get global registry object opc:1
wlDisplayGetRegistry :: Text -> ClState -> WNewId -> BS.ByteString
wlDisplayGetRegistry ifac st registry = runByteString $ do
    putTObjId ifac st
    putWord16host 1
    putWord16host 12
    put registry

-- ----------------------------------------------------------------------
-- Event Handling
-- ----------------------------------------------------------------------
-- Function types
type TwlRegistryGlobal = WUint -> WString -> WUint -> ClMonad ()
type TwlRegistryGlobalRemove = WUint -> ClMonad ()
type TwlBufferRelease = ClMonad ()

-- Generate proxy functions for all events

-- Proxy to call function

pwlRegistryGlobal :: Maybe TwlRegistryGlobal -> WInputMsg -> ClMonad ()
pwlRegistryGlobal Nothing _ = pure ()
pwlRegistryGlobal (Just f) msg = runGet p $ BL.fromStrict $ winpData msg
        where p = f <$> get <*> get <*> get

pwlRegistryGlobalDelete :: Maybe TwlRegistryGlobalRemove -> WInputMsg -> ClMonad ()
pwlRegistryGlobalDelete Nothing _ = pure ()
pwlRegistryGlobalDelete (Just f) msg = runGet p $ BL.fromStrict $ winpData msg
        where p = f <$> get

pwlBufferRelease :: Maybe TwlBufferRelease -> WInputMsg -> ClMonad ()
pwlBufferRelease Nothing _ = pure ()
pwlBufferRelease (Just f) _msg = f

-- Generate Listeners for all Interfaces (with all its events)
data WlRegistryListener = WlRegistryListener
    { wlRegistryGlobal :: Maybe TwlRegistryGlobal
    , wlRegistryGlobalRemove :: Maybe TwlRegistryGlobalRemove}

newtype WlBuffer = WlBuffer
    {wlBufferRelease :: Maybe TwlBufferRelease}

type ClMonad a = ST.StateT ClState IO a

data ClState = ClState {
    clRegistryListener :: Maybe WlRegistryListener,
    clActiveIfaces :: [IfacKey],
    clMaxUsedIface :: WObj,
    clReqs :: [BS.ByteString] }

initClState :: ClState
initClState = ClState
    {clRegistryListener = Nothing
    , clActiveIfaces = initActiveIfaces
    , clMaxUsedIface = 1
    , clReqs = []
    }

-- Setter function for liseners
setRegistryListener :: WlRegistryListener -> ClMonad ()
setRegistryListener lisnr = do
    st <- ST.get
    ST.put st {clRegistryListener = Just lisnr}

dispatchEvent :: WInputMsg -> ClMonad ()
dispatchEvent msg = do
    st <- ST.get
    ST.liftIO $ print msg
    let ifName = fromMaybe T.empty (lookup (winpObj msg) (clActiveIfaces st))
    ST.liftIO $ M.when (T.null ifName)
         $ unhandledEv ((T.pack . show) (winpObj msg)) (winpOpc msg)
    ST.liftIO $ putStrLn $ "dispatchEvent for " <>
        show ifName <> " obj:" <> show (winpObj msg) <> " opc:" <> show (winpOpc msg)
    case ifName of
            "wl_display" -> do
                ST.liftIO $ putStrLn "Display Event"
            "wl_registry"  -> do
                let listener = clRegistryListener st
                M.when (isJust listener) $ do
                  case winpOpc msg of
                    0 -> pwlRegistryGlobal (wlRegistryGlobal (fromJust listener)) msg
                    _ -> pwlRegistryGlobalDelete (wlRegistryGlobalRemove (fromJust listener)) msg

            "wl_callback" -> do
                ST.liftIO $ putStrLn "Callback event !!"

            _ ->  error ("dispatchEvent: No case for interface object " <> T.unpack ifName)


-- Get an WObj from the interface text name
getObjectId :: Text -> ClState -> WObj
getObjectId txt st =
    fst $ fromMaybe (0, T.empty) (find ((==) txt . snd ) (clActiveIfaces st))

-- Put a text object name as WObj on a ByteStream
putTObjId :: Text -> ClState -> Put
putTObjId txt st = putWord32host $ fromIntegral $ getObjectId txt st
