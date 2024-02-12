{-# Language GeneralisedNewtypeDeriving, DerivingStrategies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Types where

import Data.Word
import Data.Text (Text)
import Data.Bits
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as T
import qualified Control.Monad              as M
import Data.EnumMap.Strict (EnumMap)
import qualified Data.EnumMap.Strict        as Map

import Data.Binary
import Data.Binary.Put
import Data.Binary.Get
import Text.Printf
import System.Posix.Types (Fd(..))

import qualified Data.ByteString       as BS

-- --------------------------------------------------------------------
-- Wayland types on the wire
-- --------------------------------------------------------------------

-- | Wayland wire object id - The 32 bit object id we send and receive over the wire
newtype WObj = WObj Word32
    deriving (Eq, Ord, Enum, Num)
    deriving newtype (Read, Show, Integral, Real)

instance Binary WObj where
    put (WObj n) = putWord32host n
    get = WObj <$> getWord32host

-- | The types WNewId and WObj are identical
type WNewId = WObj  -- Do not distinguish between WNewId and WObj

-- Object code: Request or event code of a Wayland wire message
newtype WOpc = WOpc Word16
    deriving (Eq, Ord, Enum, Num)
    deriving newtype (Read, Show, Integral, Real)

instance Binary WOpc where
    put (WOpc n) = putWord16host n
    get = WOpc <$> getWord16host

newtype WUint = WUint Word32
    deriving (Eq, Ord, Enum, Num)
    deriving newtype (Read, Show, Integral, Real, Bits)

instance Binary WUint where
    put (WUint n) = putWord32host n
    get = WUint <$> getWord32host


newtype WInt = WInt Word32
    deriving (Eq, Ord, Enum, Num)
    deriving newtype (Read, Show, Integral, Real)

instance Binary WInt where
    put (WInt n) = putWord32host n
    get = WInt <$> getWord32host

newtype WString = WString {getString ::Text}
    deriving (Eq)
    deriving newtype (Read, Show)

instance Binary WString where
    put = putWString
    get = parseWString


newtype WFixed = WFixed Word32
    deriving (Eq, Ord, Enum, Num)
    deriving newtype (Read, Show, Integral, Real)

-- TODO Decide about representation of WFixed
instance Binary WFixed where
    put (WFixed _s) = error "put instance for WFixed not yet defined"
    get = parseWFixed


instance Binary Fd where
    -- WFds are not sent over the wire. We use the ancillary channel of
    put (Fd _s) = pure ()
    get = error "get instance for Fd not yet defined"


newtype WArray = WArray [WUint]
    deriving (Eq, Ord)
    deriving newtype (Read, Show)

instance Binary WArray where
    put (WArray _s) = error "put instance for WArray not yet defined"
    get = parseWArray

-- | Wayland Object as we use it in the client.
data WObject = WObject
    { ifacName :: Text                     -- eg "wl_display"
    , ifacLink :: [(WObj, Text)]           -- Links to other objects  of this interface
    }

addLink :: (WObj,Text) -> WObject -> WObject
addLink link wobject = wobject { ifacLink = link : ifacLink wobject }

-- | The object id of WlDisplay is fixed 1
wlDisplayWObj :: WObj
wlDisplayWObj = 1

emptyWObject :: WObject
emptyWObject = WObject T.empty []

-- | printActiveIfaces - for debugging
printActiveIfaces :: EnumMap WObj WObject -> IO ()
printActiveIfaces ifaces = do
    putStrLn ("ACTIVE Interfaces: " <> concatMap showIfac assocs)
  where
    assocs = Map.assocs ifaces
    showIfac :: (WObj, WObject) -> String
    showIfac (wobj, WObject name _subs) = "(" <> show wobj <> ", "  <> T.unpack name  <>
     " subs:" <> show _subs <> ")"


parseWString :: Get WString
parseWString = do
    lenW <- getWord32host
    let dataLen = fromIntegral lenW
    if dataLen == 0 -- this is a null string, it's not even an empty string
        then pure $ WString $ T.pack "(null)" -- TODO: better representation?
        else do
            bs <- getByteString (dataLen - 1)
            skip 1                                  -- terminating NUL byte
            skip $ paddedLength dataLen - dataLen   -- read padded bytes
            return $ WString $ T.decodeUtf8 bs

-- TODO: Get a rational number for this !!
parseWFixed :: Get WFixed
parseWFixed = do
    val <- getWord32host
    pure (WFixed val)


-- Calculate the number of bytes needed to pad up to a 4 byte boundary
paddedLength :: Int -> Int
paddedLength len = if rem len 4 == 0
        then len
        else len + 4 - rem len 4

putWString :: WString -> Put
putWString (WString txt) = do
    let len = 1 + T.length txt
        pad = paddedLength len - len
    putWord32host $ fromIntegral len
    putByteString $ T.encodeUtf8 txt
    putWord8 0   -- terminating NUL byte
    M.replicateM_ pad $ putWord8 0

calcWStringLength :: WString -> Int
calcWStringLength (WString txt) =
    let len = 1 + T.length txt
        pad = paddedLength len
    in 4 + pad

calcWArrayLength  :: WArray -> Int
calcWArrayLength _ = undefined

-- TODO Remove this wart
unWString :: WString -> Text
unWString (WString txt) = txt


parseWArray :: Get WArray
parseWArray = do
    len <- getWord32host
    arr <- mapM (const get) [1..(len `div` 4)] -- Array length is in bytes
    pure $ WArray arr

data WInputMsg = WInputMsg
    { winpObj :: WObj
    , winpOpc  :: WOpc
    , winpLen :: Int
    , winpData :: BS.ByteString
    }
    deriving (Eq)

instance Show WInputMsg where
    show msg = "WInputMessage: " <> show (winpObj msg) <> " " <> show (winpOpc msg) <> " " <> show (winpLen msg)
               <> " " <> toHexString1 (winpData msg)

toHexString1 :: BS.ByteString -> String
toHexString1 = BS.foldr ((<>) . printf "%02x") ""
