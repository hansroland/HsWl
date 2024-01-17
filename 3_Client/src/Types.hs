{-# Language GeneralisedNewtypeDeriving, DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
module Types where

import Data.Word
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Control.Monad as M

import Data.Binary
import Data.Binary.Put
import Data.Binary.Get
import Text.Printf

import qualified Data.ByteString       as BS
-- --------------------------------------------------------------------
-- Wayland types on the wire
-- --------------------------------------------------------------------

newtype WObj = WObj Word32
    deriving (Eq, Ord, Enum, Num)
    deriving newtype (Read, Show, Integral, Real)

instance Binary WObj where
    put (WObj n) = putWord32host n
    get = WObj <$> getWord32host


newtype WNewId = WNewId Word32
    deriving (Eq, Ord, Enum, Num)
    deriving newtype (Read, Show, Integral, Real)

instance Binary WNewId where
    put (WNewId n) = putWord32host n
    get = WNewId <$> getWord32host

newtype WOpc = WOpc Word16
    deriving (Eq, Ord, Enum, Num)
    deriving newtype (Read, Show, Integral, Real)

instance Binary WOpc where
    put (WOpc n) = putWord16host n
    get = WOpc <$> getWord16host

newtype WUint = WUint Word32
    deriving (Eq, Ord, Enum, Num)
    deriving newtype (Read, Show, Integral, Real)

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
    put (WString _s) = error "put instance for WString not yet defined"
    get = parseWString

-- TODO WEventId Still used ??
newtype WEventId = WEventId Word16
    deriving (Eq, Ord, Enum, Num)
    deriving newtype (Read, Show, Integral, Real)

instance Binary WEventId where
    put (WEventId _s) = error "put instance for WEventId not yet defined"
    get = error "get instance for WEventId not yet defined"


newtype WFixed = WFixed Word32
    deriving (Eq, Ord, Enum, Num)
    deriving newtype (Read, Show, Integral, Real)

instance Binary WFixed where
    put (WFixed _s) = error "put instance for WFixed not yet defined"
    get = error "get instance for WFixed not yet defined"


newtype WFd = WFd Int
    deriving (Eq, Ord, Enum, Num)
    deriving newtype (Read, Show, Integral, Real)

instance Binary WFd where
    put (WFd _s) = error "put instance for WFd not yet defined"
    get = error "get instance for WFd not yet defined"


newtype WArray = WArray Int
    deriving (Eq, Ord, Enum, Num)
    deriving newtype (Read, Show, Integral, Real)

instance Binary WArray where
    put (WArray _s) = error "put instance for WArray not yet defined"
    get = error "get instance for WArray not yet defined"


parseWString :: Get WString
parseWString = do
    lenW <- getWord32host
    let dataLen = fromIntegral lenW
    if dataLen == 0 -- this is a null string, it's not even an empty string
        then pure $ WString "(null)" -- TODO: better representation?
        else do
            bs <- getByteString (dataLen - 1)
            skip 1                                  -- terminating NUL byte
            skip $ paddedLength dataLen - dataLen   -- read padded bytes
            return $ WString $ T.decodeUtf8 bs

-- Calculate the number of bytes needed to pad up to a 4 byte boundary
paddedLength :: Int -> Int
paddedLength len = if rem len 4 == 0
        then len
        else len + 4 - rem len 4

putWString :: WString -> Put
putWString (WString txt) = do
    let len = 1 + T.length txt
        pad = paddedLength len
    putWord32host $ fromIntegral len
    put $ T.encodeUtf8 txt
    putWord8 0                 -- terminating NUL byte
    M.replicateM_ pad $ putWord8 0

calcWStringLength :: WString -> Int
calcWStringLength (WString txt) =
    let len = 1 + T.length txt
        pad = paddedLength len
    in 8 + len + pad

calcWArrayLength  :: WArray -> Int
calcWArrayLength _ = undefined


toWEventId :: Integral a => a -> WEventId
toWEventId = WEventId . fromIntegral


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

