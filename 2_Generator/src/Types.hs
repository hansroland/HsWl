{-# LANGUAGE OverloadedStrings #-}

module Types

-- TODO: Make all summaries Maybe's

where

import qualified Data.Text as T
import           Data.Text( Text )
import           Data.List

data WlProtocol = WlProtocol
    { protName :: !Text
    , protInterfaces :: [WlInterface]
    }

data WlDescription = WlDescription
    { descrSummary :: !Text
    , descrBody :: !Text
    }

data WlInterface = WlInterface
    { ifaOid :: Int
    , ifaWName :: !Text
    , ifaDescr :: WlDescription
    , ifaVersion :: !Int
    , ifaRequests :: [WlRequest]
    , ifaEvents   :: [WlRequest]
    }

-- Return True, if this interface has events
hasEvents :: WlInterface -> Bool
hasEvents = not . null . ifaEvents

-- Return True, if this interface has requests
hasRequests :: WlInterface -> Bool
hasRequests = not . null . ifaRequests

data RequestType = Request | Event
    deriving (Eq)

data WlRequest = WlRequest
    { reqType :: RequestType
    , reqOpc :: !Int
    , reqName :: !Text
    , reqDescr :: WlDescription
    , reqArgs :: [WlArgument]
    }

-- | The name for the helper dispatch function
reqDspName :: WlRequest -> Text
reqDspName req = T.cons 'd' $ reqName req

-- TODO Check the use of strict maybe
--     https://stackoverflow.com/questions/34656816/strict-maybe-in-data-definitions
data WlArgument = WlArgument
    { argName :: !Text
    , argType :: !Text
    , argSummary :: Maybe Text
    }

class TShow a  where
    tshow :: a -> Text

instance (TShow a) => TShow (Maybe a) where
    tshow Nothing   = "Nothing"
    tshow (Just a) = "Just " <> tshow a

instance (TShow a) => TShow [a] where
    tshow lst = "[" <> mconcat (intersperse "," (tshow <$> lst)) <> "]"

instance TShow Int where
    tshow = T.pack . show

instance TShow Text where
    tshow txt = txt


nl :: Text
nl = "\n"

instance TShow WlProtocol where
    tshow prot = "protocol: " <> protName prot <> nl
       <> T.concat (map tshow $ protInterfaces prot)

instance TShow WlInterface where
    tshow ifa = "interface: " <> ifaWName ifa <> nl
        <> tshow (ifaDescr ifa) <> nl
        <> T.concat (map tshow $ ifaRequests ifa)
        <> T.concat (map tshow $ ifaEvents ifa)

instance TShow RequestType where
    tshow Request = "   request: "
    tshow Event   = "   event:   "

instance TShow WlRequest where
    tshow req = tshow (reqType req) <> reqName req <> nl
        <> tshow (reqDescr req)
        <> T.concat (map tshow $ reqArgs req)

instance TShow WlArgument where
    tshow arg = "       arg:    " <> argName arg
        <> " type: "    <> argType arg
        <> " summary: " <> tshow (argSummary arg)
        <> nl

instance TShow WlDescription where
    tshow descr = " summary: " <> descrSummary descr <> nl
         <> descrBody descr
