{-# LANGUAGE OverloadedStrings #-}

module Client where

-- import ClientSupport
import Protocol
import ProtocolSupport
import Types

import qualified Data.Binary                as BP
import qualified Data.Binary.Put            as BP

import Data.EnumMap.Strict (EnumMap)
import qualified Data.EnumMap.Strict        as Map

import           Control.Monad.State.Strict
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Maybe (fromMaybe)

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

-- * Management (non request) functions

-- | removeActiveIfac remove an interface from the map of active interfaces
removeActiveIfac :: WObj -> ClMonad ()
removeActiveIfac wobj  = do
    st <- get
    let newacts =  Map.delete wobj $ clActiveIfaces st
    liftIO $ printActiveIfaces newacts
    put $ st {clActiveIfaces = newacts }

-- | Get the WObj of the specified global interface
getWObjForGlobal :: Text -> ClMonad WObj
getWObjForGlobal strIfac = do
   st <- get
   let globs = filter ((== strIfac) . ifacName . snd ) $ Map.assocs (clActiveIfaces st)
   if null globs
      then error $ "Global object " <> T.unpack strIfac <> " not available."
      else (pure . fst . head)  globs

addLinkTo :: (WObj,Text) -> WObj -> ClMonad ()
addLinkTo link parentWObj = do
   st <- get
   let actives = clActiveIfaces st
   let mbParentObject = Map.lookup parentWObj actives
   let parentObject = fromMaybe
         (error ("addLinkTo: Object " <> show parentWObj <> " not found.")) mbParentObject
       newParentObject = parentObject{ ifacLink = link : ifacLink parentObject }
       newActives = Map.insert parentWObj newParentObject (clActiveIfaces st)
   put $ st {clActiveIfaces = newActives}

-- | Retrieve all the WObj's from children from the specified interface
getWObjLinks :: WObj -> Text -> ClMonad [WObj]
getWObjLinks parentWObj iface = do
   st <- get
   let actives = clActiveIfaces st
       mbParentObject = Map.lookup parentWObj actives
       parentObject = fromMaybe
         (error ("getWObjChildren: Object " <> show parentWObj <> " not found.")) mbParentObject
   pure $ map fst $ filter ((==) iface . snd) (ifacLink parentObject)

getWObjLink :: WObj -> Text -> ClMonad WObj
getWObjLink parentWObj iface = do
   links <- getWObjLinks parentWObj iface
   if length links == 1
      then pure $ head links
      else error ("getWObjLink: wrong number of links found for wobj " <> show parentWObj)
