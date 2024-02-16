{-# LANGUAGE OverloadedStrings #-}

module Wayland.Client.Support where

import Wayland.Client.Types
import Wayland.Wire.Protocol

import qualified Data.EnumMap.Strict        as Map

import           Control.Monad.State.Strict
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Maybe (fromMaybe)


-- * Management (non request) functions


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
