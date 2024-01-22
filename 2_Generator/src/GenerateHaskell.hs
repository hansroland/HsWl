{-# LANGUAGE OverloadedStrings #-}
module GenerateHaskell where

import Types
import Casing
import Data.List(intersperse)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Builder.Linear ( fromText, runBuilder, Builder )

generateHaskell :: WlProtocol -> Text
generateHaskell = runBuilder . genProtocol

genProtocol :: WlProtocol -> Builder
genProtocol prot = genModuleHeader
    <> genMonad
    <> genLine "-- Constants for Interface Names"
    <> mconcat (genInterfaceConst <$> interfaces) <> bnl
    <> genTitle "* Request Handling"

    <> mconcat (genReqInterface <$> interfaces)
    <> genTitle "* Event Handling"
    <> genLine "-- ** Event Handling Function Types"
    <> mconcat (map genEventFunTypes interfaces) <> bnl
    <> genLine "-- ** Proxy Functions for all Events" <> bnl
    <> mconcat (map genProxyFunctions interfaces)
    <> genLine "-- ** Listeners for all Interfaces" <> bnl
    <> mconcat (map genListenerData interfaces) <> bnl
    <> genLine "-- data type ClState" <> bnl
    <> genClStateData interfaces
    <> genLine "-- Initializer for ClState" <> bnl
    <> genClStateInit interfaces
    <> genLine "-- Generate the setter functions for the listeners" <> bnl
    <> genSetListeners interfaces <> bnl
    <> genLine "-- Function dispatchEvent" <> bnl
    <> genDispatchEvent interfaces
    <> bnl
  where
    interfaces = protInterfaces prot


genModuleHeader :: Builder
genModuleHeader = genLine "{-# LANGUAGE OverloadedStrings #-}"
    <> bnl <> genLine "-- *** ATTENTION *** Generated Code *** DO NOT MODIFY" <> bnl
    <> genLine "module Wire where"
    <> genLine "import Types"
    <> genLine "import WireSupport" <> bnl
    <> genLine "import Data.Binary"
    <> genLine "import Data.Binary.Put"
    <> genLine "import Data.Binary.Get" <> bnl
    <> genLine "import qualified Control.Monad.State.Strict as ST"
    <> genLine "import qualified Data.ByteString            as BS"
    <> genLine "import qualified Data.ByteString.Lazy       as BL"
    <> genLine "import qualified Control.Monad              as M"
    <> genLine "import Data.Maybe (fromMaybe, isJust, isNothing, fromJust)"
    <> genLine "import qualified Data.Text as T"
    <> genLine "import Data.Text (Text)"
    <> bnl

-- Generate Monad
genMonad :: Builder
genMonad = genLine "type ClMonad a = ST.StateT ClState IO a" <> bnl

-- Generate a constant for each interface
-- > cWlDisplay :: Text
-- > cWlDisplay = "wl_display"
genInterfaceConst :: WlInterface -> Builder
genInterfaceConst ifac =
    ifName <> fromText " :: Text" <> bnl
      <>  ifName  <> fromText (" = \"" <> ifaWName ifac <> "\"") <> bnl
  where
    ifName = fromText $ nameInterface ifac

-- ----------------------------------------------------------------------
-- Request Handling
-- ----------------------------------------------------------------------

-- | Generate the requests of an Interface
genReqInterface :: WlInterface -> Builder
genReqInterface ifac =
  if hasRequests ifac
  then
    sectiontit ("Interface: " <> ifaName ifac <> " - " <> descrSummary (ifaDescr ifac)) <>
    bnl <>
    genRequests (ifaRequests ifac)
  else
    fromText T.empty

genRequests :: [WlRequest] -> Builder
genRequests reqs = mconcat $ map genRequest reqs

-- | Generate a single request
--
-- > wlDisplaySync :: Text -> ClState -> WNewId -> BS.ByteString
-- > wlDisplaySync wobj callback  = runByteString $ do
-- >     put wobj
-- >     put $ WOpc 0
-- >     putWord16host 12
-- >     put callback
genRequest :: WlRequest -> Builder
genRequest req = funtit ("Req: " <> descrSummary (reqDescr req) <> " opc:" <> tshow (reqOpc req)) <>
                 fromText (reqName req) <> genReqType req <>
                 fromText (reqName req) <> genReqBody req

genReqType :: WlRequest -> Builder
genReqType req = genLine $ " :: " <> "WObj -> " <> mconcat types
  where
    types = intersperse " -> " (map argType (reqArgs req) <> ["BS.ByteString"])

genReqBody :: WlRequest -> Builder
genReqBody req = fromText (" wobj " <> args  <> " = ")
               <> genReqDoHeader req
               <> genReqDoArgs req
               <> genReqDoWhere req
               <> bnl
  where
    args = mconcat $ intersperse " "  (map argName (reqArgs req))


genReqDoHeader :: WlRequest -> Builder
genReqDoHeader req = genLine "runByteString $ do" <>
      indent 4 <> genLine "put wobj"  <>
      indent 4 <> genLine ("put $ WOpc " <> tshow (reqOpc req)) <>
      indent 4 <> fromText "putWord16host " <>
      if needsWhere req
      then genLine " $ fromIntegral len"
      else genLine $ tshow $ getFixReqLength req

genReqDoArgs :: WlRequest -> Builder
genReqDoArgs req = mconcat $ map doReqArg (reqArgs req)

doReqArg :: WlArgument -> Builder
doReqArg arg = indent 4 <> genLine ("put " <> argName arg)

genReqDoWhere :: WlRequest -> Builder
genReqDoWhere req =
    if needsWhere req
    then indent 2 <> genLine ("where len = " <> tshow (getFixReqLength req) <>
           " + sum (calcWStringLength <$> " <> tshow (reqStringArgs req) <> ") " <>
           " + sum (calcWArrayLength  <$> " <> tshow (reqArrayArgs req)  <> ") ")
    else fromText T.empty

argLength4List :: [Text]  -- We store only types with length 4
argLength4List = ["WInt", "WUint", "WFixed", "WObj", "WNewId"]

-- A list of pairs describing the argument
--fst is type, snd is argName
reqArgPairs :: WlRequest -> [(Text, Text)]
reqArgPairs req = zip (argType <$> reqArgs req) (argName <$> reqArgs req)

-- A list of the names of the arguments with type WString
reqStringArgs :: WlRequest -> [Text]
reqStringArgs req = snd <$> filter (\(t,_) -> t == "WString" ) (reqArgPairs req)

-- A list of the names of the arguments with type WArray
reqArrayArgs :: WlRequest -> [Text]
reqArrayArgs req = snd <$> filter (\(t,_) -> t == "WArray" ) (reqArgPairs req)

needsWhere :: WlRequest -> Bool
needsWhere req = hasStringArgs req || hasArrayArgs req

hasStringArgs :: WlRequest -> Bool
hasStringArgs req = not $ null $ reqStringArgs req

hasArrayArgs :: WlRequest -> Bool
hasArrayArgs req = not $ null $ reqArrayArgs req

-- Calculate the fix part of the request length,
-- all data without WString and WArray data type arguments
getFixReqLength :: WlRequest -> Int
getFixReqLength req = 8 + 4 * length (filter (`elem` argLength4List) types)
  where
    types = map argType $ reqArgs req

-- ----------------------------------------------------------------------
-- Event Handling
-- ----------------------------------------------------------------------

-- Generate Event Function Types
-- > type TwlRegistryGlobal = WUint -> WString -> WUint -> ClMonad ()

-- Generate the Event function types for a single interface
genEventFunTypes :: WlInterface -> Builder
genEventFunTypes ifac = mconcat  (map genEventFunType $ ifaEvents ifac)

genEventFunType :: WlRequest -> Builder
genEventFunType req =
  fromText ("type " <> nameEventFunctionType req <> " = ")
    <> genEvTypes req <> bnl

genEvTypes :: WlRequest -> Builder
genEvTypes req = mconcat $ map fromText types
  where
    types = intersperse " -> " (map argType (reqArgs req) <> ["ClMonad ()"])

-- Generate Event Proxy Functions
--
-- For event functions with arguments
-- > pwlRegistryGlobal :: Maybe TwlRegistryGlobal -> ClState -> WInputMsg -> IO ()
-- > pwlRegistryGlobal Nothing _ _ = pure ()
-- > pwlRegistryGlobal (Just f) msg = runGet g $ BL.fromStrict $ winpData msg
-- >         where g = f <$> get <*> get <*> get

-- -- For event functions without arguments
-- > pwlBufferRelease :: Maybe TwlBufferRelease -> InputMsg -> ClMonad ()
-- > pwlBufferRelease Nothing _ = pure ()
-- > pwlBufferRelease (Just f) _msg = f

genProxyFunctions :: WlInterface -> Builder
genProxyFunctions ifac = mconcat  (map genProxyFunction $ ifaEvents ifac)

genProxyFunction :: WlRequest -> Builder
genProxyFunction req =
    fromText (proxyName <> " :: Maybe " <> nameEventFunctionType req
            <> " -> WInputMsg -> ClMonad ()")  <> bnl <>
    fromText (proxyName <> " Nothing _ = pure ()") <> bnl
            <> genArgs <> bnl <> bnl
  where
    proxyName = nameProxyFunction req
    args =  intersperse " <*> " (map (const "get") (reqArgs req))
    genArgs =
      if null (reqArgs req)
      then fromText (proxyName <> " (Just f) _msg = f")
      else
        genLine (proxyName <> " (Just f) msg = runGet g $ BL.fromStrict $ winpData msg") <>
        indent 4 <> fromText ("where g = f <$> " <> mconcat args)


-- Generate Listeners for all Interfaces (with all its events)
-- > data WlRegistryListener = WlRegistryListener
-- >     { wlRegistryGlobal :: Maybe TwlRegistryGlobal
-- >     , wlRegistryGlobalRemove :: Maybe TwlRegistryGlobalRemove}
genListenerData :: WlInterface -> Builder
genListenerData ifac =
    if hasEvents ifac
    then
      genLine (dataOrType <> listenerName <> " = " <> listenerName) <>
        indent 4 <> fromText "{" <> genListenerFields (ifaEvents ifac) <> indent 4 <> genLine "}"
    else fromText T.empty
  where
    listenerName = nameListenerData ifac
    dataOrType = if length (ifaEvents ifac) == 1 then "newtype " else "data "

genListenerFields :: [WlRequest] -> Builder
genListenerFields [] = fromText T.empty
genListenerFields (evt : events) = genListenerField evt <>
    mconcat (map (prefix . genListenerField) events)
  where
    prefix :: Builder -> Builder
    prefix b = indent 4 <> fromText ", " <> b

genListenerField :: WlRequest -> Builder
genListenerField req = fromText (reqName req <> " :: Maybe " <>
    nameEventFunctionType req) <> bnl

-- --------------------------------------------------------------------
-- Generate the ClState data type
-- --------------------------------------------------------------------

-- > data ClState = ClState {
-- >     clRegistryListener :: Maybe WlRegistryListener,
-- >     < more of the above lines for each interface>
-- >     clActiveIfaces :: [IfacKey],
-- >     clReqs :: [BS.ByteString] }
genClStateData :: [WlInterface] -> Builder
genClStateData ifacs = genClStateDataHeader
    <> mconcat (genClStateDataListener <$> filter hasEvents ifacs)
    <> genClStateDataEnd <> bnl

genClStateDataHeader :: Builder
genClStateDataHeader = genLine "data ClState = ClState {"

-- > clRegistryListener :: Maybe WlRegistryListener,
genClStateDataListener :: WlInterface -> Builder
genClStateDataListener ifac =
    indent 4 <> fromText (nameClStateListener ifac) <>
      fromText (" :: Maybe " <> nameListenerData ifac <> ",") <> bnl

genClStateDataEnd :: Builder
genClStateDataEnd = indent 4 <>
    genLine "clActiveIfaces :: [IfacKey],"
    <> indent 4 <> genLine "clReqs :: [BS.ByteString] }"

-- Generate the initialization function for the ClState data structure
-- > initClState :: ClState
-- > initClState = ClState {
-- >     clRegistryListener = Nothing
-- >     < more of the above lines for each interface>
-- >     clActiveIfaces = initActiveIfaces
-- >     clReqs = []  }

genClStateInit ::  [WlInterface] -> Builder
genClStateInit ifacs = genClStateInitHeader
    <> mconcat (genClStateInitListener <$> filter hasEvents ifacs)
    <> genClStateInitEnd

genClStateInitHeader :: Builder
genClStateInitHeader = fromText "initClState :: ClState" <> bnl
    <> fromText "initClState = ClState {"

-- > clRegistryListener :: Maybe WlRegistryListener,
genClStateInitListener :: WlInterface -> Builder
genClStateInitListener ifac =
    indent 4 <> genLine (nameClStateListener ifac <> " = Nothing,")

genClStateInitEnd :: Builder
genClStateInitEnd = {- bnl <> -}
        indent 4 <> genLine "clActiveIfaces = initActiveIfaces,"
        <> indent 4 <> genLine "clReqs = [] }"


-- Generate the setter functions for all listeners
genSetListeners :: [WlInterface] -> Builder
genSetListeners ifacs = mconcat (genSetListener <$> filter hasEvents ifacs)

-- Generate the setter function for a listener
-- > setRegistryListener :: WlRegistryListener -> ClMonad ()
-- > setRegistryListener lisnr = do
-- >     st <- get
-- >     put st {clRegistryListener = Just lisnr}
genSetListener :: WlInterface -> Builder
genSetListener ifac = genLine (nameSetListenerFunc ifac
             <> " :: " <> nameListenerData ifac <> " -> ClMonad ()")
    <> genLine (nameSetListenerFunc ifac <> " lisnr = do")
    <> indent 4 <> genLine "st <- ST.get"
    <> indent 4 <> genLine ("ST.put st { " <> nameClStateListener ifac <> " = Just lisnr }")
    <> bnl

-- --------------------------------------------------------------------
-- Generate dispatchEvent function
-- --------------------------------------------------------------------

-- > dispatchEvent :: WInputMsg -> ClMonad ()
-- > dispatchEvent msg = do
-- >     st <- ST.get
-- >     ST.liftIO $ print msg
-- >     let wopc = winpOpc msg
-- >     let ifName = fromMaybe T.empty (lookup (winpObj msg) (clActiveIfaces st))
-- >     ST.liftIO $ M.when (T.null ifName)
-- >          $ unhandledEv (tshow (winpObj msg)) wopc
-- >     -- ST.liftIO $ putStrLn $ "dispatchEvent for " <>
-- >     --     show ifName <> " obj:" <> show (winpObj msg) <> " opc:" <> show wopc
-- >     case ifName of
-- >             "wl_registry"  -> do
-- >                 let listener = clRegistryListener st
-- >                 M.when (isJust listener) $ do
-- >                   case winpOpc msg of
-- >                     0 -> pwlRegistryGlobal (wlRegistryGlobal (fromJust listener)) st msg
-- >                     _ -> pwlRegistryGlobalDelete (wlRegistryGlobalRemove (fromJust listener)) st msg
-- >
-- >             < More cases for each listener with events
-- >             _ ->  error ("dispatchEvent: No case for interface object " <> T.unpack ifName)

genDispatchEvent :: [WlInterface] -> Builder
genDispatchEvent ifacs = genDispatchEventHeader
    <> mconcat (genDispatchEventCase <$> filter hasEvents ifacs)
    <> genDispatchEventEnd

genDispatchEventHeader :: Builder
genDispatchEventHeader =
    genLine "dispatchEvent :: WInputMsg -> ClMonad ()"  <>
    genLine "dispatchEvent msg = do" <>
    indent 4 <> genLine "st <- ST.get" <>
    indent 4 <> genLine "ST.liftIO $ print msg" <>
    indent 4 <> genLine "let wopc = winpOpc msg" <>
    indent 8 <> genLine "ifName = fromMaybe T.empty (lookup (winpObj msg) (clActiveIfaces st))" <>
    indent 4 <> genLine "ST.liftIO $ M.when (T.null ifName)" <>
    indent 8 <> genLine "$ unhandledEv ((T.pack . show) (winpObj msg)) wopc" <>
    -- indent 4 <> genLine "ST.liftIO $ putStrLn $ \"dispatchEvent for \" <> " <>
    -- indent 8 <> genLine "show ifName <> \" obj:\" <> show (winpObj msg) <> \" opc:\" <> show wopc" <>
    indent 4 <> genLine "case ifName of"


genDispatchEventCase :: WlInterface -> Builder
genDispatchEventCase ifac =
  if hasEvents ifac
  then indent 8 <> genLine ( "\"" <> ifaWName ifac <> "\" -> do")
        <> indent 10 <> genLine ("let listener = " <> nameClStateListener ifac <> " st")
        <> indent 10 <> genLine "M.when (isNothing listener) $ ST.liftIO (unhandledEv ((T.pack . show) (winpObj msg)) wopc)"
        <> indent 10 <> genLine "M.when (isJust listener) $ do"
        <> indent 12 <> genLine "case winpOpc msg of"
        <> mconcat (map genDispatchEventCaseEntry (ifaEvents ifac))
        <> indent 14 <> genLine "_ -> error (T.unpack ifName <> \"Unknown op-code:\" <> show wopc)"
  else fromText T.empty


--  0 -> ST.liftIO $ pwlRegistryGlobal (wlRegistryGlobal (fromJust listener)) st msg
genDispatchEventCaseEntry :: WlRequest -> Builder
genDispatchEventCaseEntry req =
  indent 14 <> fromText (tshow (reqOpc req)  <> " -> " <> nameProxyFunction req <>
      " (" <> reqName req <> " (fromJust listener)) msg") <> bnl

genDispatchEventEnd :: Builder
genDispatchEventEnd =
  indent 8 <> genLine "_ ->  error (\"dispatchEvent: No case for interface object \" <> T.unpack ifName)"
      <> bnl

-- --------------------------------------------------------------------
-- Helper Functions to Generate Names
-- --------------------------------------------------------------------

-- Generate a pascal name for WlInterface.ifaWName
ifaName :: WlInterface -> Text
ifaName = snakeToPascal . ifaWName

-- Generate the name for a Listener data structure
-- > WlRegistryListener
nameListenerData :: WlInterface -> Text
nameListenerData ifac = ifaName ifac <> "Listener"

-- > cWlRegistryListener
nameInterface :: WlInterface -> Text
nameInterface ifac = T.cons 'c' $ ifaName ifac

 -- > clRegistryListener
nameClStateListener :: WlInterface -> Text
nameClStateListener ifac = "cl" <> T.drop 2 (nameListenerData ifac)

-- > setRegistryLisener
nameSetListenerFunc :: WlInterface -> Text
nameSetListenerFunc ifac = "set" <> T.drop 2 (nameListenerData ifac)

-- Generate the name of the type of an event function
-- eg: TwlRegistryGlobal
nameEventFunctionType :: WlRequest -> Text
nameEventFunctionType req = T.cons 'T' (reqName req)

nameProxyFunction :: WlRequest -> Text
nameProxyFunction req = T.cons 'p' (reqName req)


-- --------------------------------------------------------------------
-- Helper Functions for Text Formatting
-- --------------------------------------------------------------------
-- Generate a title line
genTitle :: Text -> Builder
genTitle txt =
    genLine "-- ----------------------------------------------------------------------" <>
    genLine ("-- " <> txt)  <>
    genLine "-- ----------------------------------------------------------------------"

-- Generate a code line
genLine :: Text -> Builder
genLine txt = fromText txt <> bnl

bnl :: Builder
bnl = fromText nl

funtit :: Text -> Builder
funtit txt = fromText "-- | " <> fromText txt <> bnl

sectiontit :: Text -> Builder
sectiontit txt = fromText "-- ** " <> fromText txt <> bnl


indent :: Int -> Builder
indent n = fromText (T.replicate n " ")
