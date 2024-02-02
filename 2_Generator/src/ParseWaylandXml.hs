{-# LANGUAGE OverloadedStrings #-}
module ParseWaylandXml
where

-- TODO: Improve error handling. Report missing fields !!

import Types
import Casing

import qualified Text.XML.Light  as XML
import           Data.Text (Text)
import qualified Data.Text       as T
import qualified Data.Map.Strict as DM


parseWaylandXml :: String -> Maybe WlProtocol
parseWaylandXml fileData = do
    xmlDoc <- XML.parseXMLDoc fileData
    pName <- XML.findAttr (XML.QName "name" Nothing Nothing) xmlDoc
    ifaces <- mapM parseInterface $ xmlInterfaces xmlDoc
    pure $ WlProtocol { protName = snakeToPascal (T.pack pName), protInterfaces = ifaces}
    where
        xmlInterfaces :: XML.Element -> [XML.Element]
        xmlInterfaces = XML.findChildren (XML.QName "interface" Nothing Nothing)

parseInterface :: XML.Element -> Maybe WlInterface
parseInterface e = do
    iName <- XML.findAttr (XML.QName "name" Nothing Nothing) e
    requests <- mapM (parseRequest iName Request) $ xmlRequests e
    events <- mapM (parseRequest iName Event) $ xmlEvents e
    descr <- parseDescription e
    pure $ WlInterface  { ifaOid = 0
            , ifaWName = T.pack iName
            , ifaDescr = descr
            , ifaVersion = 0
            , ifaRequests = zipWith reqSetOpcode requests [0..]
            , ifaEvents = zipWith reqSetOpcode events [0..]
    }
    where
        xmlRequests :: XML.Element -> [XML.Element]
        xmlRequests = XML.findChildren (XML.QName "request" Nothing Nothing)
        xmlEvents :: XML.Element -> [XML.Element]
        xmlEvents = XML.findChildren (XML.QName "event" Nothing Nothing)

--         messageMap :: [WMessageDescription] -> WMessageMap
--         messageMap = IM.fromList . L.zip [0..] -- opcodes start from 0

parseRequest :: String -> RequestType -> XML.Element -> Maybe WlRequest
parseRequest iName rt e = do
    rName <-XML.findAttr (XML.QName "name" Nothing Nothing) e
    args <- mapM parseArgument $ xmlArguments e
    descr <- parseDescription e
    pure $ WlRequest { reqType = rt
        , reqOpc = 0
        , reqName = snakeToCamel $ T.pack (iName <> "_" <> rName)
        , reqDescr = descr
        , reqArgs = args
     }


reqSetOpcode :: WlRequest -> Int -> WlRequest
reqSetOpcode req opc = req{ reqOpc = opc}


xmlArguments :: XML.Element -> [XML.Element]
xmlArguments = XML.findChildren (XML.QName "arg" Nothing Nothing)

parseArgument :: XML.Element -> Maybe WlArgument
parseArgument e = do
    aName <- XML.findAttr (XML.QName "name" Nothing Nothing) e
    aType <- XML.findAttr (XML.QName "type" Nothing Nothing) e
    let aSum = XML.findAttr (XML.QName "summary" Nothing Nothing) e    -- Summary is Maybe !!

    pure $ WlArgument
        { argName = fixStopword $ snakeToCamel $ T.pack aName
        , argType = translateArgType $ T.pack aType
        , argSummary = T.pack <$> aSum
        }

parseDescription :: XML.Element -> Maybe WlDescription
parseDescription e = do
    xmldescr <-  XML.findChild (XML.QName "description" Nothing Nothing) e
    summary <- XML.findAttr (XML.QName "summary" Nothing Nothing) xmldescr
    pure $ WlDescription
        { descrSummary = T.pack summary
        , descrBody = T.pack $ XML.strContent xmldescr
        }

translateArgType :: Text -> Text
translateArgType typ = DM.findWithDefault "???" typ argumentMap

argumentMap :: DM.Map Text Text
argumentMap = DM.fromList [ ("int", "WInt"), ("uint", "WUint"),
                            ("fixed", "WFixed"), ("string", "WString"),
                            ("object", "WObj"), ("new_id", "WNewId"),
                            ("array", "WArray"), ("fd", "Fd") ]

fixStopword :: Text -> Text
fixStopword w =
    if w `elem` stopwords
        then T.cons 'x' w
    else w

stopwords :: [Text]
stopwords = ["class", "id"]
