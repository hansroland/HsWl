module Main where

import ParseWaylandXml
import GenerateHaskell
import Types ( WlProtocol(..) )

import qualified System.IO                  as IO
import qualified Data.Text.IO               as TIO

import Data.Maybe

main :: IO ()
main = do
    -- TODO Read filenames from the main parameters
    -- TODO Read from installed XML files in /usr/share/wayland-protocols/stable/
    -- TODO Add check for empty input
    prots <- mapM readXmlFile ["wayland", "xdg-shell" ]
    oks <- checkRead prots
    TIO.writeFile "Wire.hs" $ generateHaskell $ concatProts oks

-- Read a single XML file
readXmlFile:: String -> IO (Maybe WlProtocol)
readXmlFile fn = do
    h <- IO.openFile (fn <> ".xml") IO.ReadMode
    -- set encoding, because Wayland XML files are UTF8
    IO.hSetEncoding h IO.utf8
    d <- IO.hGetContents h
    pure $ parseWaylandXml d


checkRead :: [Maybe WlProtocol] -> IO [WlProtocol]
checkRead prots = do
    if any isNothing prots
    -- TODO use Either instead of Maybe
    then do
      putStrLn "Error on parsing xml file"
      pure []
    else do
      pure $ catMaybes prots

-- Join the different protocols to one single
concatProts :: [WlProtocol] -> WlProtocol
concatProts prots =
  WlProtocol (protName (head prots)) $ concatMap protInterfaces prots
