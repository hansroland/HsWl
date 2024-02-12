{-# LANGUAGE OverloadedStrings #-}

module ClientSupport
where

import Protocol
import Types
import Socket

import Data.Binary.Get

import qualified Data.ByteString            as BS
import qualified Data.ByteString.Lazy       as BL
import qualified System.Environment         as E
import qualified System.Exit                as Exit
import qualified Control.Monad              as M
import qualified System.Posix.IO            as PI
import qualified System.Posix.Types         as PT
import qualified System.Posix.User          as PU
import qualified System.Posix.Signals       as Signals
import qualified Network.Socket             as Socket
import qualified Control.Concurrent.STM     as STM
import qualified System.IO.Error            as Err
import           System.Posix.IO

import Control.Monad.State.Strict

-- --------------------------------------------------------------------

splitMsgs :: BL.ByteString -> [BL.ByteString]
splitMsgs inp =
    if BL.length inp >= 8
        then let (msg, rest) = split1Msg inp
             in msg : splitMsgs rest
        else []
  where
    split1Msg :: BL.ByteString -> (BL.ByteString, BL.ByteString)
    split1Msg bs =
    -- TODO take 2 strict bytes and convert them to lazy
    -- do not convert the whole end string to lazy
        let (_start, end) = BL.splitAt 6 bs
            len = fromIntegral $ runGet getWord16host end
        in BL.splitAt len bs

parseWInpMsg :: BL.ByteString -> WInputMsg
parseWInpMsg = runGet go
  where
    go ::  Get WInputMsg
    go  = do
        obj <- fromIntegral <$> getWord32host
        opc <- fromIntegral <$> getWord16host
        len <- fromIntegral <$> getWord16host
        let newLen = len - 8
        bs  <- getByteString newLen
        pure $ WInputMsg obj opc len bs

data SysEvent = ServerClosedSocket
           | ProcessingEnded
           | SigChld
           | SigInt
    deriving (Show, Eq)

connectToServer :: (Socket.Socket -> ClMonad ()) -> IO ()
connectToServer clientFun = do

    -- loggerChan <- STM.newTChanIO
    eventV <- STM.newEmptyTMVarIO

    _ <- Signals.installHandler Signals.sigINT (Signals.Catch $ sigHandler Signals.sigINT eventV) Nothing

    xdgDir <- Err.catchIOError (E.getEnv "XDG_RUNTIME_DIR") createXdgPath
    serverName <- Err.catchIOError (E.getEnv "WAYLAND_DISPLAY") (\_ -> return "wayland-0")
    serverSock <- Socket.socket Socket.AF_UNIX Socket.Stream Socket.defaultProtocol
    serverSockFd <- Socket.unsafeFdSocket serverSock
    PI.setFdOption (PT.Fd serverSockFd) PI.CloseOnExec True
    let serverPath = xdgDir ++ "/" ++ serverName
    putStrLn $ "connecting to " ++ serverPath
    Socket.connect serverSock (Socket.SockAddrUnix serverPath)

    evalStateT (clientFun serverSock) initClState

    _ <- M.forever $ do
        e <- STM.atomically $ STM.takeTMVar eventV

        case e of
            SigInt -> do
                putStrLn "sigINT received"
                -- wait until the logger thread finishes
                -- STM.atomically $ STM.writeTChan loggerChan Nothing
                Exit.exitSuccess
            SigChld -> do
                putStrLn "sigCHLD received"
                -- STM.atomically $ STM.writeTChan loggerChan Nothing
            ServerClosedSocket -> do
                putStrLn "server closed socket"
                -- Signals.signalProcess Signals.sigINT pid
                -- STM.atomically $ STM.writeTChan loggerChan Nothing
            ProcessingEnded -> do
                putStrLn "exiting"
                -- Socket.close clientSock
                Socket.close serverSock

                -- finally exit when the logger thread is done
                Exit.exitSuccess

    putStrLn "ConnectToSocket"

createXdgPath :: a -> IO String
createXdgPath _ = do
    userid <- PU.getRealUserID
    pure $ "/var/run/" ++ show userid

sigHandler :: Signals.Signal -> STM.TMVar SysEvent -> IO ()
sigHandler sig var = do
    let e = if sig == Signals.sigINT
        then SigInt
        else SigChld
    STM.atomically $ STM.putTMVar var e


socketRead :: Socket.Socket -> ClMonad ()
socketRead serverSock = do
    bs <- liftIO $ socketReceive serverSock
    -- TODO: Clean up lazy and strict bytestring : convert to lazy after socket read!!
    let bsl = BL.fromStrict bs
    let blocks = splitMsgs bsl
    -- liftIO $ mapM_ (putStrLn . toHexString) blocks
    let msgs = map parseWInpMsg blocks
    -- liftIO $ mapM_ print msgs
    mapM_ dispatchEvent msgs
    --
    -- socketLoop serverSock

-- | sendRequests - send all requests from the request list
sendRequests :: Socket.Socket -> ClMonad ()
sendRequests serverSock = do
    _ <- liftIO $ putStrLn "sendRequest"
    fds  <- collectFds
    reqs <- collectRequests
    _ <- liftIO $ Socket.withFdSocket serverSock (socketSend reqs fds )
    liftIO $ mapM_ closeFd fds
    st <- get
    -- liftIO $ printActiveIfaces (clActiveIfaces st)
    put st {clReqs = [], clFds = []}
  where
    collectFds :: ClMonad [PT.Fd]
    collectFds = do reverse . clFds <$> get
    collectRequests :: ClMonad BS.ByteString
    collectRequests = do mconcat . reverse . clReqs <$> get




