module Server (
    startServer
) where

import Network.Socket
import System.IO
import Control.Concurrent
import Data.List

type Event = String
type Msg = String
type User = String
type Channel = String

startServer :: PortNumber -> IO ()
startServer port = do
    sock <- socket AF_INET Stream 0
    setSocketOption sock ReuseAddr 1
    bind sock (SockAddrInet port iNADDR_ANY)
    listen sock 2
    chan <- newChan
    forkIO (mainServer chan [] [])
    acceptConnections sock chan

acceptConnections :: Socket -> Chan Event -> IO ()
acceptConnections sock chan = do
    conn <- accept sock
    forkIO (startConn conn chan)
    acceptConnections sock chan

mainServer :: Chan Event -> [User] -> [Channel] -> IO ()
mainServer chan users channels = do
    msg <- (readChan chan)
    putStrLn msg
    mainServer chan users channels

startConn :: (Socket, SockAddr) -> Chan Event -> IO ()
startConn (sock, _) chan = do
    hdl <- socketToHandle sock ReadWriteMode
    hSetBuffering hdl NoBuffering
    loopConn hdl chan
    hClose hdl

loopConn :: Handle -> Chan Event -> IO ()
loopConn hdl chan = do
    line <- hGetLine hdl
    writeChan chan line
    loopConn hdl chan
