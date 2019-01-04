module Server (
    startServer
) where

import Network.Socket
import System.IO
import Control.Concurrent
import Data.List

import Client
import User

data Event = ClientMsg Msg
    deriving Show

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
    event <- (readChan chan)
    putStrLn $ show event
    mainServer chan users channels

startConn :: (Socket, SockAddr) -> Chan Event -> IO ()
startConn (sock, _) chan = do
    hdl <- socketToHandle sock ReadWriteMode
    hSetBuffering hdl NoBuffering
    loopConn hdl chan NoUser
    hClose hdl

loopConn :: Handle -> Chan Event -> User -> IO ()
loopConn hdl chan user = do
    line <- hGetLine hdl
    writeChan chan $ ClientMsg $ parseMsg $ init line
    loopConn hdl chan user
