module Server (
    startServer
) where

import Network.Socket

startServer :: PortNumber -> IO ()
startServer port = do
    sock <- socket AF_INET Stream 0
    setSocketOption sock ReuseAddr 1
    bind sock (SockAddrInet port iNADDR_ANY)
    listen sock 2
    serverLoop sock

serverLoop :: Socket -> IO ()
serverLoop sock = do
    conn <- accept sock
    handleConn conn
    serverLoop sock

handleConn :: (Socket, SockAddr) -> IO ()
handleConn (sock, _) = do
    send sock "Hello World!\n"
    close sock
