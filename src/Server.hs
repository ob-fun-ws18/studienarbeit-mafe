module Server (
    startServer
) where

import Network.Socket
import System.IO
import Control.Concurrent
import Data.List

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
    forkIO (handleConn conn)
    serverLoop sock

handleConn :: (Socket, SockAddr) -> IO ()
handleConn (sock, _) = do
    hdl <- socketToHandle sock ReadWriteMode
    hSetBuffering hdl NoBuffering
    getUser hdl ""
    hClose hdl

getUser :: Handle -> String -> IO ()
getUser hdl nick = do
    line <- hGetLine hdl
    if isPrefixOf "NICK" line then
        let Just name = stripPrefix "NICK " line
        in getUser hdl name
    else if isPrefixOf "USER" line then do
        hPutStrLn hdl (":bar.expample.com 001 " ++ nick ++ " :Welcome to the Internet Relay Network " ++ nick ++ "!" ++ nick ++ "@foo.example.com")
        getUser hdl nick
        else do
            hPutStrLn hdl (":bar.expample.com PRIVMSG " ++ nick ++ " :" ++ line)
            getUser hdl nick
