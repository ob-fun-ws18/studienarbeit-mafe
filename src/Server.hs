module Server (
    startServer
) where

import Network.Socket
import System.IO
import Control.Concurrent
import Data.List
import Data.Maybe
import qualified Data.Map as Map
import Data.Map((!))
import Control.Monad
import Data.Foldable

import Client
import User

data Event = ClientMsg User Msg
        | NewUser User Handle
    deriving Show

type Channel = String

type Users = (Map.Map String Handle)
type Channels = (Map.Map String [String])

startServer :: PortNumber -> IO ()
startServer port = do
    sock <- socket AF_INET Stream 0
    setSocketOption sock ReuseAddr 1
    bind sock (SockAddrInet port iNADDR_ANY)
    listen sock 2
    chan <- newChan
    forkIO (mainServer chan Map.empty Map.empty)
    acceptConnections sock chan

acceptConnections :: Socket -> Chan Event -> IO ()
acceptConnections sock chan = do
    conn <- accept sock
    forkIO (startConn conn chan)
    acceptConnections sock chan

mainServer :: Chan Event -> Users -> Channels -> IO ()
mainServer chan users channels = do
    event <- readChan chan
    print event
    (new_users, new_channels) <- handleEvent users channels event
    mainServer chan new_users new_channels

handleEvent :: Users -> Channels -> Event -> IO (Users, Channels)
handleEvent users channels (NewUser user@(FullUser nick username name) hdl) = do
    let new_users = Map.insert nick hdl users
    hPutStrLn hdl (":localhost 001 " ++ nick ++ " :Welcome to the Internet Relay Network " ++ toString user)
    return (new_users, channels)
handleEvent users channels (ClientMsg user@(FullUser nick _ _) (Ping server)) = do
    sendToUser users nick "PONG localhost"
    return (users, channels)
handleEvent users channels (ClientMsg user@(FullUser nick _ _) msg@(PrivMsg receiver _)) = do
    if head receiver == '#'
        then do
            let receivers = filter (nick /=) (Map.findWithDefault [] receiver channels)
            sendToAllUsers users receivers (buildMsg user msg)
        else sendToUser users receiver $ buildMsg user msg
    return (users, channels)
handleEvent users channels (ClientMsg user@(FullUser nick _ _) msg@(Join channel)) = do
    let members = nick : Map.findWithDefault [] channel channels
        new_channels = Map.insert channel members channels
    sendToAllUsers users members $ buildMsg user msg
    return (users, new_channels)
handleEvent users channels (ClientMsg _ (JoinMulti [])) = return (users, channels)
handleEvent users channels (ClientMsg user msg@(JoinMulti (c : cs))) = do
    (new_users, new_channels) <- handleEvent users channels (ClientMsg user (Join c))
    handleEvent new_users new_channels (ClientMsg user (JoinMulti cs))
handleEvent users channels (ClientMsg user@(FullUser nick _ _) msg@(Part channel _)) = do
    let members = Map.findWithDefault [] channel channels
    sendToAllUsers users members $ buildMsg user msg
    let new_channels = Map.insert channel (filter (nick /=) members) channels
    return (users, new_channels)
handleEvent users channels (ClientMsg user@(FullUser nick _ _) msg@(Quit _)) = do
    let hdl = users ! nick
        new_users = Map.delete nick users
        new_channels = Map.map (filter (nick /=)) channels
    hClose hdl
    sendToAllUsers users (Map.keys new_users) (buildMsg user msg)
    return (new_users , new_channels)
handleEvent users channels (ClientMsg user@(FullUser nick _ _) msg@(IsOn nicks)) = do
    let usersOn = filter (`Map.member` users) nicks
    sendToUser users nick $ buildMsg user (IsOn usersOn)
    return (users, channels)
handleEvent users channels _ = return (users, channels)

sendToUser :: Users -> String -> String -> IO ()
sendToUser users nick = hPutStrLn (users ! nick)

sendToAllUsers :: Users -> [String] -> String -> IO ()
sendToAllUsers _ [] _ = return ()
sendToAllUsers users receivers msg = do
    sendToUser users (head receivers) msg
    sendToAllUsers users (tail receivers) msg

startConn :: (Socket, SockAddr) -> Chan Event -> IO ()
startConn (sock, _) chan = do
    hdl <- socketToHandle sock ReadWriteMode
    hSetBuffering hdl NoBuffering
    loopConn hdl chan NoUser

loopConn :: Handle -> Chan Event -> User -> IO ()
loopConn hdl chan user = do
    eof <- hIsEOF hdl
    if eof then
        writeChan chan $ ClientMsg user $ Quit "Client lost connection."
    else do
        line <- hGetLine hdl
        let msg = parseMsg $ init line
            (new_user, event) = handleMsg hdl user msg
        putStrLn ("\n" ++ line)
        print msg
        Data.Foldable.forM_ event (writeChan chan)
        unless (isFinalMsg msg) (loopConn hdl chan new_user)

isFinalMsg :: Msg -> Bool
isFinalMsg (Quit _) = True
isFinalMsg _ = False

handleMsg :: Handle -> User -> Msg -> (User, Maybe Event)
handleMsg _ NoUser (Nick nick) = (NickUser nick, Nothing)
handleMsg hdl (NickUser nick) (User username name) = (user, Just (NewUser user hdl))
        where user = FullUser nick username name
handleMsg _ user msg = (user, Just (ClientMsg user msg))

