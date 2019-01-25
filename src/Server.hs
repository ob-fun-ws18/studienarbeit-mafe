{-|
Module: Server
Description: Connection handling

This module contains functions for the handling of connections between client
and server.
-}
module Server (
      Event(..)
    , Users
    , Channels
    , startServer
    , acceptConnections
    , mainServer
    , handleEvent
    , sendToUser
    , sendToAllUsers
    , startConn
    , loopConn
    , isFinalMsg
    , handleMsg
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

-- |An event that should be handled by the main server
data Event = ClientMsg User Msg -- ^A parsed message from a client
        | NewUser User Handle   -- ^A new user can bea reached via the given handle
    deriving (Show, Eq)

-- |This type maps from the nicknames to the handles to the client.
type Users = (Map.Map String Handle)
-- |This type maps channel names to the nicknames of the users in the channel.
type Channels = (Map.Map String [String])

-- |This function sets up the listening socket, starts the main server, and starts
-- a loop for accepting connections.
startServer :: PortNumber -- ^ The port that should accept new connections
            -> IO ()
startServer port = do
    sock <- socket AF_INET Stream 0
    setSocketOption sock ReuseAddr 1
    bind sock (SockAddrInet port iNADDR_ANY)
    listen sock 2
    chan <- newChan
    forkIO (mainServer chan Map.empty Map.empty)
    acceptConnections sock chan

-- |This function accepts new connections from clients and starts the communication
-- with them in a new thread.
acceptConnections :: Socket     -- ^The listening socket
                  -> Chan Event -- ^The channel to communicate with the main server
                  -> IO ()
acceptConnections sock chan = do
    conn <- accept sock
    forkIO (startConn conn chan)
    acceptConnections sock chan

-- |The main server waits for events that the clients generate. It also keeps track
-- of the connected users and the channels. It modifies the states and sends replys
-- to the clients.
mainServer :: Chan Event -- ^The channels to receive events from the clients
           -> Users      -- ^The currently connected users with their handles
           -> Channels   -- ^The currently available channels with their users
           -> IO ()
mainServer chan users channels = do
    event <- readChan chan
    print event
    (new_users, new_channels) <- handleEvent users channels event
    mainServer chan new_users new_channels

-- |This function contains the main logic of how to react to the different events.
-- It processes the event, alters the users and the channel information where neccessary
-- and sends responses to the clients.
handleEvent :: Users                -- ^The current users
            -> Channels             -- ^The current channel information
            -> Event                -- ^The event received from the client
            -> IO (Users, Channels) -- ^It returns the new state of users and channels
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

-- |This function sends a message to a specified user.
sendToUser :: Users  -- ^The map of users to look up the correct handle
           -> String -- ^The nickname of the user to send to
           -> String -- ^The message to send
           -> IO ()
sendToUser users nick = hPutStrLn (users ! nick)

-- |This function sends the same message to many users.
sendToAllUsers :: Users    -- ^The map of users to look up the correct handles
               -> [String] -- ^The nicknames of the users to send to
               -> String   -- ^The message to send
               -> IO ()
sendToAllUsers _ [] _ = return ()
sendToAllUsers users receivers msg = do
    sendToUser users (head receivers) msg
    sendToAllUsers users (tail receivers) msg

-- |This function sets up the connection to a client and starts the loop that handles
-- the further communication.
startConn :: (Socket, SockAddr) -- ^The socket information for the client
          -> Chan Event         -- ^The channel to communictate with the main server
          -> IO ()
startConn (sock, _) chan = do
    hdl <- socketToHandle sock ReadWriteMode
    hSetBuffering hdl NoBuffering
    loopConn hdl chan NoUser

-- |This function handles the communication from the client to the server. It reads
-- the messages, parses them, and acts accordingly.
loopConn :: Handle     -- ^The handle to receive data from the client
         -> Chan Event -- ^The channel to communicate with the main server
         -> User       -- ^The data of the user that is handled by this connection
         -> IO ()
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

