module Main where

import Control.Concurrent
import Control.Monad (void, when)
import Control.Monad.Fix (fix)
import qualified Data.ByteString as BS (ByteString, head, length)
import Data.ByteString.UTF8 as BSU (fromString, toString)
import Data.Char (chr)
import Network.Socket
import Network.Socket.ByteString (recv, send)
import System.IO
import System.Random
import System.Timeout (timeout)

protoHeader :: BS.ByteString
protoHeader = BSU.fromString "CHESS_PROTO/1.0"

type Msg = (String, Socket, Chan Socket)

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  sock <- socket AF_INET Stream 0
  setSocketOption sock ReuseAddr 1
  bind sock (SockAddrInet 8081 0)
  listen sock 4000
  putStrLn "Server started"
  roomJoinChannel <- newChan
  logChannel <- newChan

  _ <- forkIO $
    fix $ \loop ->
      readChan logChannel >>= putStrLn >> loop

  _ <- forkIO $
    fix $ \loop -> readChan roomJoinChannel >> loop

  fix $ \loop ->
    accept sock
      >>= ( \accepted ->
              forkIO $ process accepted roomJoinChannel logChannel
          )
      >> loop

process :: (Socket, SockAddr) -> Chan Msg -> Chan String -> IO ()
process (accepted, address) channel logChannel = do
  headerFromClient <- timeout 3000000 (recv accepted (BS.length protoHeader))
  case headerFromClient of
    Just s ->
      if s /= protoHeader
        then do
          logMsg ("Invalid header: " ++ show s)
          close accepted
        else do
          logMsg "Connected"
          handleLobby accepted channel logMsg
          logMsg "Disconnected"
          close accepted
    Nothing -> do
      logMsg "REJECTED"
      close accepted
  where
    logMsg :: String -> IO ()
    logMsg msg = writeChan logChannel ("[" ++ show address ++ "] " ++ msg)

sendByte :: Socket -> Int -> IO Int
sendByte sock a = send sock (BSU.fromString [chr a])

genJoinCode :: IO [Char]
genJoinCode = genJoinCodeExec ""

genJoinCodeExec :: [Char] -> IO [Char]
genJoinCodeExec generated = case len of
  3 -> genJoinCodeExec (generated ++ "-")
  7 -> return generated
  _ -> do
    index <- randomRIO (0 :: Int, length charset - 1)
    genJoinCodeExec (generated ++ [charset !! index])
  where
    len = length generated
    charset = ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9'] :: [Char]

handleLobby :: Socket -> Chan Msg -> (String -> IO ()) -> IO ()
handleLobby sock sendChannel logMsg = do
  recvChannel <- dupChan sendChannel
  command <- recv sock 1
  when (BS.length command /= 0) $
    case BS.head command of
      -- PKG_CREATE_ROOM Create room
      0 -> do
        joinCode <- genJoinCode
        logMsg ("Room '" ++ joinCode ++ "' created")
        _ <- sendByte sock 0 -- PKG_CLIENT_CODE
        _ <- send sock (BSU.fromString joinCode)
        -- Wait for join
        clientSockChan <- newChan
        chanFilterThread <- forkIO $
          fix $ \waitForJoin -> do
            (code, clientSock, callback) <- readChan recvChannel
            if code == joinCode
              then do
                writeChan callback sock
                writeChan clientSockChan clientSock
              else waitForJoin

        clientSockMaybe <- timeout (60 * 1000000) (readChan clientSockChan)
        case clientSockMaybe of
          Just clientSock -> do
            _ <- sendByte sock 1 -- PKG_CLIENT_ROOM_FULL
            -- Send joined command
            _ <- sendByte clientSock 6 -- PKG_CLIENT_JOINED
            _ <- sendByte clientSock 0 -- isWhite
            -- Run game
            handleGame sock clientSock (roomLogMsgProvider joinCode)
          Nothing -> do
            killThread chanFilterThread
            logMsg "Nobody joined to the room during 1 minute"
      -- PKG_JOIN_ROOM Join room
      1 -> do
        joinCode <- recv sock 7
        callback <- newChan
        writeChan sendChannel (BSU.toString joinCode, sock, callback)
        otherSockMaybe <- timeout 2000000 (readChan callback)
        case otherSockMaybe of
          Just otherSock -> do
            logMsg ("Joined to the room with code '" ++ BSU.toString joinCode ++ "'")
            handleGame sock otherSock (roomLogMsgProvider (BSU.toString joinCode))
          Nothing -> do
            logMsg "Can't join room during 2s"
      -- Invalid package
      _ -> return ()
  where
    roomLogMsgProvider roomCode =
      \message -> logMsg ("[" ++ roomCode ++ "] " ++ message)

handleGame :: Socket -> Socket -> (String -> IO ()) -> IO ()
handleGame player1 player2 logMsg = fix $ \loop -> do
  command <- recv player1 1

  let transmit pkgType len = sendByte player2 pkgType >> recv player1 len >>= (void . send player2)
  let ping = sendByte player2 7 >> logMsg "ping"
  let pong = sendByte player2 8 >> logMsg "pong"
  let transmitChat =
        sendByte player2 100
          >> fix
            ( \chatLoop ->
                recv player1 1 >>= \char ->
                  if BS.length char /= 0 && BS.head char /= 0
                    then send player2 char >> chatLoop
                    else void $ sendByte player2 0
            )

  when (BS.length command /= 0) $ do
    case BS.head command of
      2 -> transmit 2 4 -- MOVE(2), LEN(4)
      3 -> transmit 3 1 -- CASTLING(3), LEN(1)
      4 -> transmit 4 2 -- EN_PASSANT(4), LEN(2)
      5 -> transmit 5 3 -- PROMOTION(5), LEN(3)
      7 -> ping -- PING(7), LEN(0)
      8 -> pong -- PONG(8), LEN(0)
      100 -> transmitChat
      _ -> return ()
    loop
