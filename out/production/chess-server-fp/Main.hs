module Main where

import Control.Concurrent
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
  roomJoinChannel <- newChan
  _ <- forkIO $
    fix $ \loop -> do
      _ <- readChan roomJoinChannel
      loop
  fix $ \loop -> do
    accepted <- accept sock
    _ <- forkIO $ process accepted roomJoinChannel
    loop

process :: (Socket, SockAddr) -> Chan Msg -> IO ()
process (accepted, _) channel = do
  headerFromClient <- timeout 3000000 (recv accepted (BS.length protoHeader))
  case headerFromClient of
    Just s ->
      if s /= protoHeader
        then do
          putStrLn ("Invalid header: " ++ show s)
          close accepted
        else do
          putStrLn "Connected"
          handleLobby accepted channel
          putStrLn "Disconnected"
    Nothing -> do
      putStrLn "REJECTED"
      close accepted

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
    charset = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789" :: [Char]

handleLobby :: Socket -> Chan Msg -> IO ()
handleLobby sock sendChannel = do
  recvChannel <- dupChan sendChannel
  command <- recv sock 1
  case BS.head command of
    -- PKG_CREATE_ROOM Create room
    0 -> do
      joinCode <- genJoinCode
      putStrLn joinCode
      _ <- sendByte sock 0 -- PKG_CLIENT_CODE
      _ <- send sock (BSU.fromString joinCode)
      -- Wait for join
      fix $ \waitForJoin -> do
        (code, clientSock, callback) <- readChan recvChannel
        if code == joinCode
          then do
            _ <- sendByte sock 1 -- PKG_CLIENT_ROOM_FULL
            -- Send joined command
            writeChan callback sock
            _ <- sendByte clientSock 6 -- PKG_CLIENT_JOINED
            _ <- sendByte clientSock 0 -- isWhite
            -- Run game
            handleGame sock clientSock
          else -- Continue waiting
            waitForJoin
    -- PKG_JOIN_ROOM Join room
    1 -> do
      joinCode <- recv sock 7
      callback <- newChan
      writeChan sendChannel (BSU.toString joinCode, sock, callback)
      otherSock <- readChan callback
      handleGame sock otherSock
    -- Invalid package
    _ -> return ()

handleGame :: Socket -> Socket -> IO ()
handleGame player1 player2 = fix $ \loop -> do
  command <- recv player1 1

  let transmit pkgType len = sendByte player2 pkgType >> recv player1 len >>= send player2 >> return ()
  let transmitChat = do
        _ <- sendByte player2 100
        fix $ \chatLoop -> do
          char <- recv player1 1
          if BS.head char /= 0
            then do
              _ <- send player2 char
              chatLoop
            else do
              _ <- sendByte player2 0
              return ()
        return ()

  if BS.length command == 0
    then return ()
    else do
      case BS.head command of
        2 -> transmit 2 4 -- MOVE(2), LEN(4)
        3 -> transmit 3 1 -- CASTLING(3), LEN(1)
        4 -> transmit 4 2 -- EN_PASSANT(4), LEN(2)
        5 -> transmit 5 3 -- PROMOTION(5), LEN(3)
        100 -> transmitChat
        _ -> return ()
      loop
