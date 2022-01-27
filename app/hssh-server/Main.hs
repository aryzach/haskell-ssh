{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Concurrent             ( threadDelay )
import           Control.Concurrent.Async
import           Control.Exception              ( bracket
                                                , bracketOnError
                                                , handle
                                                , throwIO
                                                )
import           Control.Monad                  ( forever
                                                , void
                                                )
import qualified Data.ByteArray                as BA
import qualified Data.ByteString               as BS
import           Data.Default
import           System.Exit

import qualified System.Socket                  as S
import qualified System.Socket.Family.Inet6     as S
import qualified System.Socket.Protocol.Default as S
import qualified System.Socket.Type.Stream      as S
import qualified System.Socket.Unsafe           as S

import           Network.SSH
import qualified Network.SSH.Server            as Server

main :: IO ()
main = do
    _ <- putStrLn "start"
    keyPair <- newKeyPair
    _ <- putStrLn "before runServer"
    Server.runServer (config) keyPair
  where
    config = def
        { Server.socketConfig             = def { Server.socketBindAddresses = pure (Address "*" 2024)}
        , Server.transportConfig          = def {
                onSend = \x -> putStrLn ("CLIENT: " ++ show x),
                onReceive = \x -> putStrLn ("SERVER: " ++ show x)
            }
        , Server.userAuthConfig           = def
            { Server.onAuthRequest        = \_ addr username _ _ -> pure (Just username)
            }
        , Server.connectionConfig         = def
            { Server.onSessionRequest     = handleSessionRequest
            , Server.onDirectTcpIpRequest = handleDirectTcpIpRequest
            }
        , Server.onConnect                = \ha -> do
            print ha
            pure (Just ())
        , Server.onDisconnect             = \ha st user d -> do
            print ha
            print st
            print user
            print d
        }

handleDirectTcpIpRequest :: state -> user -> SourceAddress -> DestinationAddress -> IO (Maybe Server.DirectTcpIpHandler)
handleDirectTcpIpRequest state user src dst = pure $ Just $ Server.DirectTcpIpHandler $ \stream-> do
    bs <- receive stream 4096
    sendAll stream "HTTP/1.1 200 OK\n"
    sendAll stream "Content-Type: text/plain\n\n"
    sendAll stream "Hello world!\n"
    sendAll stream "\n\n"
    sendAll stream bs
    print bs

handleSessionRequest :: state -> user -> IO (Maybe Server.SessionHandler)
handleSessionRequest state user = pure $ Just $ Server.SessionHandler $ mySessionHandler state user
   

mySessionHandler :: (InputStream stdin, OutputStream stdout, OutputStream stderr) => state -> user -> Environment -> Maybe TermInfo -> Maybe Command -> stdin -> stdout -> stderr -> IO ExitCode     
mySessionHandler state user a b c stdin stdout d = do
    s <- receive stdin 10
    if s == BS.empty
      then pure ExitSuccess
    else do
      sendAll stdout "Hello world!\n"
      sendAll stdout s
      sendAll stdout s
      mySessionHandler state user a b c stdin stdout d

 


