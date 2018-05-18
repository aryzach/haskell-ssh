{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Concurrent             (forkIO, threadDelay)
import           Control.Concurrent.MVar
import           Control.Exception              (SomeException, bracket, catch,
                                                 finally)
import           Control.Monad                  (forever)
import           Control.Monad.STM
import qualified Data.ByteArray                 as BA
import qualified Data.ByteString                as BS
import qualified Data.Text                      as T
import qualified Data.Text.Encoding             as T
import           System.Exit
import qualified System.Socket                  as S
import qualified System.Socket.Family.Inet6     as S
import qualified System.Socket.Protocol.Default as S
import qualified System.Socket.Type.Stream      as S

import           Control.Monad.Replique
import           Control.Monad.Terminal

import           Network.SSH.Constants
import           Network.SSH.DuplexStream
import           Network.SSH.Key
import qualified Network.SSH.Server             as Server
import qualified Network.SSH.Server.Config      as Server

main :: IO ()
main = do
    print version
    file <- BS.readFile "./resources/id_ed25519"
    (privateKey, _):_ <- decodePrivateKeyFile BS.empty file :: IO [(PrivateKey, BA.Bytes)]

    c <- Server.newDefaultConfig
    let config = c {
            Server.hostKey        = privateKey
        ,   Server.onShellRequest = Just runShell
        }
    bracket open close (accept config)
  where
    open        = S.socket :: IO (S.Socket S.Inet6 S.Stream S.Default)
    close       = S.close
    send    s x = S.sendAll s x S.msgNoSignal >> pure ()
    receive s i = S.receive s i S.msgNoSignal
    accept config s = do
      S.setSocketOption s (S.ReuseAddress True)
      S.setSocketOption s (S.V6Only False)
      S.bind s (S.SocketAddressInet6 S.inet6Any 22 0 0)
      S.listen s 5
      token <- newEmptyMVar
      forever $ do
        forkIO $ bracket
          (S.accept s `finally` putMVar token ())
          (S.close . fst)
          (\(stream,_)-> Server.serve config stream)
        takeMVar token

runShell :: Terminal -> IO ExitCode
runShell term = do
    runTerminalT (runRepliqueT repl 0) term
    pure (ExitFailure 1)

repl :: RepliqueT Int (TerminalT IO) ()
repl = readLine "ssh % " >>= \case
    ""           -> pure ()
    "quit"       -> quit
    "fail"       -> fail "Failure is not an option."
    line         -> putStringLn (show (line :: String))

instance DuplexStream (S.Socket f S.Stream p) where
    send stream bytes = S.send stream (BA.convert bytes) S.msgNoSignal
    receive stream len = BA.convert <$> S.receive stream len S.msgNoSignal
