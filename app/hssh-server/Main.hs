{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Debug.Trace

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
import           Data.ByteString.Char8         as C8 hiding (putStrLn, unsnoc, snoc, map, filter)
import           Data.Default
import           System.Exit

import qualified System.Socket                  as S
import qualified System.Socket.Family.Inet6     as S
import qualified System.Socket.Protocol.Default as S
import qualified System.Socket.Type.Stream      as S
import qualified System.Socket.Unsafe           as S

import           Network.SSH
import qualified Network.SSH.Server            as Server
import           Data.Text                     as T hiding (map, filter)
import           Data.Text.Encoding            as TE

main :: IO ()
main = do
    _ <- putStrLn "start"
    keyPair <- newKeyPair
    _ <- putStrLn "before runServer"
    Server.runServer (config) keyPair
  where
    config = def
        { Server.socketConfig             = def { Server.socketBindAddresses = pure (Address "*" 2023)}
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
        , Server.onConnect                = \_ -> do
            pure (Just ())
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

handleSessionRequest :: (Show state, Show user) => state -> user -> IO (Maybe Server.SessionHandler)
handleSessionRequest state user = pure $ Just $ Server.SessionHandler $ mySessionHandler state user BS.empty
   
mySessionHandler :: (Show state, Show user, InputStream stdin, OutputStream stdout, OutputStream stderr) => state -> user -> BS.ByteString -> Environment -> Maybe TermInfo -> Maybe Command -> stdin -> stdout -> stderr -> IO ExitCode     
mySessionHandler state user previousCommandBytes a b c stdin stdout d = do
    p <- receive stdin 1024
    let currentCommandBytes = (BS.append previousCommandBytes p)
    sendAll stdout p
    if lastIsCarriageReturn $ TE.decodeUtf8 p
      then do
        sendAll stdout $ C8.pack . createResponseFromCommand . C8.unpack $ currentCommandBytes
        mySessionHandler state user BS.empty a b c stdin stdout d 
      else do
        mySessionHandler state user currentCommandBytes a b c stdin stdout d 

lastIsCarriageReturn :: T.Text -> Bool
lastIsCarriageReturn t = case unsnoc t of
  Just (_,l) -> l == '\r'
  Nothing -> False

handleBSCommand :: BS.ByteString -> BS.ByteString
handleBSCommand bs = bs

createResponseFromCommand :: String -> String
createResponseFromCommand text = Prelude.concat $
  [ 
    "\n"
  , "\r"
  , createResponseContent $ text
  , "\n"
  , "\r"
  ]

createResponseContent :: String -> String
createResponseContent userCommand = case isUserCommand (stripAll userCommand) of
  Just LS -> "Documents Projects Desktop"
  Just CD -> "new dir"
  Nothing -> "bad command"

data UserCommand = LS | CD

isUserCommand :: String -> Maybe UserCommand
isUserCommand "ls" = Just LS
isUserCommand "cd" = Just CD
isUserCommand _    = Nothing

stripAll :: String -> String
stripAll s = filter (\char -> char /= '\n' && char /= '\r') s

{- TODO
 - ctrl-C to exit
 - strip whitespace before command
 - add '#' or whatever before user types
 - make game
 - figure out how to allow the play the game from the ssh session
 -}

--createResponseContent :: T.Text -> T.Text
--createResponseContent text = T.pack $ 
     

-- T.pack :: String -> Text
-- T.unpack :: Text -> String
-- C8.pack :: String -> ByteString
-- C8.unpack :: ByteString -> String
-- TE.encodeUtf8 :: Text -> ByteString
-- TE.decodeUtf8 :: ByteString -> Text
