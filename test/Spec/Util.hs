{-# LANGUAGE OverloadedStrings          #-}
module Spec.Util where

import qualified Data.ByteString          as BS
import           Control.Monad.STM
import           Control.Concurrent.STM.TVar
import           Control.Exception
import           Control.Monad

import           Test.Tasty.HUnit

import           Network.SSH.Encoding
import           Network.SSH.Stream
import qualified Network.SSH.TStreamingQueue as Q


assertThrows :: (Eq e, Exception e) => String -> e -> IO a -> Assertion
assertThrows label e action = (action >> failure0) `catch` \e'-> when (e /= e') (failure1 e')
    where
        failure0 = assertFailure (label ++ ": should have thrown " ++ show e)
        failure1 e' = assertFailure (label ++ ": should have thrown " ++ show e ++ " (saw " ++ show e' ++ " instead)")

data DummySocket = DummySocket Q.TStreamingQueue Q.TStreamingQueue

newSocketPair :: IO (DummySocket, DummySocket)
newSocketPair = atomically $ do
    window <- newTVar 1000000000
    x <- Q.newTStreamingQueue 1000000000 window
    y <- Q.newTStreamingQueue 1000000000 window
    pure (DummySocket x y, DummySocket y x)

instance DuplexStream DummySocket

instance OutputStream DummySocket where
    send (DummySocket q _) = send q

instance InputStream DummySocket where
    receive (DummySocket _ q) = receive q

close :: DummySocket -> IO ()
close (DummySocket q _) = atomically $ Q.terminate q

receivePlainMessage :: Encoding msg => DummySocket -> IO msg
receivePlainMessage sock = do
    bs0 <- receiveAll sock 4
    let size = BS.foldl (\acc w8-> acc * 256 + fromIntegral w8) 0 bs0 :: Int
    bs1 <- receiveAll sock size
    let padding = fromIntegral (BS.index bs1 0)
    assertBool "4 <= len padding <= 255" (4 <= padding && padding <= 255)
    case runGet get (BS.take (size - 1 - padding) $ BS.drop 1 bs1) of
        Nothing -> assertFailure "parser error"
        Just m  -> pure m

sendPlainMessage :: Encoding msg => DummySocket -> msg -> IO ()
sendPlainMessage sock msg = do
    void $ sendAll sock $ runPut (putPacked $ runPut $ put msg)

