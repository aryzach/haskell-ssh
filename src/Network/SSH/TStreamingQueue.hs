{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiWayIf        #-}
module Network.SSH.TStreamingQueue where

import           Control.Concurrent.STM.TChan
import           Control.Concurrent.STM.TVar
import           Control.Concurrent.STM.TMVar
import           Control.Monad.STM
import           Control.Applicative
import           Data.Word
import qualified Data.ByteString               as BS
import           Prelude                 hiding ( head
                                                , tail
                                                )

import qualified Network.SSH.Stream            as S
import           Network.SSH.Constants

data TStreamingQueue
    = TStreamingQueue
    { qCapacity  :: Word32
    , qWindow    :: TVar Word32
    , qSize      :: TVar Word32
    , qHead      :: TMVar BS.ByteString
    , qTail      :: TChan BS.ByteString
    }

newTStreamingQueue :: Word32 -> TVar Word32 -> STM TStreamingQueue
newTStreamingQueue c window =
    TStreamingQueue c window <$> newTVar 0 <*> newEmptyTMVar <*> newTChan

capacity :: TStreamingQueue -> Word32
capacity = qCapacity

getSize :: TStreamingQueue -> STM Word32
getSize = readTVar . qSize

getWindowSpace :: TStreamingQueue -> STM Word32
getWindowSpace = readTVar . qWindow

addWindowSpace :: TStreamingQueue -> Word32 -> STM ()
addWindowSpace q increment = do
    wndw <- getWindowSpace q :: STM Word32
    check $ (fromIntegral wndw + fromIntegral increment :: Word64) <= fromIntegral (maxBound :: Word32)
    writeTVar (qWindow q) $! wndw + increment

askWindowSpaceAdjustRecommended :: TStreamingQueue -> STM Bool
askWindowSpaceAdjustRecommended q = do
    size <- getSize q
    wndw <- getWindowSpace q
    let threshold = capacity q `div` 2
    -- 1st condition: window size must be below half of its maximum
    -- 2nd condition: queue size must be below half of its capacity
    -- in order to avoid byte-wise adjustment and flapping
    pure $ size < threshold && wndw < threshold 

fillWindowSpace :: TStreamingQueue -> STM Word32
fillWindowSpace q = do
    wndw <- getWindowSpace q
    if capacity q > wndw
        then writeTVar (qWindow q) (capacity q) >> pure (capacity q - wndw)
        else pure 0

enqueue :: TStreamingQueue -> BS.ByteString -> STM Word32
enqueue q bs
    | BS.null bs = pure 0
    | otherwise = do
        size <- getSize q
        wndw <- getWindowSpace q
        let free       = capacity q - size
            requested  = fromIntegral (BS.length bs) :: Word32
            available  = min (min free wndw) maxBoundIntWord32 :: Word32
        check $ available > 0 -- Block until there's free capacity and window space.
        if  | available >= requested -> do
                writeTVar (qSize q)   $! size + requested
                writeTVar (qWindow q) $! wndw - requested
                writeTChan (qTail q) bs
                pure requested
            | otherwise -> do
                writeTVar (qSize q)   $! size + available
                writeTVar (qWindow q) $! wndw - available
                writeTChan (qTail q)  $! BS.take (fromIntegral available) bs
                pure available

dequeue :: TStreamingQueue -> Word32 -> STM BS.ByteString
dequeue q i
    | i < 1     = pure mempty
    | otherwise = do
        size <- getSize q
        check $ size > 0 -- Block until there's at least 1 byte available.
        bs <- takeTMVar (qHead q) <|> readTChan (qTail q)
        let requested = min i maxBoundIntWord32
            available = fromIntegral (BS.length bs)
        if  | available > requested -> do
                writeTVar (qSize q) $! size - requested
                putTMVar  (qHead q) $! BS.drop (fromIntegral requested) bs
                pure $ BS.take (fromIntegral requested) bs
            | otherwise -> do
                writeTVar (qSize q) $! size - available
                pure bs

instance S.DuplexStream TStreamingQueue

instance S.OutputStream TStreamingQueue where
    send q bs = fromIntegral <$> atomically (enqueue q bs)

instance S.InputStream TStreamingQueue where
    receive q i = atomically $ dequeue q $ fromIntegral $ min i maxBoundIntWord32
