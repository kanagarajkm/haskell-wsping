{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Concurrent (MVar, modifyMVar, modifyMVar_, newMVar,
                                     readMVar)
import           Control.Concurrent (forkIO, threadDelay)
import           Control.Exception  (AsyncException, fromException, handle,
                                     throwIO)
import           Control.Monad      (forM_, forever)
import           Data.Text          (Text)
import qualified Data.Text.IO       as T

import qualified Network.WebSockets as WS

import           Lib

main :: IO ()
main = do
    WS.runServer "localhost" 9160 $ application

application :: WS.ServerApp
application pending = do
    conn <- WS.acceptRequest pending
    WS.forkPingThread conn 10
    startDataPing conn
    forever $ do
        (msg :: Text) <- WS.receiveData conn
        print msg
        return ()
    return ()

startDataPing :: WS.Connection -> IO ()
startDataPing conn = do
    _ <- forkIO (ignore `handle` go)
    return ()
  where
    go :: IO ()
    go = do
        sendPingMsg conn
        threadDelay (5 * 1000 * 1000)
        go
    ignore e =
        case fromException e of
            Just async -> throwIO (async :: AsyncException)
            Nothing    -> return ()

sendPingMsg :: WS.Connection -> IO ()
sendPingMsg conn = do
    WS.sendTextData conn ("__PING__" :: Text)
