{-# LANGUAGE OverloadedStrings #-}

-- | Netem tests.

module Main where

import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Concurrent.MVar
import           Control.Exception
import           Data.ByteString (ByteString)
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import           Formatting
import           Formatting.Clock
import qualified Network.Socket as N
import qualified Network.Socket.ByteString as NB
import           System.Clock
import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: SpecWith ()
spec = do latency

latency :: SpecWith ()
latency =
  describe
    "Latency"
    (do it
          ""
          (do serverRunning <- newEmptyMVar
              serverDone <- newEmptyMVar
              t0 <- getTime Monotonic
              _ <- concurrently (udpAnalysisServer serverRunning serverDone)
                                (udpAnalysisClient serverRunning serverDone)
              t1 <- getTime Monotonic
              fprint
                ("Received " % int % " packets in " % timeSpecs)
                udpAnalysisPackets
                t0
                t1))

udpAnalysisServer :: MVar () -> MVar () -> IO ()
udpAnalysisServer running done = do
  addrinfos <-
    N.getAddrInfo Nothing (Just udpAnalysisHost) (Just udpAnalysisPort)
  case addrinfos of
    [] -> error "getAddrInfo failed."
    (serveraddr:_) -> do
      sock <- N.socket (N.addrFamily serveraddr) N.Datagram N.defaultProtocol
      N.bind sock (N.addrAddress serveraddr)
      putMVar running ()
      finally
        (do let loop :: Int -> IO ()
                loop 0 = pure ()
                loop i = do
                  -- S.putStrLn (S8.pack ("recv: " ++ show (i)))
                  packet <- NB.recv sock udpAnalysisPacketSize
                  yield
                  if S.length packet == udpAnalysisPacketSize
                    then loop (i - 1)
                    else error "Invalid packet size."
            loop udpAnalysisPackets)
        (do putMVar done ()
            N.close sock)

udpAnalysisClient :: MVar () -> MVar () -> IO ()
udpAnalysisClient serverRunning serverDone = do
  takeMVar serverRunning
  addrinfos <-
    N.getAddrInfo Nothing (Just udpAnalysisHost) (Just udpAnalysisPort)
  let serveraddr = head addrinfos
  sock <- N.socket (N.addrFamily serveraddr) N.Datagram N.defaultProtocol
  N.connect sock (N.addrAddress serveraddr)
  let loop 0 = pure ()
      loop i = do
        sent <- NB.send sock (S.replicate udpAnalysisPacketSize 97)
        -- S.putStrLn (S8.pack ("send: " ++ show (i)))
        yield
        if sent == udpAnalysisPacketSize
           then loop (i -1)
           else do error "Packet didn't send."
                   loop i
  loop udpAnalysisPackets
  takeMVar serverDone
  N.close sock

udpAnalysisHost :: N.HostName
udpAnalysisHost = "127.0.0.1"

udpAnalysisPort :: N.ServiceName
udpAnalysisPort = "7000"

udpAnalysisPackets :: Int
udpAnalysisPackets = 100

udpAnalysisPacketSize :: Int
udpAnalysisPacketSize = 4096
