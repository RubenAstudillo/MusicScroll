{-# language LambdaCase #-}
module MusicScroll.MPRIS (dbusThread, dbusThreadP, dbusThreadP2) where

import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TBQueue (TBQueue, writeTBQueue)
import Control.Exception (bracket)
import Control.Monad ((=<<), forever)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.State.Class (MonadState(..), gets, modify)
import Control.Monad.Trans.State (StateT, evalStateT)

import Debug.Trace

import Pipes as P
import Pipes.Prelude as P hiding (print)
import Pipes.Concurrent
import qualified Pipes.Safe as Safe
import Pipes.Safe (SafeT)

import DBus.Client

import MusicScroll.TrackInfo
import MusicScroll.DBusSignals
import MusicScroll.ConnState
import MusicScroll.ConnStateP
import MusicScroll.UIEvent

dbusThread :: TBQueue TrackIdentifier -> TBQueue UIEvent -> IO a
dbusThread trackChan eventChan = bracket connectSession disconnect
  (evalStateT loop . newConnState trackChan eventChan)
  where
    loop :: StateT ConnState IO a
    loop = forever $ do
      mtrack <- liftIO . uncurry tryGetInfo =<<
                (,) <$> gets cClient <*> gets cBusActive
      case mtrack of
        Left (NoMusicClient _) -> changeMusicClient
        Left NoSong -> reportErrorOnUI *> waitForChange mediaPropChangeRule
        (Right trackIdent) -> sendToLyricsPipeline trackIdent
                              *> waitForChange mediaPropChangeRule

dbusThreadP2 :: TBQueue TrackIdentifier -> TBQueue UIEvent -> IO a
dbusThreadP2 trackout errorout = bracket connectSession disconnect
  (evalStateT loop . newConnStateP)
  where
    loop :: StateT ConnStateP IO a
    loop = forever $ do
      printm "antes tryGetInfoP"
      iterNum <- gets iter
      liftIO (putStr "Iteracion numero " >> print iterNum)
      modify (\s -> s { iter = succ iterNum })
      tryGetInfoP >>= \case
        Left (NoMusicClient _) ->
          do printm "antes chageMusicP"
             changeMusicClientP
             printm "despues changeMusicP"
        Left NoSong ->
          do liftIO . putStrLn $ "Recive1: antes enviar error"
             liftIO . atomically $ writeTBQueue errorout (ErrorOn ENoSong)
             -- runEffect $ yield (ErrorOn ENoSong) >-> toOutput errorout
             liftIO . putStrLn $ "Recive1: despues enviar error"
             waitForChangeP2 mediaPropChangeRule
        Right trackIdent ->
          do liftIO . putStrLn $ "Recive1: antes enviar cancion"
             liftIO . atomically $ writeTBQueue trackout trackIdent
             -- runEffect $ yield trackIdent >-> toOutput trackout
             liftIO . putStrLn $ "Recive1: despues enviar cancion"
             waitForChangeP2 mediaPropChangeRule
             liftIO . putStrLn $ "Recive1: despues de esperar"

dbusThreadP :: Output TrackIdentifier -> Output UIEvent -> IO a
dbusThreadP trackout errorout = bracket connectSession disconnect
  (evalStateT loop . newConnStateP)
  where
    loop :: StateT ConnStateP IO a
    loop = forever $ printm "antes tryGetInfoP" >> tryGetInfoP >>= \case
        Left (NoMusicClient _) -> printm "antes chageMusicP" *> changeMusicClientP *> printm "despues changeMusicP"
        Left NoSong ->
          do liftIO . putStrLn $ "Recive1: antes enviar error"
             liftIO . atomically $ send errorout (ErrorOn ENoSong)
             -- runEffect $ yield (ErrorOn ENoSong) >-> toOutput errorout
             liftIO . putStrLn $ "Recive1: despues enviar error"
             waitForChangeP mediaPropChangeRule
        Right trackIdent ->
          do liftIO . putStrLn $ "Recive1: antes enviar cancion"
             liftIO . atomically $ send trackout trackIdent
             -- runEffect $ yield trackIdent >-> toOutput trackout
             liftIO . putStrLn $ "Recive1: despues enviar cancion"
             waitForChangeP mediaPropChangeRule

printm = liftIO . putStrLn

sendToLyricsPipeline :: TrackIdentifier -> StateT ConnState IO ()
sendToLyricsPipeline trackIdent =
  do outTrackChan <- gets cOutTrackChan
     liftIO . atomically $ writeTBQueue outTrackChan trackIdent

reportErrorOnUI :: StateT ConnState IO ()
reportErrorOnUI =
  do eventChan <- gets cOutEventChan
     let wrapedCause = ErrorOn (ENoSong)
     liftIO . atomically $ writeTBQueue eventChan wrapedCause
