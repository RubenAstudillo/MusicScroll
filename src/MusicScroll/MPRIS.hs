{-# language LambdaCase #-}
module MusicScroll.MPRIS (dbusThread, dbusThreadP) where

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

dbusThreadP :: Output TrackIdentifier -> Output ErrorCause -> IO a
dbusThreadP trackout errorout = bracket connectSession disconnect
  (evalStateT loop . newConnStateP)
  where
    loop :: StateT ConnStateP IO a
    loop = forever $ tryGetInfoP >>= \case
        Left (NoMusicClient _) -> changeMusicClientP
        Left NoSong ->
          do runEffect $ yield ENoSong >-> toOutput errorout
             waitForChangeP mediaPropChangeRule
        Right trackIdent ->
          do runEffect $ yield trackIdent >-> toOutput trackout
             waitForChangeP mediaPropChangeRule

-- printm = liftIO . putStrLn

sendToLyricsPipeline :: TrackIdentifier -> StateT ConnState IO ()
sendToLyricsPipeline trackIdent =
  do outTrackChan <- gets cOutTrackChan
     liftIO . atomically $ writeTBQueue outTrackChan trackIdent

reportErrorOnUI :: StateT ConnState IO ()
reportErrorOnUI =
  do eventChan <- gets cOutEventChan
     let wrapedCause = ErrorOn (ENoSong)
     liftIO . atomically $ writeTBQueue eventChan wrapedCause
