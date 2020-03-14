module MusicScroll.MPRIS (dbusThread) where

import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TBQueue (TBQueue, writeTBQueue)
import Control.Exception (bracket)
import Control.Monad (when, (=<<), forever)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.State.Class (gets, modify)
import Control.Monad.Trans.State (StateT, evalStateT)

import DBus.Client

import MusicScroll.TrackInfo
import MusicScroll.DBusSignals
import MusicScroll.ConnState
import MusicScroll.UIEvent

dbusThread :: TBQueue TrackIdentifier -> TBQueue UIEvent -> IO a
dbusThread trackChan eventChan = bracket connectSession disconnect
  (evalStateT loop . newConnState trackChan eventChan)
  where
    sendAndWaitNewSong val = sendToLyricsPipeline val
                             *> waitForChange mediaPropChangeRule

    loop :: StateT ConnState IO a
    loop = forever $ do
      mtrack <- liftIO . uncurry tryGetInfo =<<
                (,) <$> gets cClient <*> gets cBusActive
      case mtrack of
        Left (NoMusicClient _) -> changeMusicClient
        Left (NoMetadata path) -> sendAndWaitNewSong (Left path)
        (Right track)          -> sendAndWaitNewSong (Right track)

sendToLyricsPipeline :: TrackIdentifier -> StateT ConnState IO ()
sendToLyricsPipeline partialTrack =
  do outTrackChan <- gets cOutTrackChan
     liftIO . atomically $ writeTBQueue outTrackChan partialTrack
