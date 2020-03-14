module MusicScroll.MPRIS (dbusThread) where

import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TBQueue (TBQueue, writeTBQueue)
import Control.Exception (bracket)
import Control.Monad (when, (=<<))
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.State.Class (gets, modify)
import Control.Monad.Trans.State (StateT, evalStateT)

import DBus.Client

import MusicScroll.TrackInfo
import MusicScroll.DBusSignals
import MusicScroll.ConnState
import MusicScroll.UIEvent

dbusThread :: TBQueue TrackInfo -> TBQueue UIEvent -> IO a
dbusThread trackChan eventChan = bracket connectSession disconnect
  (evalStateT go . newConnState trackChan eventChan)
  where
    go :: StateT ConnState IO a
    go = do mtrack <- liftIO . uncurry tryGetInfo =<<
                      (,) <$> gets cClient <*> gets cBusActive
            case mtrack of
              Left (NoMusicClient _) -> changeMusicClient
              Left (NoMetadata cause) -> do
                reportErrorOnUI cause
                waitForChange mediaPropChangeRule
              Right track -> do
                writeIfNotRepeated track
                waitForChange mediaPropChangeRule
            go

writeIfNotRepeated :: TrackInfo -> StateT ConnState IO ()
writeIfNotRepeated newSong = do
    query   <- (/=) <$> gets cLastSentTrack <*> pure (Just newSong)
    outTrackChan <- gets cOutTrackChan
    when query $
      do liftIO . atomically $ writeTBQueue outTrackChan newSong
         modify (setSong newSong)

reportErrorOnUI :: MetadataError -> StateT ConnState IO ()
reportErrorOnUI cause =
  do eventChan <- gets cOutEventChan
     let wrapedCause = ErrorOn (ENoMetadata cause)
     liftIO . atomically $ writeTBQueue eventChan wrapedCause
