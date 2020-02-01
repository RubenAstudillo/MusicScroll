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

dbusThread :: TBQueue TrackInfo -> IO a
dbusThread outChan = bracket connectSession disconnect
  (evalStateT go . newConnState outChan)
  where
    go :: StateT ConnState IO a
    go = do mtrack <- liftIO . uncurry tryGetInfo =<<
                      (,) <$> gets cClient <*> gets cBusActive
            case mtrack of
              Left (NoMusicClient _) -> changeMusicClient
              Left NoMetadata -> waitForChange mediaPropChangeRule
              Right track -> do writeIfNotRepeated track
                                waitForChange mediaPropChangeRule
            go

writeIfNotRepeated :: TrackInfo -> StateT ConnState IO ()
writeIfNotRepeated newSong = do
    query   <- (/=) <$> gets cLastSentTrack <*> pure (Just newSong)
    outChan <- gets cOutChan
    when query $
      do liftIO . atomically $ writeTBQueue outChan newSong
         modify (setSong newSong)
