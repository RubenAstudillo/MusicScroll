module MusicScroll.MPRIS (dbusThread) where

import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TBQueue (TBQueue, writeTBQueue)
import Control.Exception (bracket)
import Control.Monad ((=<<), forever)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.State.Class (gets)
import Control.Monad.Trans.State (StateT, evalStateT)

import Pipes as P
import Pipes.Prelude as P
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

dbusThreadP :: Output TrackIdentifier -> Output TrackInfoError
            -> Producer TrackIdentifier IO ()
dbusThreadP trackout errorout = do
  state <- Safe.runSafeP $ Safe.bracket (liftIO connectSession)
             (liftIO . disconnect) (pure . newConnStateP)
  loop state
  where
    -- loop :: ConnStateP -> _
    loop state =
      do Just mtrack <- P.head $ for (yield state) tryGetInfoP
         case mtrack of
           Left (NoMusicClient _) -> changeMusicClientP state >>= loop
           Left NoSong -> do yield (ErrorOn ENoSong) >-> toOutput errorout
                             wait state *> loop state
           Right trackIdent -> do yield trackIdent >-> toOutput trackout
                                  wait state *> loop state

    wait s = waitForChangeP s mediaPropChangeRule

sendToLyricsPipeline :: TrackIdentifier -> StateT ConnState IO ()
sendToLyricsPipeline trackIdent =
  do outTrackChan <- gets cOutTrackChan
     liftIO . atomically $ writeTBQueue outTrackChan trackIdent

reportErrorOnUI :: StateT ConnState IO ()
reportErrorOnUI =
  do eventChan <- gets cOutEventChan
     let wrapedCause = ErrorOn (ENoSong)
     liftIO . atomically $ writeTBQueue eventChan wrapedCause
