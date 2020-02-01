{-# language OverloadedStrings, ScopedTypeVariables, NamedFieldPuns #-}
module MusicScroll.MPRIS where

import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TBQueue (TBQueue, writeTBQueue)
import Control.Concurrent.STM.TMVar (TMVar, takeTMVar, newEmptyTMVar, putTMVar)
import Control.Exception (bracket)
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.State.Class (MonadState(..), gets, modify)
import Control.Monad.Trans.State (StateT, evalStateT)
import Data.Foldable (traverse_)

import DBus.Client
import DBus (BusName)

import MusicScroll.TrackInfo
import MusicScroll.DBusNames

dbusThread :: TBQueue TrackInfo -> IO a
dbusThread outChan = bracket connectSession disconnect
  (evalStateT go . newConnState outChan)
  where
    go :: StateT ConnState IO a
    go = do mtrack <- gets cClient >>= liftIO . tryGetInfo
            traverse_ writeIfNotRepeated mtrack
            waitForChange
            go

writeIfNotRepeated :: TrackInfo -> StateT ConnState IO ()
writeIfNotRepeated newSong = do
    query   <- (/=) <$> gets cLastSentTrack <*> pure (Just newSong)
    outChan <- gets cOutChan
    when query $
      do liftIO . atomically $ writeTBQueue outChan newSong
         modify (setSong newSong)

waitForChange :: StateT ConnState IO ()
waitForChange =
  do client <- gets cClient
     liftIO $ do
       trigger       <- atomically newEmptyTMVar
       disarmHandler <- gotSignalOfChange client trigger
       _ <- atomically $ takeTMVar trigger
       removeMatch client disarmHandler

gotSignalOfChange :: Client -> TMVar () -> IO SignalHandler
gotSignalOfChange client trigger =
  let rule = matchAny
        { matchPath      = pure mediaObject
        , matchInterface = pure "org.freedesktop.DBus.Properties"
        , matchMember    = pure "PropertiesChanged" }
  in addMatch client rule (\_ -> atomically ( putTMVar trigger () ))

data ConnState = ConnState
  { cClient        :: Client
  , cBusActive     :: BusName
  , cOutChan       :: TBQueue TrackInfo
  , cLastSentTrack :: Maybe TrackInfo
  }

newConnState :: TBQueue TrackInfo -> Client -> ConnState
newConnState outChan c = ConnState c smplayerBus outChan Nothing

setSong :: TrackInfo -> ConnState -> ConnState
setSong track s = s { cLastSentTrack = pure track }
