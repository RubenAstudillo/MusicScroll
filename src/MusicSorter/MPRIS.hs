{-# language OverloadedStrings, ScopedTypeVariables, NamedFieldPuns #-}
module MusicSorter.MPRIS where

import           Control.Concurrent.STM (atomically)
import           Control.Concurrent.STM.TBQueue (TBQueue)
import qualified Control.Concurrent.STM.TBQueue as TBQueue
import           Control.Concurrent.STM.TMVar (TMVar, takeTMVar,
                                               newEmptyTMVar, putTMVar)
import qualified Control.Exception as Exc
import           Control.Monad (when)
import           DBus.Client

import           MusicSorter.TrackInfo
import           MusicSorter.DBusNames

dbusThread :: TBQueue TrackInfo -> IO a
dbusThread outChan =
  Exc.bracket connectSession disconnect (flip go Nothing)
  where
    go :: Client -> Maybe TrackInfo -> IO a
    go client lastTrack =
        do mtrack <- tryGetInfo client
           case mtrack of
             Right track ->
                 do writeIfNotRepeated outChan lastTrack track
                    waitForChange client
                    go client (pure track)

             Left _ -> waitForChange client >> go client lastTrack

writeIfNotRepeated :: TBQueue TrackInfo -> Maybe TrackInfo -> TrackInfo
                   -> IO ()
writeIfNotRepeated outChan maybeLast current = do
    let query = (/=) <$> maybeLast <*> pure current
    when (maybe True id query) $
        atomically (TBQueue.writeTBQueue outChan current)

waitForChange :: Client -> IO ()
waitForChange client =
  do trigger <- atomically newEmptyTMVar
     disarmHandler <- gotSignalOfChange client trigger
     _ <- atomically $ takeTMVar trigger
     removeMatch client disarmHandler

gotSignalOfChange :: Client -> TMVar () -> IO SignalHandler
gotSignalOfChange client trigger =
  let rule = matchAny
        { matchPath = pure mediaObject
        , matchInterface = pure "org.freedesktop.DBus.Properties"
        , matchMember = pure "PropertiesChanged" }
  in addMatch client rule (\_ -> atomically ( putTMVar trigger () ))
