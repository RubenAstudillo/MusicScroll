{-# language OverloadedStrings, ScopedTypeVariables, NamedFieldPuns #-}
module MusicSorter.MPRIS where

import           Control.Concurrent (threadDelay)
import           Control.Concurrent.Async (race_)
import           Control.Concurrent.MVar (MVar)
import           Control.Concurrent.STM.TBQueue (TBQueue)
import qualified Control.Concurrent.STM.TBQueue as TQueue
import           Control.Concurrent.STM.TMVar (TMVar, takeTMVar)
import           Control.Exception (Exception)
import qualified Control.Exception as Exc
import           Control.Monad (when, join)
import           DBus
import           DBus.Client
import           Data.Bifunctor (first, bimap)
import           Data.Functor (void)
import           Data.Function (&)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Text (Text)

smplayerBus :: BusName
smplayerBus = "org.mpris.MediaPlayer2.smplayer"

mediaObject :: ObjectPath
mediaObject = "/org/mpris/MediaPlayer2"

mediaInterface :: InterfaceName
mediaInterface = "org.mpris.MediaPlayer2.Player"

dbusThread :: TMVar () -> TBQueue TrackInfo -> IO a
dbusThread trigger outChan =
  Exc.bracket connectSession disconnect (flip go Nothing)
  where
    go :: Client -> Maybe TrackInfo -> IO a
    go client lastTrack =
        do track <- tryGetInfo client -- throws, shoould arm a mvar that is put when the dbus connections changes.
           writeIfNotRepeated lastTrack track
           let userInterrupt = void . atomically $ takeTMVar trigger
           race_ (songDelay track) userInterrupt
           go client track

writeIfNotRepeated :: TBQueue TrackInfo -> Maybe TrackInfo -> TrackInfo
                   -> IO ()
writeIfNotRepeated outChan maybelast current = do
    let query = (/=) <$> maybeLast <*> pure current
    when (maybe True id query) $
        atomically (TBQueue.writeTBQueue outChan current)

songDelay :: TrackInfo -> IO ()
songDelay (TrackInfo {tLength}) = threadDelay (tLenght * 1e6)

-- An exception here means that either there is not a music player
-- running or what it is running it's not a song. Either way we should
-- wait for a change on the dbus connection to try again.
tryGetInfo :: Client -> IO TrackInfo
tryGetInfo client = do
    metadata <- getPropertyValue client
                    (methodCall mediaObject mediaInterface "Metadata") {
                    methodCallDestination = Just smplayerBus }
                    & first NoMusicClient
    position <- getPropertyValue client
                    (methodCall mediaObject mediaInterface "Position") {
                    methodCallDestination = Just smplayerBus }
                    & first NoMusicClient
    either throwIO id . join $ obtainTrackInfo <$> metadata <*> position

data TrackInfo = TrackInfo
  { tTitle  :: [Text]
  , tArtist :: [Text]
  , tLength :: Double
  , tPos    :: Int
  } deriving (Eq, Show) -- TODO: better eq instance

data TrackInfoError = NoMusicClient MethodError | NoMetadata
  deriving (Exception)

obtainTrackInfo :: Map Text Variant -> Int
                -> Either TrackInfoError TrackInfo
obtainTrackInfo metadata pos =
  let lookup name = Map.lookup name v >>= fromVariant
      track = TrackInfo <$> lookup "xesam:title"
              <*> lookup "xesam:artist" <*> lookup "xesam:length"
              <*> pure pos
  in maybe (Left NoMetadata) Right track
