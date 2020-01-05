{-# language OverloadedStrings, ScopedTypeVariables, NamedFieldPuns #-}
module MusicSorter.MPRIS where

import           Control.Concurrent (threadDelay)
import           Control.Concurrent.Async (race_)
import           Control.Concurrent.MVar (MVar)
import           Control.Concurrent.STM (atomically)
import           Control.Concurrent.STM.TBQueue (TBQueue)
import qualified Control.Concurrent.STM.TBQueue as TBQueue
import           Control.Concurrent.STM.TMVar (TMVar, takeTMVar)
import           Control.Exception (Exception)
import qualified Control.Exception as Exc
import           Control.Monad (when, join)
import           DBus
import           DBus.Client
import           Data.Bifunctor (first, bimap)
import           Data.Function ((&))
import           Data.Functor (void)
import           Data.Int (Int64)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Text (Text)
import           Data.Typeable (Typeable)

 -- [1]:
 -- throws, shoould arm a mvar that is put when the dbus
 -- connections changes.

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
        do track <- tryGetInfo client -- [1]
           writeIfNotRepeated outChan lastTrack track
           let userInterrupt = void . atomically $ takeTMVar trigger
           race_ (songDelay track) userInterrupt
           go client (pure track)

writeIfNotRepeated :: TBQueue TrackInfo -> Maybe TrackInfo -> TrackInfo
                   -> IO ()
writeIfNotRepeated outChan maybeLast current = do
    let query = (/=) <$> maybeLast <*> pure current
    when (maybe True id query) $
        atomically (TBQueue.writeTBQueue outChan current)

songDelay :: TrackInfo -> IO ()
songDelay (TrackInfo {tLength}) = threadDelay $ floor tLength

-- An exception here means that either there is not a music player
-- running or what it is running it's not a song. Either way we should
-- wait for a change on the dbus connection to try again.
tryGetInfo :: Client -> IO TrackInfo
tryGetInfo client = do
    metadata <- getPropertyValue client
                    (methodCall mediaObject mediaInterface "Metadata") {
                    methodCallDestination = Just smplayerBus }
                    & fmap (first NoMusicClient)
    position <- getPropertyValue client
                    (methodCall mediaObject mediaInterface "Position") {
                    methodCallDestination = Just smplayerBus }
                    & fmap (first NoMusicClient)
    either Exc.throwIO return . join $ obtainTrackInfo <$> metadata <*> position

data TrackInfo = TrackInfo
  { tTitle  :: Text
  , tArtist :: Text
  , tLength :: Double
  , tPos    :: Int64
  } deriving (Eq, Show) -- TODO: better eq instance

data TrackInfoError = NoMusicClient MethodError | NoMetadata
  deriving (Show, Typeable)

instance Exc.Exception TrackInfoError

obtainTrackInfo :: Map Text Variant -> Int64
                -> Either TrackInfoError TrackInfo
obtainTrackInfo metadata pos =
  let lookup name = Map.lookup name metadata >>= fromVariant
      track = TrackInfo <$> lookup "xesam:title"
              <*> lookup "xesam:artist" <*> lookup "mpris:length"
              <*> pure pos
  in maybe (Left NoMetadata) Right track
