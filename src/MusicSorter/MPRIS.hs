{-# language OverloadedStrings, ScopedTypeVariables #-}
module MusicSorter.MPRIS where

import Control.Monad (unless, join)
import Control.Exception (Exception)
import qualified Control.Exception as Exc
import DBus
import DBus.Client
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)

smplayerBus :: BusName
smplayerBus = "org.mpris.MediaPlayer2.smplayer"

mediaObject :: ObjectPath
mediaObject = "/org/mpris/MediaPlayer2"

mediaInterface :: InterfaceName
mediaInterface = "org.mpris.MediaPlayer2.Player"

dbusThread :: TChan TrackInfo -> IO Text
dbusThread outChan =
  Exc.bracket connectSession disconnect $ \client -> do
      track <- tryGetInfo client
  where
    tryGetInfo :: Client -> IO TrackInfo
    tryGetInfo client = do
        resp <- getPropertyValue client
                    (methodCall mediaObject mediaInterface "Metadata") {
                    methodCallDestination = Just smplayerBus }
        let possiblyTrackInfo = obtainTrackInfo resp
        either throwIO return possiblyTrackInfo

data TrackInfo = TrackInfo
  { tlength :: Double
  } deriving (Show)

data TrackInfoError = NoMusicClient | NoMetadata
  deriving (Exception)

obtainTrackInfo :: Either Text (Map Text Variant)
                -> Either TrackInfoError TrackInfo
obtainTrackInfo = join $ bimap (const NoMusicClient) go
  where
    go :: Map Text Variant -> Either TrackInfoError TrackInfo
    go v = let val = Map.lookup "xesam:length" v >>= fromVariant
           in maybe (Left NoMetadata) TrackInfo val
