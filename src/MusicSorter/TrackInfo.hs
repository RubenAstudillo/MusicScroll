{-# language OverloadedStrings, ScopedTypeVariables, NamedFieldPuns #-}
module MusicSorter.TrackInfo
  ( tryGetInfo
  , TrackInfo(..)
  , TrackInfoError(..)
  ) where

import           Control.Monad (join)
import           DBus
import           DBus.Client
import           Data.Bifunctor (first)
import           Data.Function ((&))
import           Data.Int (Int64)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Text (Text)

import           MusicSorter.DBusNames

-- An exception here means that either there is not a music player
-- running or what it is running it's not a song. Either way we should
-- wait for a change on the dbus connection to try again.
tryGetInfo :: Client -> IO (Either TrackInfoError TrackInfo)
tryGetInfo client = do
    metadata <- getPropertyValue client
                    (methodCall mediaObject mediaInterface "Metadata") {
                    methodCallDestination = Just smplayerBus }
                    & fmap (first NoMusicClient)
    position <- getPropertyValue client
                    (methodCall mediaObject mediaInterface "Position") {
                    methodCallDestination = Just smplayerBus }
                    & fmap (first NoMusicClient)
    return . join $
      obtainTrackInfo <$> metadata <*> position

data TrackInfo = TrackInfo
  { tTitle  :: Text
  , tArtist :: Text
  , tLength :: Double
  , tPos    :: Int64
  } deriving (Eq, Show) -- TODO: better eq instance

data TrackInfoError = NoMusicClient MethodError | NoMetadata

obtainTrackInfo :: Map Text Variant -> Int64
                -> Either TrackInfoError TrackInfo
obtainTrackInfo metadata pos =
  let lookup name = Map.lookup name metadata >>= fromVariant
      track = TrackInfo <$> lookup "xesam:title"
              <*> lookup "xesam:artist" <*> lookup "mpris:length"
              <*> pure pos
  in maybe (Left NoMetadata) Right track
