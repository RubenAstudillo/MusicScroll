{-# language OverloadedStrings, ScopedTypeVariables, NamedFieldPuns #-}
module MusicSorter.TrackInfo
  ( tryGetInfo
  , TrackInfo(..)
  , TrackInfoError(..)
  , cleanTrack
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
import qualified Data.Text as T
import           Data.Char (isAlpha)
import           Data.Bifunctor

import           MusicSorter.DBusNames

data TrackInfo = TrackInfo
  { tTitle  :: Text
  , tArtist :: Text
  , tLength :: Double
  , tPos    :: Int64
  } deriving (Eq, Show) -- TODO: better eq instance

data TrackInfoError = NoMusicClient MethodError | NoMetadata

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

obtainTrackInfo :: Map Text Variant -> Int64
                -> Either TrackInfoError TrackInfo
obtainTrackInfo metadata pos =
  let lookup name = Map.lookup name metadata >>= fromVariant
      track = TrackInfo <$> lookup "xesam:title"
              <*> lookup "xesam:artist" <*> lookup "mpris:length"
              <*> pure pos
  in maybe (Left NoMetadata) Right track



cleanTrack :: TrackInfo -> TrackInfo
cleanTrack t@(TrackInfo {tTitle}) = t { tTitle = cleanTitle tTitle }

-- Remove .mp3 and numbers from the title.
cleanTitle :: Text -> Text
cleanTitle title0 =
  let (title1, format) = first T.init $ T.breakOnEnd "." title0
      title2 = if elem format musicFormats then title1 else title0
  in T.dropWhile (not . isAlpha) title2

musicFormats :: [Text]
musicFormats = ["mp3", "flac", "ogg", "wav", "acc", "opus", "webm"]
