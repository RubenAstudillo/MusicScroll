{-# language OverloadedStrings, NamedFieldPuns #-}
module MusicScroll.TrackInfo
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
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Char (isAlpha)

import           MusicScroll.DBusNames

data TrackInfo = TrackInfo
  { tTitle  :: Text
  , tArtist :: Text -- xesam:artist is weird
  } deriving (Eq, Show) -- TODO: better eq instance

data TrackInfoError = NoMusicClient MethodError | NoMetadata

-- An exception here means that either there is not a music player
-- running or what it is running it's not a song. Either way we should
-- wait for a change on the dbus connection to try again.
tryGetInfo :: Client -> BusName -> IO (Either TrackInfoError TrackInfo)
tryGetInfo client busName = do
    metadata <- getPropertyValue client
                  (methodCall mediaObject mediaInterface "Metadata") {
                    methodCallDestination = pure busName
                  } & fmap (first NoMusicClient)
    return . join $ obtainTrackInfo <$> metadata

obtainTrackInfo :: Map Text Variant -> Either TrackInfoError TrackInfo
obtainTrackInfo metadata =
  let lookup name = Map.lookup name metadata >>= fromVariant
      track = TrackInfo <$> lookup "xesam:title" <*>
                xesamArtistFix (lookup "xesam:artist") (lookup "xesam:artist")
  in maybe (Left NoMetadata) pure track

-- xesam:artist by definition should return a `[Text]`, but in practice
-- it returns a `Text`. This function makes it always return `Text`.
xesamArtistFix :: Maybe Text -> Maybe [Text] -> Maybe Text
xesamArtistFix (Just title) _ = pure title
xesamArtistFix Nothing (Just arr) | (title : _) <- arr = pure title
xesamArtistFix _ _ = Nothing

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
