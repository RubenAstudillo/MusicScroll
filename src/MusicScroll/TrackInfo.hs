{-# language OverloadedStrings, NamedFieldPuns #-}
module MusicScroll.TrackInfo
  ( TrackInfo(..)
  , TrackByPath(..)
  , TrackIdentifier
  , TrackInfoError(..)
  , SongFilePath
  , tryGetInfo
  , cleanTrack
  ) where

import           Prelude hiding (readFile, lookup)
import           Control.Applicative (Alternative(..))
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
  , tUrl    :: SongFilePath
  } deriving (Eq, Show) -- TODO: better eq instance

data TrackByPath = TrackByPath
  { tpPath :: SongFilePath
  , tpTitle :: Maybe Text -- Best effort
  , tpArtist :: Maybe Text -- Best effort
  } deriving (Eq, Show)

type SongFilePath = FilePath
type TrackIdentifier = Either TrackByPath TrackInfo

data TrackInfoError = NoMusicClient MethodError
                    | NoSong

-- An exception here means that either there is not a music player
-- running or what it is running it's not a song. Either way we should
-- wait for a change on the dbus connection to try again.
tryGetInfo :: Client -> BusName -> IO (Either TrackInfoError TrackIdentifier)
tryGetInfo client busName = do
    metadata <- getPropertyValue client
                  (methodCall mediaObject mediaInterface "Metadata") {
                    methodCallDestination = pure busName
                  } & fmap (first NoMusicClient)
    return . join $ obtainTrackInfo <$> metadata

obtainTrackInfo :: Map Text Variant -> Either TrackInfoError TrackIdentifier
obtainTrackInfo metadata =
  let lookup :: IsVariant a => Text -> Maybe a
      lookup name = Map.lookup name metadata >>= fromVariant

      mTitle  = lookup "xesam:title"
      mArtist = xesamArtistFix (lookup "xesam:artist") (lookup "xesam:artist")
      mUrl    = lookup "xesam:url"

      trackInfo :: Maybe TrackInfo
      trackInfo = TrackInfo <$> mTitle <*> mArtist <*> mUrl

      trackByPath :: Maybe TrackByPath
      trackByPath = TrackByPath <$> mUrl <*> pure mTitle <*> pure mArtist

      trackIdent :: Maybe TrackIdentifier
      trackIdent =  (Left <$> trackByPath) <|> (Right <$> trackInfo)
  in maybe (Left NoSong) Right trackIdent

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
