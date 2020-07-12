{-# language OverloadedStrings, NamedFieldPuns, FlexibleContexts, PatternSynonyms #-}
module MusicScroll.TrackInfo where

import           Prelude hiding (readFile, lookup)
import           Control.Applicative (Alternative(..))
import           Control.Monad (join)
import           Control.Monad.State.Class (MonadState(..))
import           DBus
import           DBus.Client
import           Data.Bifunctor (first, bimap)
import           Data.Map.Strict (Map, lookup)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Char (isAlpha)
import qualified Pipes.Prelude as PP (map)

import           MusicScroll.DBusNames
import           MusicScroll.ConnState

import Pipes

data TrackInfo = TrackInfo
  { tTitle  :: Text
  , tArtist :: Text -- xesam:artist is weird
  , tUrl    :: SongFilePath
  } deriving (Show)

instance Eq TrackInfo where
  t1 == t2 = tUrl t1 == tUrl t2

data TrackByPath = TrackByPath
  { tpPath :: SongFilePath
  , tpTitle :: Maybe Text -- Best effort
  , tpArtist :: Maybe Text -- Best effort
  } deriving (Show)

instance Eq TrackByPath where
  t1 == t2 = tpPath t1 == tpPath t2

type SongFilePath = FilePath
type TrackIdentifier = Either TrackByPath TrackInfo

newtype TrackIdentifierWithEq = TIWE TrackIdentifier

instance Eq TrackIdentifierWithEq where
  (TIWE t1) == (TIWE t2) = extractUrl t1 == extractUrl t2

extractUrl :: TrackIdentifier -> SongFilePath
extractUrl = either tpPath tUrl

pattern OnlyMissingArtist :: TrackByPath
pattern OnlyMissingArtist <- TrackByPath {tpArtist = Nothing, tpTitle = Just _}


data DBusError = NoMusicClient MethodError | NoSong

-- An exception here means that either there is not a music player
-- running or what it is running it's not a song. Either way we should
-- wait for a change on the dbus connection to try again.
tryGetInfo :: (MonadState ConnState m, MonadIO m) =>
  m (Either DBusError TrackIdentifier)
tryGetInfo = do
  (ConnState client busName) <- get
  liftIO $ do
    metadata <- (first NoMusicClient) <$> getPropertyValue client
      (methodCall mediaObject mediaInterface "Metadata") {
        methodCallDestination = pure busName
      }
    pure . join $ obtainTrackInfo <$> metadata

obtainTrackInfo :: Map Text Variant -> Either DBusError TrackIdentifier
obtainTrackInfo metadata =
  let lookup' :: IsVariant a => Text -> Maybe a
      lookup' name = lookup name metadata >>= fromVariant

      mTitle  = lookup' "xesam:title"
      mArtist = xesamArtistFix (lookup' "xesam:artist") (lookup' "xesam:artist")
      mUrl    = vlcFix <$> lookup' "xesam:url"

      trackInfo :: Maybe TrackInfo
      trackInfo = TrackInfo <$> mTitle <*> mArtist <*> mUrl

      trackByPath :: Maybe TrackByPath
      trackByPath = TrackByPath <$> mUrl <*> pure mTitle <*> pure mArtist

      trackIdent :: Maybe TrackIdentifier
      trackIdent =  (Right <$> trackInfo) <|> (Left <$> trackByPath)
  in maybe (Left NoSong) Right trackIdent

-- xesam:artist by definition should return a `[Text]`, but in practice
-- it returns a `Text`. This function makes it always return `Text`.
xesamArtistFix :: Maybe Text -> Maybe [Text] -> Maybe Text
xesamArtistFix (Just title) _ = pure title
xesamArtistFix Nothing (Just arr) | (title : _) <- arr = pure title
xesamArtistFix _ _ = Nothing

cleanTrack :: Functor m => Pipe TrackIdentifier TrackIdentifier m a
cleanTrack = PP.map go
  where
    go :: TrackIdentifier -> TrackIdentifier
    go = bimap (\byPath -> let newTitle = cleanTitle <$> tpTitle byPath
                           in byPath { tpTitle = newTitle })
               (\track -> track { tTitle = cleanTitle (tTitle track) })

-- | This functions does two main things:
--     1. Remove format at the end, ie .mp3, .opus etc.
--     2. Remove the leading order separators, ie "05 - song name" ->
--        "song name"
cleanTitle :: Text -> Text
cleanTitle title0 =
  let (title1, format) = first T.init $ T.breakOnEnd "." title0
      title2 = if elem format musicFormats then title1 else title0
  in T.dropWhile (not . isAlpha) title2

musicFormats :: [Text]
musicFormats = ["mp3", "flac", "ogg", "wav", "acc", "opus", "webm"]

vlcFix :: SongFilePath -> SongFilePath
vlcFix = T.unpack . T.replace "%20" " " . T.replace "file://" "" . T.pack
