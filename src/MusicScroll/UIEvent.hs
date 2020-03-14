{-# language OverloadedStrings #-}
module MusicScroll.UIEvent where

import Data.Text (Text)

import MusicScroll.TrackInfo (TrackInfo(..), MetadataError(..))
import MusicScroll.TagParsing (Lyrics(..))

data UIEvent = GotLyric TrackInfo Lyrics
             | ErrorOn ErrorCause

data ErrorCause = ENoMetadata MetadataError | NoLyricsOnWeb

errorMsg :: ErrorCause -> Text
errorMsg (ENoMetadata NoArtist) = "No artist on song's metadata, please suplement info."
errorMsg (ENoMetadata NoTitle) = "No title on song's metadata, please suplement info."
errorMsg NoLyricsOnWeb = "Lyrics provider didn't have that song."
