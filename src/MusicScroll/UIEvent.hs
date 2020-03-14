{-# language OverloadedStrings #-}
module MusicScroll.UIEvent where

import Data.Text (Text)

import MusicScroll.TrackInfo (TrackInfo(..), MetadataError(..))
import MusicScroll.TagParsing (Lyrics(..))

data UIEvent = GotLyric TrackInfo Lyrics
             | ErrorOn ErrorCause

data ErrorCause = NotOnDB MetadataError | NoLyricsOnWeb

errorMsg :: ErrorCause -> Text
errorMsg (NotOnDB NoArtist) =
  "No lyrics found by hash on the song file, try to suplement the song's\
  \ artist metadata to try to get it from the web."
errorMsg (NotOnDB NoTitle) =
  "No lyrics found by hash on the song file, try to suplement the song's\
  \ title metadata to try to get it from the web."
errorMsg NoLyricsOnWeb = "Lyrics provider didn't have that song."
