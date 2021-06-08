{-# LANGUAGE DataKinds #-}

module MusicScroll.Providers.Utils where

import Data.Text (Text)
import MusicScroll.TrackInfo (TrackInfo)
import Network.HTTP.Req

newtype Lyrics = Lyrics Text

instance Show Lyrics where
  show _ = "Lyrics"

data Provider = Provider
  { toUrl :: TrackInfo -> Url 'Https,
    extractLyricsFromPage :: Text -> Lyrics
  }
