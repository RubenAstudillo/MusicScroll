{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module MusicScroll.Providers.AZLyrics (azLyricsInstance) where

import Control.Category hiding (id, (.))
import Data.Maybe (catMaybes)
import Data.Text as T hiding (filter, map, mapAccumL, tail, elem)
import Data.Traversable (mapAccumL)
import MusicScroll.Providers.Utils
import MusicScroll.TrackInfo (TrackInfo (..))
import Network.HTTP.Req
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Match (tagOpenLit)

azLyricsInstance :: Provider
azLyricsInstance =
  Provider
    { toUrl = toUrl',
      extractLyricsFromPage = pipeline
    }

toUrl' :: TrackInfo -> Url 'Https
toUrl' track =
  let base :: Url 'Https
      base = https "www.azlyrics.com"

      quotedArtist = normalize (tArtist track)
      quotedSong = normalize (tTitle track) <> ".html"
   in base /: "lyrics" /: quotedArtist /: quotedSong

normalize :: Text -> Text
normalize =
  let targets :: String
      targets = " '_-"
   in T.intercalate mempty . split (`elem` targets) . toLower

pipeline :: Text -> Lyrics
pipeline =
  parseTags >>> mapAccumL discriminate False >>> snd
    >>> catMaybes
    >>> innerText
    >>> stripStart
    >>> Lyrics

discriminate :: Bool -> Tag Text -> (Bool, Maybe (Tag Text))
discriminate onDiv@True tag | isTagText tag = (onDiv, pure tag)
discriminate onDiv tag
  | tagOpenLit "div" (== []) tag = (True, Nothing)
  | isTagCloseName "div" tag = (False, Nothing)
  | otherwise = (onDiv, Nothing)
