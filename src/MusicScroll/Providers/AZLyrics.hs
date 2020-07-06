{-# language OverloadedStrings, DataKinds #-}
module MusicScroll.Providers.AZLyrics (azLyricsInstance) where

import Control.Category hiding ((.), id)
import Data.Maybe (catMaybes)
import Data.Text as T hiding (filter, tail, map, mapAccumL)
import Data.Traversable (mapAccumL)
import Network.HTTP.Req
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Match (tagOpenLit)

import MusicScroll.TrackInfo (TrackInfo(..))
import MusicScroll.Providers.Utils

azLyricsInstance :: Provider
azLyricsInstance = Provider
  { toUrl = toUrl'
  , extractLyricsFromPage = pipeline }

toUrl' :: TrackInfo -> Url 'Https
toUrl' track =
  let base :: Url 'Https
      base = https "www.azlyrics.com"

      quotedArtist = normalize (tArtist track)
      quotedSong = normalize (tTitle track) <> ".html"
  in base /: "lyrics" /: quotedArtist /: quotedSong

normalize :: Text -> Text
normalize = let noSpaces = replace " " "" in noSpaces . toLower

pipeline :: Text -> Lyrics
pipeline = parseTags >>> mapAccumL discriminate False >>> snd
             >>> catMaybes >>> innerText >>> stripStart >>> Lyrics

discriminate :: Bool -> Tag Text -> (Bool, Maybe (Tag Text))
discriminate onDiv@True tag | isTagText tag = (onDiv, pure tag)
discriminate onDiv tag
  | tagOpenLit "div" (== []) tag  = (True, Nothing)
  | isTagCloseName "div" tag      = (False, Nothing)
  | otherwise                     = (onDiv, Nothing)
