{-# language OverloadedStrings, DataKinds #-}
module MusicScroll.Providers.AZLyrics (azLyricsInstance) where

import qualified Data.Char as C
import           Data.Text (Text)
import           Data.Text as T hiding (filter, tail, map)
import           Network.HTTP.Req
import           Text.HTML.TagSoup

import           MusicScroll.TrackInfo (TrackInfo(..))
import           MusicScroll.Providers.Utils

azLyricsInstance :: Provider
azLyricsInstance = Provider
  { toUrl = toUrl'
  , extractLyricsFromPage = extractLyricsFromPage' }

toUrl' :: TrackInfo -> Url 'Https
toUrl' track =
  let base :: Url 'Https
      base = https "www.azlyrics.com"

      quotedArtist = normalize (tArtist track)
      quotedSong = normalize (tTitle track) <> ".html"
  in base /: "lyrics" /: quotedArtist /: quotedSong

normalize :: Text -> Text
normalize = let noSpaces = replace " " "" in noSpaces . toLower

extractLyricsFromPage' :: Text -> Lyrics
extractLyricsFromPage' page =
  let stream = parseTags page
      pass1  = flip filter stream
        (\t -> (not (isScript t)) && noEmptyText t && validTags t)
      stream2 = zip3 pass1 (tail pass1) (tail (tail pass1))
      pass2   = map (\(t, _, _) -> t) $ filter isStrophe stream2
  in Lyrics . T.unlines . cleanOut $ pass2

-- Pass 1
validTags, noEmptyText :: Tag Text -> Bool
validTags t = isTagOpenName "br" t   || isTagCloseName "div" t
            || isTagOpenName "div" t || isTagCloseName "div" t
            || isTagText t

noEmptyText =
  let invalidChars c = C.isSpace c || c == '\\' || c == 'n'
                       || c == 'r' || c == 't'
  in maybe True (not . T.all invalidChars) . maybeTagText

-- Pass 2
isScript :: Tag Text -> Bool
isScript = let invalidChars c = c == '>' || c == '{'
           in maybe False (T.any invalidChars) . maybeTagText

-- Pass 3
isStrophe :: (Tag Text, Tag Text, Tag Text) -> Bool
isStrophe (TagText _, TagOpen "br" _, TagText _) = True
isStrophe (TagText _, TagOpen "br" _, TagOpen "br" _) = True
isStrophe (TagOpen "br" _, TagOpen "br" _, _) = True -- breakline
isStrophe _ = False

-- Cleaning output
cleanOut :: [Tag Text] -> [Text]
cleanOut =   map (T.strip . T.replace "\\n" "")
           . map (maybe "\n" id . maybeTagText)
