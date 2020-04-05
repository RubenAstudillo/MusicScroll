{-# language OverloadedStrings, DataKinds #-}
module MusicScroll.Providers.MusiXMatch (musiXMatchInstance) where

import Control.Category hiding ((.))
import Data.Maybe (catMaybes)
import Data.Text (Text, replace, toTitle)
import Network.HTTP.Req
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Match (tagOpenLit, anyAttr)
import Data.Traversable (mapAccumL)
import qualified Data.Set as Set
-- import Data.Text.IO as T (readFile)

import MusicScroll.TrackInfo (TrackInfo(..))
import MusicScroll.Providers.Utils

musiXMatchInstance :: Provider
musiXMatchInstance = Provider
  { toUrl = toUrl'
  , extractLyricsFromPage = pipeline }

toUrl' :: TrackInfo -> Url 'Https
toUrl' track =
  let base :: Url 'Https
      base = https "www.musixmatch.com"

      quotedArtist = normalize (tArtist track)
      quotedSong = normalize (tTitle track)
  in base /: "lyrics" /: quotedArtist /: quotedSong

normalize :: Text -> Text
normalize = let noSpaces = replace " " "-" in noSpaces . toTitle

-- exampleTrack = TrackInfo "hey jude" "the beatles" "/home"

-- testOnFile fp =
--   do contents <- T.readFile fp
--      return (pipeline contents)

pipeline :: Text -> Lyrics
pipeline = parseTags >>> mapAccumL discriminate False
             >>> snd >>> catMaybes >>> innerText >>> Lyrics

--
discriminate :: Bool -> Tag Text -> (Bool, Maybe (Tag Text))
discriminate onSpan@True tag | isTagText tag = (onSpan, pure tag)
discriminate onSpan tag
  | tagOpenLit "span" goodClasses tag = (True, Nothing)
  | isTagCloseName "span" tag         = (False, Nothing)
  | otherwise                         = (onSpan, Nothing)
  where
    goodClasses = anyAttr (\attr -> Set.member attr spanDiscr)
    spanDiscr = Set.fromList [ ("class", "lyrics__content__ok")
                             , ("class", "lyrics__content__warning") ]

