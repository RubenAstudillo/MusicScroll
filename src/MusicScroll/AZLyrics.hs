{-# language OverloadedStrings, DataKinds, NamedFieldPuns,
             TypeApplications #-}
module MusicScroll.AZLyrics (getLyricsFromWeb) where

import Control.Exception (try, SomeException)
import Control.Applicative (Alternative(empty))
import Data.Text (Text)
import Data.Text as T hiding (filter, tail, map, empty)
import Data.Text.Encoding (decodeUtf8)
import Network.HTTP.Req

import MusicScroll.TrackInfo (TrackInfo(..))
import MusicScroll.TagParsing
import MusicScroll.DatabaseUtils (insertDBLyrics)

getLyricsFromWeb :: TrackInfo -> IO Lyrics
getLyricsFromWeb track@(TrackInfo {tArtist, tTitle}) =
  do let songUrl = url tArtist tTitle
     resp <- try @SomeException (getPage songUrl)
     let notValid = either (const True)
                      ((/= 200) . responseStatusCode) resp
     if notValid then empty
       else let Right realResp = resp
                body   = decodeUtf8 (responseBody realResp)
                lyrics = extractLyricsFromPage body
            in insertDBLyrics track lyrics *> return lyrics

getPage :: Url 'Https -> IO BsResponse
getPage url = runReq defaultHttpConfig $
  req GET url NoReqBody bsResponse mempty

url :: Text -> Text -> Url 'Https
url artist song =
  let base :: Url 'Https
      base = https "www.azlyrics.com"

      quotedArtist = normalize artist
      quotedSong = normalize song <> ".html"
  in base /: "lyrics" /: quotedArtist /: quotedSong

normalize :: Text -> Text
normalize = let noSpaces = replace " " "" in noSpaces . toLower
