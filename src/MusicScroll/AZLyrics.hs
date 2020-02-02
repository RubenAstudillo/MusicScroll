{-# language OverloadedStrings, DataKinds, NamedFieldPuns,
             TypeApplications #-}
module MusicScroll.AZLyrics (lyricsThread) where

import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TBQueue (TBQueue, readTBQueue, writeTBQueue)
import Control.Exception (try, SomeException)
import Control.Monad (forever)
import Data.Text (Text)
import Data.Text as T hiding (filter, tail, map)
import Data.Text.Encoding (decodeUtf8)
import MusicScroll.TrackInfo (TrackInfo(..), cleanTrack)
import MusicScroll.TagParsing
import Network.HTTP.Req

lyricsThread :: TBQueue TrackInfo -> TBQueue (TrackInfo, [Text]) -> IO a
lyricsThread input output = forever $
  do trackinfo <- cleanTrack <$> atomically (readTBQueue input)
     lyrics <- lyricsPipeline trackinfo
     atomically $ writeTBQueue output (trackinfo, lyrics)

lyricsPipeline :: TrackInfo -> IO [Text]
lyricsPipeline (TrackInfo {tArtist, tTitle}) =
  do let songUrl = url tArtist tTitle
     resp <- try @SomeException (getPage songUrl)
     let notValid = either (const True)
                      ((/= 200) . responseStatusCode) resp
     if notValid then return [ "I failed at getting the lyrics!" ]
       else let Right realResp = resp
                body = decodeUtf8 (responseBody realResp)
            in return (extractLyricsFromPage body)

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
