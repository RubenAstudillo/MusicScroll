{-# language OverloadedStrings, DataKinds, NamedFieldPuns #-}
module MusicSorter.AZLyrics (lyricsThread) where

import           Control.Concurrent.STM (atomically)
import           Control.Concurrent.STM.TBQueue (TBQueue, readTBQueue, writeTBQueue)
import           Control.Monad (forever)
import           Data.Text (Text)
import           Data.Text as T hiding (filter, tail, map)
import           Data.Text.Encoding (decodeUtf8)
import           MusicSorter.MPRIS (TrackInfo(..))
import           MusicSorter.TagParsing
import           Network.HTTP.Req

lyricsThread :: TBQueue TrackInfo -> TBQueue (TrackInfo, [Text]) -> IO a
lyricsThread input output = forever $
  do trackinfo <- atomically (readTBQueue input)
     lyrics <- lyricsPipeline trackinfo
     atomically $ writeTBQueue output (trackinfo, lyrics)

lyricsPipeline :: TrackInfo -> IO [Text]
lyricsPipeline (TrackInfo {tArtist, tTitle}) =
  do let songUrl = url tArtist tTitle
     resp <- getPage songUrl
     if responseStatusCode resp /= 200
       then return [ "Fallo AZLyrics" ]
       else
         do let body = decodeUtf8 (responseBody resp) -- can throw, decode header?
            return (extractLyricsFromPage body)

getPage :: Url 'Https -> IO BsResponse
getPage url = runReq defaultHttpConfig $
  req GET url NoReqBody bsResponse mempty

url :: Text -> Text -> Url 'Https
url artist song =
  let base :: Url Https
      base = https "www.azlyrics.com"

      quotedArtist = normalize artist
      quotedSong = normalize song <> ".html"
  in base /: "lyrics" /: quotedArtist /: quotedSong

normalize :: Text -> Text
normalize = let noSpaces = replace " " "" in noSpaces . toLower
