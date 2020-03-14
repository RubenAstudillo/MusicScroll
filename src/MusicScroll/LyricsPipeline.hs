{-# language OverloadedStrings #-}
module MusicScroll.LyricsPipeline (lyricsThread) where

-- | Discriminate between getting the lyrics from SQLite or the web.

import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TBQueue (TBQueue, readTBQueue, writeTBQueue)
import Control.Applicative (Alternative(..))
import Control.Monad (forever)

import MusicScroll.DatabaseUtils (getDBLyrics)
import MusicScroll.TrackInfo (TrackInfo(..), cleanTrack)
import MusicScroll.TagParsing
import MusicScroll.AZLyrics (getLyricsFromWeb)
import MusicScroll.UIEvent (UIEvent(GotLyric))

lyricsThread :: TBQueue TrackInfo -> TBQueue UIEvent -> IO a
lyricsThread input output = forever $
  do trackinfo <- cleanTrack <$> atomically (readTBQueue input)
     lyrics <- getDBLyrics trackinfo <|> getLyricsFromWeb trackinfo
               <|> noLyricsMsg
     atomically $ writeTBQueue output (GotLyric trackinfo lyrics)

noLyricsMsg :: IO Lyrics
noLyricsMsg = return "I failed at getting the lyrics!"
