{-# language OverloadedStrings, TypeApplications, RecordWildCards #-}
module MusicScroll.DatabaseUtils
  ( getDBLyrics
  , getDBSong
  , insertDBLyrics
  , getDBPath
  ) where

import           Prelude hiding (null)
import           Control.Applicative (Alternative(..))
import           Control.Exception (bracket, evaluate)
import           Control.DeepSeq (rnf)
import           Crypto.Hash (SHA1, hashUpdate, hashInit, hashFinalize)
import           Data.ByteString (hGet, null)
import           System.IO (withFile, IOMode(..))
import           Data.Text (Text)
import           Database.SQLite.Simple
import           System.Environment.XDG.BaseDir (getUserCacheDir)
import           Data.Coerce
import           System.Directory (createDirectory)

import           MusicScroll.TrackInfo (TrackInfo(..), SongFilePath)
import           MusicScroll.TagParsing (Lyrics(..))

getDBLyrics :: SongFilePath -> IO Lyrics
getDBLyrics songUrl = snd <$> getDBSong songUrl

getDBSong :: SongFilePath -> IO (TrackInfo, Lyrics)
getDBSong songUrl =
  do songHash <- fileHash songUrl
     dbPath   <- getDBPath
     bracket (open dbPath) close $ \conn ->
       do execute_ conn sqlDBCreate
          songRaw <- query conn sqlExtractSong (Only songHash)
          case (songRaw :: [ (Text, Text, Text) ]) of
            [] -> empty
            (title, artist, lyrics):_ ->
              let track = TrackInfo title artist songUrl
              in return (track, coerce lyrics)


insertDBLyrics :: TrackInfo -> Lyrics -> IO ()
insertDBLyrics (TrackInfo {..}) lyrics =
  do songHash <- fileHash tUrl
     dbPath   <- getDBPath
     bracket (open dbPath) close $ \conn ->
       do execute_ conn sqlDBCreate
          let params = (songHash, tArtist, tTitle, coerce lyrics :: Text)
          execute conn sqlInsertSong params

getDBPath :: IO FilePath
getDBPath = do cacheDir <- getUserCacheDir "musicScroll"
               createDirectory cacheDir <|> return ()
               return $ cacheDir ++ "/" ++ "lyrics.db"

-- | We use the exception thrown by withFile.
fileHash :: FilePath -> IO String
fileHash fp = withFile fp ReadMode $ \hdl ->
  let chunkSize = 512 * 1024
      looper ctx =
          do upd <- hGet hdl chunkSize
             if null upd
               then return (show (hashFinalize ctx))
               else do let newCtx = hashUpdate ctx upd
                       evaluate (rnf newCtx) -- Important!
                       looper newCtx
  in looper (hashInit @SHA1)

sqlDBCreate, sqlInsertSong, sqlExtractSong :: Query
sqlDBCreate =
  "create table if not exists MusicScrollTable(\n\
  \  songHash text primary key,\n\
  \  artist text,\n\
  \  title text, \n\
  \  lyrics text );"

sqlInsertSong = "insert into MusicScrollTable values (?, ?, ?, ?);"

sqlExtractSong =
  "select title, artist, lyrics from MusicScrollTable where songHash == ?;"
