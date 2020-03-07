{-# language OverloadedStrings, TypeApplications, ScopedTypeVariables,
      RecordWildCards #-}
module MusicScroll.DatabaseUtils
  ( getDBLyrics
  , insertDBLyrics
  , getDBPath
  ) where

import           Prelude hiding (readFile)
import           Control.Applicative (Alternative(..))
import           Control.Exception (bracket)
import           Crypto.Hash (hash, SHA1)
import           Data.ByteString (readFile)
import           Data.Text (Text)
import           Database.SQLite.Simple
import           System.Environment.XDG.BaseDir (getUserCacheDir)
import           Data.Coerce
import           System.Directory (createDirectory)

import           MusicScroll.TrackInfo (TrackInfo(..))
import           MusicScroll.TagParsing (Lyrics(..))

getDBLyrics :: TrackInfo -> IO Lyrics
getDBLyrics track =
  do songHash <- fileHash (tUrl track)
     dbPath   <- getDBPath
     bracket (open dbPath) close $ \conn ->
       do execute_ conn sqlDBCreate
          lyricsRaw <- query conn sqlExtractLyrics (Only songHash)
          case (lyricsRaw :: [ Only Text ]) of
            [] -> empty
            (Only realLyrics):_ -> return (coerce realLyrics)

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

-- | We use the exception thrown by readFile.
fileHash :: FilePath -> IO String
fileHash fp = (show . hash @_ @SHA1) <$> readFile fp

sqlDBCreate, sqlInsertSong, sqlExtractLyrics :: Query
sqlDBCreate =
  "create table if not exists MusicScrollTable(\n\
  \  songHash text primary key,\n\
  \  artist text,\n\
  \  title text, \n\
  \  lyrics text );"

sqlInsertSong = "insert into MusicScrollTable values (?, ?, ?, ?);"

sqlExtractLyrics =
  "select lyrics from MusicScrollTable where songHash == ?;"
