{-# language OverloadedStrings, TypeApplications, ScopedTypeVariables,
      RecordWildCards #-}
module MusicScroll.DatabaseUtils
  ( getDBLyrics
  , insertDBLyrics
  , getDBPath
  ) where

import           Prelude hiding (null)
import           Control.Applicative (Alternative(..))
import           Control.Exception (bracket, evaluate)
import           Control.DeepSeq (rnf)
import           Crypto.Hash (hash, SHA1, hashUpdate, hashInit, hashFinalize)
import           Data.Function (fix, (&))
import           Data.ByteString (hGet, null)
import           System.IO (withFile, IOMode(..))
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
