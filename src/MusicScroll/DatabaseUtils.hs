{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module MusicScroll.DatabaseUtils
  ( getDBLyrics,
    getDBSong,
    sqlDBCreate,
    InsertStategy,
    insertStrat,
    updateStrat,
    getDBPath,
  )
where

import Control.Applicative (Alternative (..))
import Control.Concurrent.MVar
import Control.DeepSeq (rnf)
import Control.Exception (evaluate)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Reader (ReaderT, ask)
import Crypto.Hash (SHA1, hashFinalize, hashInit, hashUpdate)
import Data.ByteString (hGet, null)
import Data.Coerce
import Data.Text (Text)
import Database.SQLite.Simple
import MusicScroll.Providers.Utils (Lyrics (..))
import MusicScroll.TrackInfo (SongFilePath, TrackInfo (..))
import System.Directory (createDirectory)
import System.Environment.XDG.BaseDir (getUserCacheDir)
import System.IO (IOMode (..), withFile)
import Prelude hiding (null)

getDBLyrics :: SongFilePath -> ReaderT (MVar Connection) IO Lyrics
getDBLyrics songUrl = snd <$> getDBSong songUrl

getDBSong :: SongFilePath -> ReaderT (MVar Connection) IO (TrackInfo, Lyrics)
getDBSong songUrl =
  ask >>= \mconn -> liftIO $
    do
      songHash <- fileHash songUrl
      songRaw <-
        withMVar
          mconn
          (\conn -> query conn sqlExtractSong (Only songHash))
      case (songRaw :: [(Text, Text, Text)]) of
        [] -> empty
        (title, artist, lyrics) : _ ->
          let track = TrackInfo title artist songUrl
           in pure (track, coerce lyrics)

type InsertStategy = TrackInfo -> Lyrics -> ReaderT (MVar Connection) IO ()

insertStrat :: InsertStategy
insertStrat (TrackInfo {..}) lyrics =
  ask >>= \mconn -> liftIO $
    do
      songHash <- fileHash tUrl
      let params = (songHash, tArtist, tTitle, coerce lyrics :: Text)
      withMVar mconn $ \conn -> execute conn sqlInsertSong params

updateStrat :: InsertStategy
updateStrat (TrackInfo {..}) lyrics =
  ask >>= \mconn -> liftIO $
    do
      songHash <- fileHash tUrl
      let params = (coerce lyrics :: Text, songHash)
      withMVar mconn $ \conn -> execute conn sqlUpdateSong params

getDBPath :: IO FilePath
getDBPath = do
  cacheDir <- getUserCacheDir "musicScroll"
  createDirectory cacheDir <|> return ()
  return $ cacheDir ++ "/" ++ "lyrics.db"

-- | We use the exception thrown by withFile.
fileHash :: FilePath -> IO String
fileHash fp = withFile fp ReadMode $ \hdl ->
  let chunkSize = 512 * 1024
      looper ctx =
        do
          upd <- hGet hdl chunkSize
          if null upd
            then return (show (hashFinalize ctx))
            else do
              let newCtx = hashUpdate ctx upd
              evaluate (rnf newCtx) -- Important!
              looper newCtx
   in looper (hashInit @SHA1)

sqlDBCreate, sqlInsertSong, sqlExtractSong, sqlUpdateSong :: Query
sqlDBCreate =
  "create table if not exists MusicScrollTable(\n\
  \  songHash text primary key,\n\
  \  artist text,\n\
  \  title text, \n\
  \  lyrics text );"
sqlInsertSong = "insert into MusicScrollTable values (?, ?, ?, ?);"
sqlExtractSong =
  "select title, artist, lyrics from MusicScrollTable where songHash == ?;"
sqlUpdateSong =
  "update MusicScrollTable set lyrics = ? where songHash = ? ;"
