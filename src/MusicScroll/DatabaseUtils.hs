{-# language OverloadedStrings, TypeApplications, RecordWildCards #-}
module MusicScroll.DatabaseUtils where

import           Prelude hiding (null)
import           Control.Applicative (Alternative(..))
import           Control.Exception (evaluate)
import           Control.Monad.Trans.Reader (ReaderT, ask)
import           Control.Monad.IO.Class (MonadIO(..))
import           Control.Concurrent.MVar
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
import           MusicScroll.Providers.Utils (Lyrics(..))

getDBLyrics2 :: SongFilePath -> ReaderT (MVar Connection) IO Lyrics
getDBLyrics2 songUrl = snd <$> getDBSong2 songUrl

getDBSong2 :: SongFilePath -> ReaderT (MVar Connection) IO (TrackInfo, Lyrics)
getDBSong2 songUrl =
  do mconn <- ask
     liftIO $
       do songHash <- fileHash songUrl
          songRaw <- withMVar mconn
                       (\conn -> query conn sqlExtractSong (Only songHash))
          case (songRaw :: [ (Text, Text, Text) ]) of
            [] -> empty
            (title, artist, lyrics):_ ->
              let track = TrackInfo title artist songUrl
              in pure (track, coerce lyrics)

insertDBLyrics2 :: TrackInfo -> Lyrics -> ReaderT (MVar Connection) IO ()
insertDBLyrics2 (TrackInfo {..}) lyrics =
  ask >>= \mconn -> liftIO $
    do songHash <- fileHash tUrl
       let params = (songHash, tArtist, tTitle, coerce lyrics :: Text)
       withMVar mconn $ \conn -> execute conn sqlInsertSong params


updateDBLyrics :: TrackInfo -> Lyrics -> ReaderT Connection IO ()
updateDBLyrics (TrackInfo {..}) lyrics = ask >>= \conn -> liftIO $
  do songHash <- fileHash tUrl
     let params = (coerce lyrics :: Text, songHash)
     execute conn sqlUpdateSong params

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
