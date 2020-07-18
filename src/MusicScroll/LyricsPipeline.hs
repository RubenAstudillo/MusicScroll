module MusicScroll.LyricsPipeline
  ( SongByOrigin(..)
  , SearchResult(..)
  , ErrorCause(..)
  , noRepeatedSongs
  , getLyricsFromAnywhere
  , getLyricsOnlyFromWeb
  , saveOnDb
  ) where

-- | Discriminate between getting the lyrics from SQLite or the web.

import Control.Concurrent.MVar
import Control.Applicative (Alternative(..))
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Database.SQLite.Simple
import Pipes
import qualified Pipes.Prelude as PP

import MusicScroll.DatabaseUtils
import MusicScroll.TrackInfo
import MusicScroll.Web
import MusicScroll.Providers.Utils (Lyrics(..))
import MusicScroll.Providers.AZLyrics (azLyricsInstance)
import MusicScroll.Providers.MusiXMatch (musiXMatchInstance)

data SongByOrigin = DB | Web deriving (Show)
data SearchResult = GotLyric SongByOrigin TrackInfo Lyrics
                  | ErrorOn ErrorCause
  deriving (Show)

data ErrorCause = NotOnDB TrackByPath | NoLyricsOnWeb TrackInfo | ENoSong
  deriving (Show)

noRepeatedSongs :: Functor m => Pipe TrackIdentifier TrackIdentifier m a
noRepeatedSongs = do firstSong <- await
                     yield firstSong
                     loop firstSong
  where
    loop prevSong = do newSong <- await
                       if (TIWE newSong) /= (TIWE prevSong)
                         then yield newSong *> loop newSong
                         else loop prevSong

getLyricsFromAnywhere :: MVar Connection -> Pipe TrackIdentifier SearchResult IO a
getLyricsFromAnywhere connMvar = PP.mapM go
  where go :: TrackIdentifier -> IO SearchResult
        go ident = runReaderT (either caseByPath caseByInfoGeneral ident) connMvar

getLyricsOnlyFromWeb :: Pipe TrackInfo SearchResult IO a
getLyricsOnlyFromWeb = PP.mapM caseByInfoWeb

caseByInfoGeneral :: TrackInfo -> ReaderT (MVar Connection) IO SearchResult
caseByInfoGeneral track =
  let local = uncurry (GotLyric DB) <$> getDBSong (tUrl track)
      web = caseByInfoWeb track
      err = pure (ErrorOn (NoLyricsOnWeb track))
  in local <|> web <|> err

caseByInfoWeb :: (MonadIO m, Alternative m) => TrackInfo -> m SearchResult
caseByInfoWeb track = GotLyric Web track <$>
  (getLyricsFromWeb azLyricsInstance track
   <|> getLyricsFromWeb musiXMatchInstance track)

caseByPath :: TrackByPath -> ReaderT (MVar Connection) IO SearchResult
caseByPath track =
  ((uncurry (GotLyric DB)) <$> getDBSong (tpPath track)) <|>
  pure (ErrorOn (NotOnDB track))

saveOnDb :: MVar Connection -> InsertStategy -> Pipe SearchResult SearchResult IO a
saveOnDb mconn strat = PP.chain go
  where go :: SearchResult -> IO ()
        go (GotLyric Web info lyr) = runReaderT (strat info lyr) mconn
        go _otherwise = pure ()
