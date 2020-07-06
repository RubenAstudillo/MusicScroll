module MusicScroll.LyricsPipeline where

-- | Discriminate between getting the lyrics from SQLite or the web.

import Control.Concurrent.MVar
import Control.Applicative (Alternative(..))
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Database.SQLite.Simple
import Pipes
import qualified Pipes.Prelude as PP

import MusicScroll.DatabaseUtils
import MusicScroll.TrackInfo
import MusicScroll.TrackSuplement
import MusicScroll.Web
import MusicScroll.Providers.AZLyrics (azLyricsInstance)
import MusicScroll.Providers.MusiXMatch (musiXMatchInstance)
import MusicScroll.UIEvent

noRepeatedFilter :: Functor m => Pipe TrackIdentifier TrackIdentifier m a
noRepeatedFilter = do firstSong <- await
                      yield firstSong
                      loop firstSong
  where
    loop prevSong = do newSong <- await
                       if newSong /= prevSong
                         then yield newSong *> loop newSong
                         else loop prevSong

cleanTrackP :: Functor m => Pipe TrackIdentifier TrackIdentifier m a
cleanTrackP = PP.map cleanTrack

mergeTrackSupl :: Functor m => TrackSuplement -> Pipe TrackIdentifier TrackInfo m a
mergeTrackSupl supl = PP.map (suplement supl)

getLyricsP :: MVar Connection -> Pipe TrackIdentifier SearchResult IO a
getLyricsP connMvar = PP.mapM go
  where go :: TrackIdentifier -> IO SearchResult
        go ident = runReaderT (either caseByPath2 caseByInfoGeneral ident) connMvar

getLyricsFromWebP :: Pipe TrackInfo SearchResult IO a
getLyricsFromWebP = PP.mapM caseByInfoWeb

caseByInfoGeneral :: TrackInfo -> ReaderT (MVar Connection) IO SearchResult
caseByInfoGeneral track =
  let local = caseByInfoLocal track
      web = caseByInfoWeb track
      err = pure (ErrorOn2 (NoLyricsOnWeb track))
  in local <|> web <|> err

caseByInfoWebP :: (MonadIO m, Alternative m) => TrackInfo -> m SearchResult
caseByInfoWebP track =
  let web = caseByInfoWeb track
      err = pure (ErrorOn2 (NoLyricsOnWeb track))
  in web <|> err

caseByInfoLocal :: TrackInfo -> ReaderT (MVar Connection) IO SearchResult
caseByInfoLocal track =
  GotLyric2 DB track <$> getDBLyrics2 (tUrl track)

caseByInfoWeb :: (MonadIO m, Alternative m) => TrackInfo -> m SearchResult
caseByInfoWeb track = GotLyric2 Web track <$>
  (getLyricsFromWeb2 azLyricsInstance track
   <|> getLyricsFromWeb2 musiXMatchInstance track)

caseByPath2 :: TrackByPath -> ReaderT (MVar Connection) IO SearchResult
caseByPath2 track =
  ((uncurry (GotLyric2 DB)) <$> getDBSong2 (tpPath track)) <|>
  pure (ErrorOn2 (NotOnDB track))

saveOnDb :: MVar Connection -> Pipe SearchResult SearchResult IO a
saveOnDb mconn = PP.chain go
  where go :: SearchResult -> IO ()
        go (GotLyric2 Web info lyr) = runReaderT (insertDBLyrics2 info lyr) mconn
        go _otherwise = pure ()
