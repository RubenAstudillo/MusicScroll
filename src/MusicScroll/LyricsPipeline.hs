module MusicScroll.LyricsPipeline (lyricsThread, sizeOfQueue) where

-- | Discriminate between getting the lyrics from SQLite or the web.

import Control.Concurrent.Async (withAsync, waitAnyCancel)
import Control.Concurrent.STM (atomically, orElse)
import Control.Concurrent.STM.TBQueue ( TBQueue, readTBQueue, writeTBQueue,
                                        newTBQueue )
import Control.Applicative (Alternative(..))
import Control.Exception (bracket)
import Control.Monad.Trans.State (StateT, get, put, evalStateT)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ReaderT, runReaderT, ask)
import Control.Monad (forever, when)
import Numeric.Natural (Natural)
import Data.Functor (void)
import Data.Maybe (isJust)
import Database.SQLite.Simple

import MusicScroll.DatabaseUtils 
import MusicScroll.TrackInfo
import MusicScroll.TrackSuplement
import MusicScroll.AZLyrics (getLyricsFromWeb)
import MusicScroll.UIEvent

sizeOfQueue :: Natural
sizeOfQueue = 5

lyricsThread :: TBQueue TrackIdentifier -> TBQueue TrackSuplement
             -> TBQueue UIEvent -> IO ()
lyricsThread inputIdent inputSupl output =
  do songFilterChan <- atomically (newTBQueue sizeOfQueue)
     let seenSongThread' = seenSongsThread inputIdent inputSupl songFilterChan
     withAsync (evalStateT seenSongThread' Nothing) $ \seenSongsA ->
       withAsync (getLyricsThread songFilterChan output) $ \getLyricsA ->
         void $ waitAnyCancel [seenSongsA, getLyricsA]

-- | This thread works as a model of the MVC pattern. The UI
--   communicates its callbacks to here. The Dbus thread sends what it
--   sees here to no repeat songs.
seenSongsThread :: TBQueue TrackIdentifier -> TBQueue TrackSuplement
                -> TBQueue TrackIdentifier -> StateT (Maybe TrackIdentifier) IO a
seenSongsThread inputIdent inputSupl output = forever $
  do mTrackIdent <- mergeQueue inputIdent inputSupl
     notSeen <- (/=) <$> get <*> pure mTrackIdent
     when (notSeen && isJust mTrackIdent) $ do
       let Just trackIdent = mTrackIdent
       put mTrackIdent
       liftIO . atomically $ writeTBQueue output trackIdent

mergeQueue :: TBQueue TrackIdentifier -> TBQueue TrackSuplement
           -> StateT (Maybe TrackIdentifier) IO (Maybe TrackIdentifier)
mergeQueue inputIdent inputSupl =
  do let cleanInputIdent = cleanTrack <$> readTBQueue inputIdent
         mergedInputChan = (Left <$> cleanInputIdent) `orElse`
                           (Right <$> readTBQueue inputSupl)
     mOldTrack   <- get
     mergedInput <- liftIO $ atomically mergedInputChan

     let suplement' :: TrackSuplement -> Maybe TrackIdentifier
         suplement' supl = (Right . suplement supl) <$> mOldTrack
         newTrack :: Maybe TrackIdentifier
         newTrack = either Just suplement' mergedInput

     pure newTrack

getLyricsThread :: TBQueue TrackIdentifier -> TBQueue UIEvent -> IO a
getLyricsThread input output =
  do dbPath <- getDBPath
     bracket (open dbPath) close $ \conn -> do
       execute_ conn sqlDBCreate
       forever $
         do trackIdent <- atomically (readTBQueue input)
            event <- flip runReaderT conn $ either caseByPath caseByInfo trackIdent
            atomically $ writeTBQueue output event

caseByInfo :: TrackInfo -> ReaderT Connection IO UIEvent
caseByInfo track =
  let tryGetLyrics = getDBLyrics (tUrl track) <|> getLyricsFromWeb track
  in (GotLyric track <$> tryGetLyrics) <|> pure (ErrorOn (NoLyricsOnWeb track))

caseByPath :: TrackByPath -> ReaderT Connection IO UIEvent
caseByPath track =
  ((uncurry GotLyric) <$> getDBSong (tpPath track)) <|>
  pure (ErrorOn (NotOnDB track))
