module MusicScroll.LyricsPipeline (lyricsThread, sizeOfQueue) where

-- | Discriminate between getting the lyrics from SQLite or the web.

import Control.Concurrent.Async (async, cancel, concurrently_)
import Control.Concurrent.STM (atomically, orElse)
import Control.Concurrent.STM.TBQueue ( TBQueue, readTBQueue, writeTBQueue,
                                        newTBQueue )
import Control.Applicative (Alternative(..))
import Control.Exception (bracket)
import Control.Monad.Trans.State (StateT, get, put, evalStateT)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Control.Monad (forever, when)
import Numeric.Natural (Natural)
import Data.Maybe (isJust)
import Database.SQLite.Simple

import MusicScroll.DatabaseUtils
import MusicScroll.TrackInfo
import MusicScroll.TrackSuplement
import MusicScroll.Web (getLyricsFromWeb)
import MusicScroll.Providers.AZLyrics (azLyricsInstance)
import MusicScroll.Providers.MusiXMatch (musiXMatchInstance)
import MusicScroll.UIEvent

type TrackQueue = (TBQueue TrackIdentifier, TBQueue TrackSuplement)
type TrackContext a = StateT (Maybe TrackIdentifier) IO a

sizeOfQueue :: Natural
sizeOfQueue = 5

lyricsThread :: TrackQueue -> TBQueue UIEvent -> IO ()
lyricsThread input output =
  do middle <- atomically (newTBQueue sizeOfQueue)
     let seenSongT'  = seenSongsThread input middle
         getLyricsT' = getLyricsThread middle output
     concurrently_ (evalStateT seenSongT' Nothing) getLyricsT'

-- | This thread works as a model of the MVC pattern. The UI
--   communicates its callbacks to here. The Dbus thread sends what it
--   sees here to no repeat songs.
seenSongsThread :: TrackQueue -> TBQueue TrackIdentifier -> TrackContext a
seenSongsThread input output = forever $
  do mTrackIdent <- mergeQueue input
     notSeen <- (/=) <$> get <*> pure mTrackIdent
     when (notSeen && isJust mTrackIdent) $ do
       let Just trackIdent = mTrackIdent
       put mTrackIdent
       liftIO . atomically $ writeTBQueue output trackIdent

mergeQueue :: TrackQueue -> TrackContext (Maybe TrackIdentifier)
mergeQueue (inputIdent, inputSupl) =
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
       flip evalStateT Nothing . forever $
         do trackIdent <- liftIO $ atomically (readTBQueue input)
            get >>= maybe (pure ()) (liftIO . cancel)
            asyncId <- liftIO . async $
              do event <- flip runReaderT conn $
                            either caseByPath caseByInfo trackIdent
                 atomically $ writeTBQueue output event
            put (pure asyncId)

caseByInfo :: TrackInfo -> ReaderT Connection IO UIEvent
caseByInfo track =
  let tryGetLyrics = getDBLyrics (tUrl track)
                     <|> getLyricsFromWeb azLyricsInstance track
                     <|> getLyricsFromWeb musiXMatchInstance track
  in (GotLyric track <$> tryGetLyrics) <|> pure (ErrorOn (NoLyricsOnWeb track))

caseByPath :: TrackByPath -> ReaderT Connection IO UIEvent
caseByPath track =
  ((uncurry GotLyric) <$> getDBSong (tpPath track)) <|>
  pure (ErrorOn (NotOnDB track))
