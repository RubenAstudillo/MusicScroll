{-# language RecordWildCards, OverloadedStrings, OverloadedLabels #-}
module MusicScroll.UI (setupUIThread) where

import           Control.Monad.IO.Class (MonadIO(..))
import           Control.Concurrent.Async
                     (withAsyncBound, waitAnyCancel, withAsync)
import           Control.Concurrent.STM (atomically)
import           Control.Concurrent.STM.TBQueue (TBQueue, readTBQueue, writeTBQueue)
import           Control.Concurrent.STM.TMVar
                     (TMVar, newEmptyTMVar, takeTMVar, putTMVar)
import           Control.Exception (throwIO, AsyncException(UserInterrupt))
import           Control.Monad (forever)
import           Data.Functor (void)
import           Data.GI.Gtk.Threading (setCurrentThreadAsGUIThread)
import           Data.Text (pack, Text)
import qualified GI.Gtk as Gtk

import           Reactive.Banana.Frameworks
import           Reactive.Banana.Combinators
import           Reactive.Banana.GI.Gtk
import           Control.Event.Handler

import           MusicScroll.TrackSuplement
import           MusicScroll.TrackInfo
import           MusicScroll.UIEvent
import           MusicScroll.Providers.Utils

import           Paths_musicScroll

searchSongSignal :: Gtk.Builder
                 -> MomentIO (Event TrackSuplement, Behavior TrackSuplement)
searchSongSignal b = do
  titleSupW <- castB b "titleSuplementEntry" Gtk.Entry
  titleB <- attrB titleSupW #text
  artistSupW <- castB b "artistSuplementEntry" Gtk.Entry
  artistB <- attrB artistSupW #text
  supAcceptButton <- castB b "suplementAcceptButton" Gtk.Button
  clickedE <- signalE0 supAcceptButton #clicked
  let supl = TrackSuplement <$> titleB <*> artistB
  return $ (supl <@ clickedE, supl)

splitDataSignal :: Event UIEvent -> (Event (TrackInfo, Lyrics), Event ErrorCause)
splitDataSignal =
  let tag :: UIEvent -> Either (TrackInfo, Lyrics) ErrorCause
      tag (GotLyric track lyrics) = Left (track, lyrics)
      tag (ErrorOn cause) = Right cause
  in split . fmap tag

networkDescription :: AddHandler UIEvent -> TBQueue TrackSuplement -> MomentIO ()
networkDescription addHandle suplChan = do
  file <- liftIO $ getDataFileName "app.glade"
  builder <- Gtk.builderNewFromFile (pack file)

  mainWindow <- castB builder "mainWindow" Gtk.Window
  destroyE <- signalE0 mainWindow #destroy
  reactimate $ Gtk.mainQuit <$ destroyE

  titleL <- castB builder "titleLabel" Gtk.Label
  artistL <- castB builder "artistLabel" Gtk.Label
  errorL <- castB builder "errorLabel" Gtk.Label
  lyricsTV <- castB builder "lyricsTextView" Gtk.TextView
  lyricsTB <- castB builder "lyricsTextView" Gtk.TextView >>= Gtk.textViewGetBuffer
  -- liftIO $ Gtk.labelSetText titleL "MusicScroll"

  dataE <- fromAddHandler addHandle
  let (songE, errorE) = splitDataSignal dataE
  -- let lyricsE = coerce . snd <$> songE
  titleB  <- stepper "MusicScroll" (getTitleB <$> dataE)
  artistB <- stepper "" (getArtistB <$> dataE)
  errorB  <- stepper "" (getErrorB <$> dataE)
  lyricsB <- stepper "" (getLyricsB <$> dataE)
  reactimate $ Gtk.labelSetText titleL . getTitleB <$> dataE
  reactimate $ Gtk.labelSetText artistL . getArtistB <$> dataE
  reactimate $ Gtk.labelSetText errorL . getErrorB <$> dataE
  reactimate $ updateNewLyrics lyricsTV <$> songE
  sink titleL [#label :== titleB]
  sink artistL [#label :== artistB]
  sink errorL [#label :== errorB]
  sink lyricsTB [#text :== lyricsB]

  (searchE, trackSuplB) <- searchSongSignal builder
  reactimate $ (atomically . writeTBQueue suplChan) <$> searchE

  Gtk.widgetShowAll mainWindow

--  <$> getWidget Gtk.Window "mainWindow"
--  <*> getWidget Gtk.Label "titleLabel"
--  <*> getWidget Gtk.Label "artistLabel"
--  <*> getWidget Gtk.TextView "lyricsTextView"
--  <*> getWidget Gtk.Label "errorLabel"
--  <*> getWidget Gtk.Entry "titleSuplementEntry"
--  <*> getWidget Gtk.Entry "artistSuplementEntry"
--  <*> getWidget Gtk.Button "suplementAcceptButton"
--  <*> getWidget Gtk.CheckButton "keepArtistNameCheck"

setupUIThread :: TBQueue UIEvent -> TBQueue TrackSuplement -> IO ()
setupUIThread events outSupl =
  do (add, fire) <- newAddHandler
     withAsyncBound (uiThread add outSupl) $ \a1 ->
       withAsync (uiUpdateThread events fire) $ \a2 ->
         void (waitAnyCancel [a1, a2]) >> throwIO UserInterrupt

uiThread :: AddHandler UIEvent -> TBQueue TrackSuplement -> IO ()
uiThread add outSupl = do
  setCurrentThreadAsGUIThread
  _ <- Gtk.init Nothing
  compile (networkDescription add outSupl) >>= actuate
  Gtk.main

uiUpdateThread :: TBQueue UIEvent -> Handler UIEvent -> IO a
uiUpdateThread input fire = forever $ atomically (readTBQueue input) >>= fire

-- uiUpdateThread :: TBQueue UIEvent -> TBQueue TrackSuplement
--                -> TMVar AppContext -> IO a
-- uiUpdateThread input outSupl ctxMVar = do
--   appCtx <- atomically (takeTMVar ctxMVar)
--   forever $ do
--     event <- atomically (readTBQueue input)
--     case event of
--       GotLyric track lyrics -> updateNewLyrics appCtx (track, lyrics)
--       ErrorOn cause -> updateErrorCause appCtx cause
--                          *> tryDefaultSupplement appCtx cause outSupl

-- sendSuplementalInfo :: AppContext -> TBQueue TrackSuplement -> IO ()
-- sendSuplementalInfo (AppContext {..}) suplChan =
--   do trackSupl <- TrackSuplement <$> Gtk.entryGetText titleSuplementEntry
--                                  <*> Gtk.entryGetText artistSuplementEntry
--      atomically (writeTBQueue suplChan trackSupl)

-- tryDefaultSupplement
--   :: AppContext -> ErrorCause -> TBQueue TrackSuplement -> IO ()
-- tryDefaultSupplement ctx@(AppContext {..}) cause suplChan =
--   do shouldMaintainArtistSupl <- Gtk.getToggleButtonActive keepArtistNameCheck
--      validGuessArtist <- (/= mempty) <$> Gtk.entryGetText artistSuplementEntry
--      case cause of
--        OnlyMissingArtist | shouldMaintainArtistSupl, validGuessArtist ->
--                      sendSuplementalInfo ctx suplChan
--        _ -> return ()
