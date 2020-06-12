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
import           Data.Maybe (fromJust)
import           Data.Text (pack, Text)
import qualified GI.Gtk as Gtk

import           Reactive.Banana.Frameworks
import           Reactive.Banana.Combinators
import           Reactive.Banana.GI.Gtk

import           MusicScroll.TrackSuplement
import           MusicScroll.UIEvent

import           Paths_musicScroll

searchSongSignal :: Gtk.Builder -> MomentIO (Event TrackSuplement)
searchSongSignal b = do
  titleSupW <- castB b "titleSuplementEntry" Gtk.Entry
  titleB <- attrB titleSupW #text
  artistSupW <- castB b "artistSuplementEntry" Gtk.Entry
  artistB <- attrB artistSupW #text
  supAcceptButton <- castB b "suplementAcceptButton" Gtk.Button
  clickedE <- signalE0 supAcceptButton #clicked
  let supl = TrackSuplement <$> titleB <*> artistB
  return $ supl <@ clickedE

networkDescription :: TBQueue TrackSuplement -> MomentIO ()
networkDescription suplChan = do
  file <- liftIO $ getDataFileName "app.glade"
  builder <- Gtk.builderNewFromFile (pack file)

  mainWindow <- castB builder "mainWindow" Gtk.Window
  destroyE <- signalE0 mainWindow #destroy
  reactimate $ Gtk.mainQuit <$ destroyE

  titleL <- castB builder "titleLabel" Gtk.Label
  liftIO $ Gtk.labelSetText titleL "MusicScroll"

  searchE <- searchSongSignal builder
  reactimate $ (atomically . writeTBQueue suplChan) <$> searchE

  Gtk.widgetShowAll mainWindow

-- Remember to use Gtk.init Nothing before calling this.
getGtkScene :: IO AppContext
getGtkScene = do
  file    <- getDataFileName "app.glade"
  builder <- Gtk.builderNewFromFile (pack file)
  -- We *know* these ids are defined
  let getWidget wid id =
        Gtk.builderGetObject builder id
          >>= Gtk.castTo wid . fromJust >>= return . fromJust
  AppContext <$> getWidget Gtk.Window "mainWindow"
             <*> getWidget Gtk.Label "titleLabel"
             <*> getWidget Gtk.Label "artistLabel"
             <*> getWidget Gtk.TextView "lyricsTextView"
             <*> getWidget Gtk.Label "errorLabel"
             <*> getWidget Gtk.Entry "titleSuplementEntry"
             <*> getWidget Gtk.Entry "artistSuplementEntry"
             <*> getWidget Gtk.Button "suplementAcceptButton"
             <*> getWidget Gtk.CheckButton "keepArtistNameCheck"

setupUIThread :: TBQueue UIEvent -> TBQueue TrackSuplement -> IO ()
setupUIThread events outSupl =
  do appCtxMVar <- atomically newEmptyTMVar
     withAsyncBound (uiThread appCtxMVar outSupl) $ \a1 ->
       withAsync (uiUpdateThread events outSupl appCtxMVar) $ \a2 ->
         void (waitAnyCancel [a1, a2]) >> throwIO UserInterrupt

uiThread :: TMVar AppContext -> TBQueue TrackSuplement -> IO ()
uiThread ctxMVar outSupl = do
  setCurrentThreadAsGUIThread
  _ <- Gtk.init Nothing
  appCtx@(AppContext {..}) <- getGtkScene
  atomically (putTMVar ctxMVar appCtx)
  compile (networkDescription outSupl) >>= actuate
  Gtk.main

---
uiUpdateThread :: TBQueue UIEvent -> TBQueue TrackSuplement
               -> TMVar AppContext -> IO a
uiUpdateThread input outSupl ctxMVar = do
  appCtx <- atomically (takeTMVar ctxMVar)
  forever $ do
    event <- atomically (readTBQueue input)
    case event of
      GotLyric track lyrics -> updateNewLyrics appCtx (track, lyrics)
      ErrorOn cause -> updateErrorCause appCtx cause
                         *> tryDefaultSupplement appCtx cause outSupl

sendSuplementalInfo :: AppContext -> TBQueue TrackSuplement -> IO ()
sendSuplementalInfo (AppContext {..}) suplChan =
  do trackSupl <- TrackSuplement <$> Gtk.entryGetText titleSuplementEntry
                                 <*> Gtk.entryGetText artistSuplementEntry
     atomically (writeTBQueue suplChan trackSupl)

tryDefaultSupplement
  :: AppContext -> ErrorCause -> TBQueue TrackSuplement -> IO ()
tryDefaultSupplement ctx@(AppContext {..}) cause suplChan =
  do shouldMaintainArtistSupl <- Gtk.getToggleButtonActive keepArtistNameCheck
     validGuessArtist <- (/= mempty) <$> Gtk.entryGetText artistSuplementEntry
     case cause of
       OnlyMissingArtist | shouldMaintainArtistSupl, validGuessArtist ->
                     sendSuplementalInfo ctx suplChan
       _ -> return ()
