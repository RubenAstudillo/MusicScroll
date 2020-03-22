{-# language RecordWildCards, OverloadedStrings #-}
module MusicScroll.UI (setupUIThread) where

import           Control.Concurrent.Async
                     (withAsyncBound, waitAnyCancel, withAsync)
import           Control.Concurrent.STM (atomically)
import           Control.Concurrent.STM.TBQueue (TBQueue, readTBQueue, writeTBQueue)
import           Control.Concurrent.STM.TMVar
                     (TMVar, newEmptyTMVar, takeTMVar, putTMVar)
import           Control.Exception (throwIO, AsyncException(UserInterrupt))
import           Control.Monad (forever)
import           Data.Functor (void)
import           Data.Maybe (isNothing)
import           Data.GI.Gtk.Threading (setCurrentThreadAsGUIThread)
import           Data.Maybe (fromJust)
import           Data.Text (pack)
import qualified GI.Gtk as Gtk

import           MusicScroll.TrackInfo (TrackByPath(tpArtist))
import           MusicScroll.TrackSuplement
import           MusicScroll.UIEvent

import           Paths_musicScroll


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
  Gtk.labelSetText titleLabel "MusicScroll"
  Gtk.widgetShowAll mainWindow
  _ <- Gtk.onButtonClicked suplementAcceptButton $
         sendSuplementalInfo appCtx outSupl
  _ <- Gtk.onWidgetDestroy mainWindow Gtk.mainQuit
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
       NotOnDB track
         | isNothing (tpArtist track), shouldMaintainArtistSupl,
           validGuessArtist -> sendSuplementalInfo ctx suplChan
       otherwise -> return ()
