{-# language RecordWildCards, OverloadedStrings #-}
module MusicScroll.UI (setupUIThread, uiThread2) where

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
import           Data.Text (pack)
import qualified GI.Gtk as Gtk

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
     withAsyncBound (uiThread appCtxMVar outSupl) $ \a1 -> do
       appCtx <- atomically (takeTMVar appCtxMVar)
       withAsync (uiUpdateThread appCtx events outSupl) $ \a2 ->
         void (waitAnyCancel [a1, a2]) >> throwIO UserInterrupt

-- setupUIThread2 :: TMVar AppContext -> IO ()
-- setupUIThread2 appCtxMVar = withAsyncBound (uiThread appCtxMVar) $ \a1 ->
--   do appCtx <- atomically (takeTMVar appCtxMVar)
--      withAsync (uiUpdateThread appCtx events outSupl) $ \a2 ->
--        void (waitAnyCancel [a1, a2]) >> throwIO UserInterrupt

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

uiThread2 :: TMVar AppContext -> IO ()
uiThread2 ctxMVar = do
  setCurrentThreadAsGUIThread
  _ <- Gtk.init Nothing
  appCtx@(AppContext {..}) <- getGtkScene
  atomically (putTMVar ctxMVar appCtx)
  Gtk.labelSetText titleLabel "MusicScroll"
  Gtk.widgetShowAll mainWindow
  -- _ <- Gtk.onButtonClicked suplementAcceptButton $
  --        sendSuplementalInfo appCtx outSupl
  _ <- Gtk.onWidgetDestroy mainWindow Gtk.mainQuit
  Gtk.main

---
uiUpdateThread :: AppContext -> TBQueue UIEvent -> TBQueue TrackSuplement
               -> IO a
uiUpdateThread appCtx input outSupl =
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
