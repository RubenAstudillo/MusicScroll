{-# language RecordWildCards, OverloadedStrings #-}
module MusicScroll.UI (uiThread, getSuplement) where

import           Control.Concurrent.STM (atomically)
import           Control.Concurrent.STM.TBQueue (TBQueue, writeTBQueue)
import           Control.Concurrent.STM.TMVar (TMVar, putTMVar)
import           Data.GI.Gtk.Threading (setCurrentThreadAsGUIThread)
import           Data.Maybe (fromJust)
import           Data.Text (pack)
import qualified GI.Gtk as Gtk

import           MusicScroll.TrackSuplement
import           MusicScroll.UIContext
import           MusicScroll.Pipeline
import           MusicScroll.EventLoop

import           Paths_musicScroll


-- Remember to use Gtk.init Nothing before calling this.
getGtkScene :: IO UIContext
getGtkScene = do
  file    <- getDataFileName "app.glade"
  builder <- Gtk.builderNewFromFile (pack file)
  -- We *know* these ids are defined
  let getWidget wid id0 =
        Gtk.builderGetObject builder id0
          >>= Gtk.castTo wid . fromJust >>= return . fromJust
  UIContext <$> getWidget Gtk.Window "mainWindow"
             <*> getWidget Gtk.Label "titleLabel"
             <*> getWidget Gtk.Label "artistLabel"
             <*> getWidget Gtk.TextView "lyricsTextView"
             <*> getWidget Gtk.Label "errorLabel"
             <*> getWidget Gtk.Entry "titleSuplementEntry"
             <*> getWidget Gtk.Entry "artistSuplementEntry"
             <*> getWidget Gtk.Button "suplementAcceptButton"
             <*> getWidget Gtk.CheckButton "keepArtistNameCheck"

uiThread :: TMVar UIContext -> TBQueue UICallback -> IO ()
uiThread ctxMVar outputTB = do
  setCurrentThreadAsGUIThread
  _ <- Gtk.init Nothing
  appCtx@(UIContext {..}) <- getGtkScene
  atomically (putTMVar ctxMVar appCtx)
  Gtk.labelSetText titleLabel "MusicScroll"
  Gtk.widgetShowAll mainWindow
  _ <- Gtk.onButtonClicked suplementAcceptButton $
       do supl <- getSuplement appCtx
          let callback = suplementPipeline supl
          atomically (writeTBQueue outputTB callback)
  _ <- Gtk.onWidgetDestroy mainWindow Gtk.mainQuit
  Gtk.main

getSuplement :: UIContext -> IO TrackSuplement
getSuplement (UIContext {..}) = TrackSuplement <$>
  Gtk.entryGetText titleSuplementEntry <*> Gtk.entryGetText artistSuplementEntry

-- TODO: Recover this functionality
-- tryDefaultSupplement
--   :: UIContext -> ErrorCause -> TBQueue TrackSuplement -> IO ()
-- tryDefaultSupplement ctx@(UIContext {..}) cause suplChan =
--   do shouldMaintainArtistSupl <- Gtk.getToggleButtonActive keepArtistNameCheck
--      validGuessArtist <- (/= mempty) <$> Gtk.entryGetText artistSuplementEntry
--      case cause of
--        OnlyMissingArtist | shouldMaintainArtistSupl, validGuessArtist ->
--                      sendSuplementalInfo ctx suplChan
--        _ -> return ()
