{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module MusicScroll.UI (uiThread, getSuplement) where

import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TBQueue (TBQueue, writeTBQueue)
import Control.Concurrent.STM.TMVar (TMVar, putTMVar)
import Control.Concurrent.STM.TVar (TVar, writeTVar)
import Data.Foldable (for_)
import Data.GI.Gtk.Threading (setCurrentThreadAsGUIThread)
import Data.Maybe (fromJust)
import Data.Text (pack)
import qualified GI.Gtk as Gtk
import MusicScroll.EventLoop
import MusicScroll.Pipeline
import MusicScroll.TrackSuplement
import MusicScroll.UIContext
import Paths_musicScroll

-- Remember to use Gtk.init Nothing before calling this.
getGtkScene :: IO UIContext
getGtkScene = do
  file <- getDataFileName "app.glade"
  builder <- Gtk.builderNewFromFile (pack file)
  -- We *know* these ids are defined
  let getWidget wid id0 =
        Gtk.builderGetObject builder id0
          >>= Gtk.castTo wid . fromJust
          >>= return . fromJust
  UIContext <$> getWidget Gtk.Window "mainWindow"
    <*> getWidget Gtk.Label "titleLabel"
    <*> getWidget Gtk.Label "artistLabel"
    <*> getWidget Gtk.TextView "lyricsTextView"
    <*> getWidget Gtk.Label "errorLabel"
    <*> getWidget Gtk.Entry "titleSuplementEntry"
    <*> getWidget Gtk.Entry "artistSuplementEntry"
    <*> getWidget Gtk.Button "suplementAcceptButton"
    <*> getWidget Gtk.Button "suplementUpdateButton"
    <*> getWidget Gtk.CheckButton "keepArtistNameCheck"

uiThread ::
  TMVar UIContext ->
  TBQueue UICallback ->
  TVar (Maybe TrackSuplement) ->
  IO ()
uiThread ctxMVar outputTB suplTVar = do
  setCurrentThreadAsGUIThread
  _ <- Gtk.init Nothing
  appCtx@(UIContext {..}) <- getGtkScene
  atomically (putTMVar ctxMVar appCtx)
  Gtk.labelSetText titleLabel "MusicScroll"
  Gtk.widgetShowAll mainWindow
  _ <-
    Gtk.onButtonClicked suplementAcceptButton $
      getSuplement appCtx >>= \msupl -> do
        for_ msupl $ \supl ->
          let callback = suplementPipeline supl
           in atomically (writeTBQueue outputTB callback)
        atomically (writeTVar suplTVar msupl)
  _ <-
    Gtk.onButtonClicked suplementUpdateButton $
      getSuplement appCtx >>= \msupl -> do
        for_ msupl $ \supl ->
          let callback = updatePipeline supl
           in atomically (writeTBQueue outputTB callback)
        atomically (writeTVar suplTVar msupl)
  _ <-
    Gtk.afterWidgetFocusOutEvent artistSuplementEntry $
      const (defUpdate appCtx *> pure True)
  _ <- Gtk.afterToggleButtonToggled keepArtistNameCheck $ defUpdate appCtx
  _ <- Gtk.onWidgetDestroy mainWindow Gtk.mainQuit
  Gtk.main
  where
    defUpdate :: UIContext -> IO ()
    defUpdate c = getSuplement c >>= atomically . writeTVar suplTVar

getSuplement :: UIContext -> IO (Maybe TrackSuplement)
getSuplement (UIContext {..}) =
  trackSuplement
    <$> Gtk.entryGetText titleSuplementEntry
    <*> Gtk.entryGetText artistSuplementEntry
    <*> Gtk.getToggleButtonActive keepArtistNameCheck
