{-# language NamedFieldPuns, RecordWildCards, OverloadedStrings #-}
module MusicSorter.RealMain (guiThread) where

import MusicSorter.UI
import qualified GI.Gtk as Gtk
import qualified Control.Concurrent as Conc

guiThread :: IO ()
guiThread = do
  _ <- Gtk.init Nothing
  AppContext {..} <- getGtkScene
  Gtk.labelSetText titleLabel "Hola Mundo"
  Gtk.widgetShowAll mainWindow
  Gtk.onWidgetDestroy mainWindow Gtk.mainQuit
  Gtk.main


realMain :: IO ()
realMain = guiThread

