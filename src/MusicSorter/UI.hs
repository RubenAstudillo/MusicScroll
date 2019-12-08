{-# language NamedFieldPuns, RecordWildCards, OverloadedStrings #-}
module MusicSorter.UI
  ( getGtkScene
  , AppContext(..)
  ) where

import qualified GI.Gtk as Gtk
import Data.Text (Text)
import Data.Text as T
import Data.Maybe (fromJust)

import Paths_musicSorter

data AppContext = AppContext
  { mainWindow :: Gtk.Window
  , titleLabel :: Gtk.Label
  , artistLabel :: Gtk.Label
  }

-- Remember to use Gtk.init Nothing before calling this.
getGtkScene :: IO AppContext
getGtkScene = do
  file    <- getDataFileName "app.glade"
  builder <- Gtk.builderNewFromFile (T.pack file)
  -- We *know* these ids are defined
  let getWidget wid id =
        Gtk.builderGetObject builder id
          >>= Gtk.castTo wid . fromJust >>= return . fromJust
  AppContext <$> getWidget Gtk.Window "mainWindow"
             <*> getWidget Gtk.Label "titleLabel"
             <*> getWidget Gtk.Label "artistLabel"

