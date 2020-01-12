{-# language NamedFieldPuns, RecordWildCards, OverloadedStrings, BangPatterns #-}
module MusicSorter.UI (setupUIThread) where

import Control.Monad (forever)
import Control.Concurrent.Async (Async, withAsyncBound, withAsync)
import qualified Control.Concurrent.Async as A
import Control.Concurrent.STM (STM, atomically)
import Control.Concurrent.STM.TBQueue (TBQueue, readTBQueue)
import Data.GI.Gtk.Threading (setCurrentThreadAsGUIThread, postGUISync)
import Data.Functor (void)
import Data.Maybe (fromJust)
import Data.Text (Text)
import Data.Text as T
import qualified GI.Gtk as Gtk
import MusicSorter.MPRIS (TrackInfo(..))

import Paths_musicSorter

data AppContext = AppContext
  { mainWindow     :: Gtk.Window
  , titleLabel     :: Gtk.Label
  , artistLabel    :: Gtk.Label
  , lyricsTextView :: Gtk.TextView
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
             <*> getWidget Gtk.TextView "lyricsTextView"

setupUIThread :: TBQueue (TrackInfo, [Text]) -> IO ()
setupUIThread trackUpdates =
  do appCtx <- getGtkScene
     withAsyncBound (uiThread appCtx) $ \a1 ->
       A.withAsync (uiUpdateThread trackUpdates appCtx) $ \a2 ->
         void $ A.wait a1 <* A.wait a2

uiThread :: AppContext -> IO ()
uiThread AppContext {..} = do
  setCurrentThreadAsGUIThread
  _ <- Gtk.init Nothing
  -- AppContext {..} <- getGtkScene
  Gtk.labelSetText titleLabel "Hola Mundo"
  Gtk.widgetShowAll mainWindow
  Gtk.onWidgetDestroy mainWindow Gtk.mainQuit
  Gtk.main

---

uiUpdateThread :: TBQueue (TrackInfo, [Text]) -> AppContext -> IO a
uiUpdateThread input appCtx = forever $
  do (track, lyrics) <- atomically (readTBQueue input)
     let !lyricsViewRef  = lyricsTextView appCtx
         !singleLyrics  = T.unlines lyrics
         !bytesToUpdate = fromIntegral $ T.length singleLyrics
     postGUISync $ do
       lyricsBuffer <- Gtk.textViewGetBuffer lyricsViewRef
       Gtk.textBufferSetText lyricsBuffer singleLyrics bytesToUpdate

