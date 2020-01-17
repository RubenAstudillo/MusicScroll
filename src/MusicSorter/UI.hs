{-# language NamedFieldPuns, RecordWildCards, OverloadedStrings,
  BangPatterns #-}
module MusicSorter.UI (setupUIThread) where

import           Control.Concurrent.Async (withAsyncBound)
import qualified Control.Concurrent.Async as A
import           Control.Concurrent.STM (atomically)
import           Control.Concurrent.STM.TBQueue (TBQueue, readTBQueue)
import           Control.Concurrent.STM.TMVar (TMVar, newEmptyTMVar,
                                               takeTMVar, putTMVar)
import           Control.Monad (forever)
import           Data.Functor (void)
import           Data.GI.Gtk.Threading (setCurrentThreadAsGUIThread,
                                        postGUISync)
import           Data.Maybe (fromJust)
import           Data.Text (Text)
import           Data.Text as T
import qualified GI.Gtk as Gtk
import           MusicSorter.TrackInfo (TrackInfo(..))

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
  do appCtxMVar <- atomically newEmptyTMVar
     withAsyncBound (uiThread appCtxMVar) $ \a1 ->
       A.withAsync (uiUpdateThread trackUpdates appCtxMVar) $ \a2 ->
         void $ A.wait a1 <* A.wait a2

uiThread :: TMVar AppContext -> IO ()
uiThread ctxMVar = do
  setCurrentThreadAsGUIThread
  _ <- Gtk.init Nothing
  appCtx@(AppContext {..}) <- getGtkScene
  atomically (putTMVar ctxMVar appCtx)
  Gtk.labelSetText titleLabel "Hola Mundo"
  Gtk.widgetShowAll mainWindow
  _ <- Gtk.onWidgetDestroy mainWindow (Gtk.mainQuit >> fail "no")
  Gtk.main

---
uiUpdateThread :: TBQueue (TrackInfo, [Text]) -> TMVar AppContext -> IO a
uiUpdateThread input ctxMVar = do
  AppContext {..} <- atomically (takeTMVar ctxMVar)
  forever $
      do (track, lyrics) <- atomically (readTBQueue input)
         let !singleLyrics  = T.unlines lyrics
             !bytesToUpdate = fromIntegral $ T.length singleLyrics
         postGUISync $ do
           Gtk.labelSetText titleLabel (tTitle track)
           Gtk.labelSetText artistLabel (tArtist track)
           lyricsBuffer <- Gtk.textViewGetBuffer lyricsTextView
           Gtk.textBufferSetText lyricsBuffer singleLyrics bytesToUpdate

