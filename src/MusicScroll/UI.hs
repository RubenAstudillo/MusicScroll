{-# language NamedFieldPuns, RecordWildCards, OverloadedStrings,
             BangPatterns #-}
module MusicScroll.UI (setupUIThread) where

import           Control.Concurrent.Async
                     (withAsyncBound, waitAnyCancel, withAsync)
import           Control.Concurrent.STM (atomically)
import           Control.Concurrent.STM.TBQueue (TBQueue, readTBQueue)
import           Control.Concurrent.STM.TMVar
                     (TMVar, newEmptyTMVar, takeTMVar, putTMVar)
import           Control.Exception (throwIO, AsyncException(UserInterrupt))
import           Control.Monad (forever)
import           Data.Functor (void)
import           Data.GI.Gtk.Threading
                     (setCurrentThreadAsGUIThread, postGUISync)
import           Data.Maybe (fromJust)
import           Data.Text as T
import qualified GI.Gtk as Gtk

import           MusicScroll.TrackInfo (TrackInfo(..))
import           MusicScroll.TagParsing (Lyrics(..))

import           Paths_musicScroll

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

setupUIThread :: TBQueue (TrackInfo, Lyrics) -> IO ()
setupUIThread trackUpdates =
  do appCtxMVar <- atomically newEmptyTMVar
     withAsyncBound (uiThread appCtxMVar) $ \a1 ->
       withAsync (uiUpdateThread trackUpdates appCtxMVar) $ \a2 ->
         void (waitAnyCancel [a1, a2]) >> throwIO UserInterrupt

uiThread :: TMVar AppContext -> IO ()
uiThread ctxMVar = do
  setCurrentThreadAsGUIThread
  _ <- Gtk.init Nothing
  appCtx@(AppContext {..}) <- getGtkScene
  atomically (putTMVar ctxMVar appCtx)
  Gtk.labelSetText titleLabel "MusicScroll"
  Gtk.widgetShowAll mainWindow
  _ <- Gtk.onWidgetDestroy mainWindow Gtk.mainQuit
  Gtk.main

---
uiUpdateThread :: TBQueue (TrackInfo, Lyrics) -> TMVar AppContext -> IO a
uiUpdateThread input ctxMVar = do
  AppContext {..} <- atomically (takeTMVar ctxMVar)
  forever $
      do (track, Lyrics singleLyrics) <- atomically (readTBQueue input)
         let !bytesToUpdate = fromIntegral $ T.length singleLyrics
         postGUISync $ do
           Gtk.labelSetText titleLabel (tTitle track)
           Gtk.labelSetText artistLabel (tArtist track)
           lyricsBuffer <- Gtk.textViewGetBuffer lyricsTextView
           Gtk.textBufferSetText lyricsBuffer singleLyrics bytesToUpdate

