{-# language RecordWildCards, OverloadedStrings, BangPatterns #-}
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
import           MusicScroll.UIEvent

import           Paths_musicScroll

data AppContext = AppContext
  { mainWindow     :: Gtk.Window
  , titleLabel     :: Gtk.Label
  , artistLabel    :: Gtk.Label
  , lyricsTextView :: Gtk.TextView
  , errorLabel     :: Gtk.Label
  , titleSuplementEntry   :: Gtk.Entry
  , artistSuplementEntry  :: Gtk.Entry
  , suplementAcceptButton :: Gtk.Button
  , suplementResetButton  :: Gtk.Button
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
             <*> getWidget Gtk.Label "errorLabel"
             <*> getWidget Gtk.Entry "titleSuplementEntry"
             <*> getWidget Gtk.Entry "artistSuplementEntry"
             <*> getWidget Gtk.Button "suplementAcceptButton"
             <*> getWidget Gtk.Button "suplementResetButton"

setupUIThread :: TBQueue UIEvent -> IO ()
setupUIThread events =
  do appCtxMVar <- atomically newEmptyTMVar
     withAsyncBound (uiThread appCtxMVar) $ \a1 ->
       withAsync (uiUpdateThread events appCtxMVar) $ \a2 ->
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
uiUpdateThread :: TBQueue UIEvent -> TMVar AppContext -> IO a
uiUpdateThread input ctxMVar = do
  appCtx <- atomically (takeTMVar ctxMVar)
  forever $ do
    event <- atomically (readTBQueue input)
    case event of
      GotLyric track lyrics -> updateNewLyrics appCtx (track, lyrics)
      ErrorOn cause -> updateErrorCause appCtx cause

updateNewLyrics :: AppContext -> (TrackInfo, Lyrics) -> IO ()
updateNewLyrics (AppContext {..}) (track, Lyrics singleLyrics) =
  let !bytesToUpdate = fromIntegral $ T.length singleLyrics
  in postGUISync $ do
    Gtk.labelSetText errorLabel mempty
    Gtk.labelSetText titleLabel (tTitle track)
    Gtk.labelSetText artistLabel (tArtist track)
    lyricsBuffer <- Gtk.textViewGetBuffer lyricsTextView
    Gtk.textBufferSetText lyricsBuffer singleLyrics bytesToUpdate

updateErrorCause :: AppContext -> ErrorCause -> IO ()
updateErrorCause (AppContext {..}) cause = postGUISync $
  do Gtk.labelSetText titleLabel "No Song available"
     Gtk.labelSetText artistLabel mempty
     lyricsBuffer <- Gtk.textViewGetBuffer lyricsTextView
     Gtk.textBufferSetText lyricsBuffer mempty 0
     Gtk.labelSetText errorLabel (errorMsg cause)
