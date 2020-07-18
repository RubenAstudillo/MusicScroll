{-# language OverloadedStrings, RecordWildCards, BangPatterns #-}
module MusicScroll.UIContext where

import           Control.Monad (unless, forever)
import           Data.GI.Gtk.Threading (postGUISync)
import           Data.Maybe (isNothing)
import           Data.Text as T
import qualified GI.Gtk as Gtk
import Pipes

import MusicScroll.TrackInfo (TrackInfo(..), TrackByPath(..))
import MusicScroll.Providers.Utils (Lyrics(..))
import MusicScroll.LyricsPipeline

data UIContext = UIContext
  { mainWindow     :: Gtk.Window
  , titleLabel     :: Gtk.Label
  , artistLabel    :: Gtk.Label
  , lyricsTextView :: Gtk.TextView
  , errorLabel     :: Gtk.Label
  , titleSuplementEntry   :: Gtk.Entry
  , artistSuplementEntry  :: Gtk.Entry
  , suplementAcceptButton :: Gtk.Button
  , suplementUpdateButton :: Gtk.Button
  , keepArtistNameCheck   :: Gtk.CheckButton
  }

errorMsg :: ErrorCause -> Text
errorMsg (NotOnDB trackPath)
  | isNothing (tpArtist trackPath) =
        "No lyrics found by hash on the song file, try to suplement the song's\
        \ artist metadata to try to get it from the web."
  | isNothing (tpTitle trackPath) =
        "No lyrics found by hash on the song file, try to suplement the song's\
        \ title metadata to try to get it from the web."
  | otherwise = "This case should not happen"
errorMsg ENoSong = "No song found, this is usually an intermediary state."
errorMsg (NoLyricsOnWeb _) = "Lyrics provider didn't have that song."

extractGuess :: ErrorCause -> Maybe (Text, Text)
extractGuess (NoLyricsOnWeb (TrackInfo {..})) = pure (tTitle, tArtist)
extractGuess (NotOnDB (TrackByPath {..})) =
  let def = maybe mempty id in pure (def tpTitle, def tpArtist)
extractGuess _ = Nothing

-- | Only usable inside a gtk context
updateNewLyrics :: UIContext -> (TrackInfo, Lyrics) -> IO ()
updateNewLyrics ctx@(UIContext {..}) (track, Lyrics singleLyrics) =
  let !bytesToUpdate = fromIntegral $ T.length singleLyrics
  in postGUISync $ do
    Gtk.labelSetText errorLabel mempty
    Gtk.labelSetText titleLabel (tTitle track)
    Gtk.labelSetText artistLabel (tArtist track)
    lyricsBuffer <- Gtk.textViewGetBuffer lyricsTextView
    Gtk.textBufferSetText lyricsBuffer singleLyrics bytesToUpdate
    updateSuplementalGuess ctx (mempty, mempty)

dischargeOnUI :: UIContext -> Consumer SearchResult IO a
dischargeOnUI ctx = forever (dischargeOnUISingle ctx)

dischargeOnUISingle :: UIContext -> Consumer SearchResult IO ()
dischargeOnUISingle ctx = do
  res <- await
  liftIO $ case res of
    GotLyric _ info lyr -> updateNewLyrics ctx (info, lyr)
    ErrorOn cause -> updateErrorCause ctx cause

updateErrorCause :: UIContext -> ErrorCause -> IO ()
updateErrorCause ctx@(UIContext {..}) cause = postGUISync $
  do Gtk.labelSetText titleLabel "No Song available"
     Gtk.labelSetText artistLabel mempty
     lyricsBuffer <- Gtk.textViewGetBuffer lyricsTextView
     Gtk.textBufferSetText lyricsBuffer mempty 0
     Gtk.labelSetText errorLabel (errorMsg cause)
     maybe (return ()) (updateSuplementalGuess ctx) (extractGuess cause)

updateSuplementalGuess :: UIContext -> (Text, Text) -> IO ()
updateSuplementalGuess (UIContext {..}) (guessTitle, guessArtist) =
  do Gtk.entrySetText titleSuplementEntry guessTitle
     shouldMaintainArtistSupl <- Gtk.getToggleButtonActive keepArtistNameCheck
     unless shouldMaintainArtistSupl $
       Gtk.entrySetText artistSuplementEntry guessArtist
