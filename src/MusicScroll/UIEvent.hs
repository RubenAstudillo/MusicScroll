{-# language OverloadedStrings, RecordWildCards, BangPatterns #-}
module MusicScroll.UIEvent where

import           Data.Maybe (isNothing)
import           Data.Text (Text)
import           Data.Text as T
import qualified GI.Gtk as Gtk
import           Data.GI.Gtk.Threading (postGUISync)

import           MusicScroll.TrackInfo (TrackInfo(..), TrackByPath(..))
import           MusicScroll.TagParsing (Lyrics(..))

data UIEvent = GotLyric TrackInfo Lyrics
             | ErrorOn ErrorCause

data ErrorCause = NotOnDB TrackByPath | NoLyricsOnWeb TrackInfo | ENoSong

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
extractGuess (NoLyricsOnWeb (TrackInfo {..})) =
  pure (tTitle, tArtist)
extractGuess (NotOnDB (TrackByPath {..})) =
  let def = maybe mempty id in pure (def tpTitle, def tpArtist)
extractGuess _ = Nothing

-- | Only usable inside a gtk context
updateNewLyrics :: AppContext -> (TrackInfo, Lyrics) -> IO ()
updateNewLyrics ctx@(AppContext {..}) (track, Lyrics singleLyrics) =
  let !bytesToUpdate = fromIntegral $ T.length singleLyrics
  in postGUISync $ do
    Gtk.labelSetText errorLabel mempty
    Gtk.labelSetText titleLabel (tTitle track)
    Gtk.labelSetText artistLabel (tArtist track)
    lyricsBuffer <- Gtk.textViewGetBuffer lyricsTextView
    Gtk.textBufferSetText lyricsBuffer singleLyrics bytesToUpdate
    updateSuplementalGuess ctx (mempty, mempty)

updateErrorCause :: AppContext -> ErrorCause -> IO ()
updateErrorCause ctx@(AppContext {..}) cause = postGUISync $
  do Gtk.labelSetText titleLabel "No Song available"
     Gtk.labelSetText artistLabel mempty
     lyricsBuffer <- Gtk.textViewGetBuffer lyricsTextView
     Gtk.textBufferSetText lyricsBuffer mempty 0
     Gtk.labelSetText errorLabel (errorMsg cause)
     maybe (return ()) (updateSuplementalGuess ctx) (extractGuess cause)

updateSuplementalGuess :: AppContext -> (Text, Text) -> IO ()
updateSuplementalGuess (AppContext {..}) (guessTitle, guessArtist) =
  do Gtk.entrySetText titleSuplementEntry guessTitle
     Gtk.entrySetText artistSuplementEntry guessArtist
