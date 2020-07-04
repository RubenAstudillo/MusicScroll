{-# language ExistentialQuantification, ScopedTypeVariables #-}
module MusicScroll.EventLoop where

import Control.Concurrent.MVar
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Concurrent.STM.TBQueue
import Database.SQLite.Simple
import Pipes
import qualified Pipes.Prelude as PP (chain, map)
import Pipes.Concurrent
import Control.Exception (bracket)
-- import GHC.Conc (labelThread)
import Control.Monad (forever)
import Data.Functor (void)

import MusicScroll.LyricsPipeline (noRepeatedFilter, getLyricsP, saveOnDb)
import MusicScroll.DatabaseUtils (getDBPath)
import MusicScroll.UIEvent (SearchResult(ErrorOn2), ErrorCause, AppContext(..), dischargeOnUI)
import MusicScroll.TrackInfo (TrackIdentifier)
import MusicScroll.MPRIS (dbusThreadP)
import MusicScroll.Pipeline

-- | This callbacks ought to update the UI themselves via postGUI
type UICallback = AppState -> IO ()

data EventLoopState = EventLoopState
  { evAppState    :: AppState
  , evUiCallbacks :: TBQueue UICallback
  , evEphemeral   :: Maybe (Async ()) }

eventLoop :: EventLoopState -> IO a
eventLoop st =
  do newCallback <- atomically . readTBQueue $ evUiCallbacks st
     maybe (pure ()) cancel (evEphemeral st)
     newAsync <- async (newCallback (evAppState st))
     let st' = st { evEphemeral = Just newAsync }
     eventLoop st'
