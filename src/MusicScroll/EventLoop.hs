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

data AppState = AppState
  { apUI :: AppContext
  , apDB :: MVar Connection -- ^Enforce mutual exclusion zone
  , apDBusTrack :: Input TrackIdentifier -- ^ Always has the last value emitted
  , apDBusErr :: Input ErrorCause }

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

staticPipeline :: AppState -> IO ()
staticPipeline (AppState ctx db dbusTrack dbusErr) =
  let songP = fromInput dbusTrack
      errP  = fromInput dbusErr
      songPipe = songP >-> noRepeatedFilter >-> getLyricsP db
                 >-> saveOnDb db >-> dischargeOnUI ctx
      errorPipe = errP >-> PP.map ErrorOn2 >-> dischargeOnUI ctx
  in withAsync (runEffect songPipe) $ \songA ->
       withAsync (runEffect errorPipe) $ \errorA ->
         void $ waitAnyCancel [ songA, errorA ]


-- debugP :: (Show a) => String -> Pipe a a IO b
-- debugP str = chain (\a -> putStr str >> print a)

-- debugPS :: String -> Pipe a a IO b
-- debugPS str = chain (const (putStrLn str))
