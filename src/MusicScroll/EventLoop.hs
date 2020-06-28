{-# language ExistentialQuantification #-}
module MusicScroll.EventLoop where

import Control.Concurrent.MVar
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Concurrent.STM.TBQueue
import Database.SQLite.Simple
import Pipes
import Pipes.Prelude (chain)
import Pipes.Concurrent
import Control.Exception (bracket)

import MusicScroll.LyricsPipeline (noRepeatedFilter, getLyricsP, saveOnDb)
import MusicScroll.DatabaseUtils (getDBPath)
import MusicScroll.UIEvent (UIEvent, AppContext(..), updateNewLyricsC)
import MusicScroll.TrackInfo (TrackIdentifier)
import MusicScroll.MPRIS (dbusThreadP)

data AppState = AppState
  { apUI :: AppContext
  , apDB :: MVar Connection -- ^Enforce mutual exclusion zone
  , apDBusTrack :: Input TrackIdentifier -- ^ Always has the last value emitted
  , apDBusErr :: Input UIEvent
  }

-- | This callbacks ought to update the UI themselves via postGUI
type UICallback = AppState -> IO ()

data EventLoopState = EventLoopState
  { evAppState :: AppState
  , evEphemeral :: Maybe (Async ())
  }

eventLoop :: AppContext -> TBQueue UICallback -> IO a
eventLoop ctx queue =
  do dbPath <- getDBPath
     bracket (open dbPath) close $ \conn -> do
       mconn               <- newMVar conn
       (outTrack, inTrack) <- spawn (newest 1)
       (outErr, inErr)     <- spawn (newest 1)
       let state = AppState ctx mconn inTrack inErr
       withAsync (dbusThreadP outTrack outErr) $ \dbusA ->
         withAsync (staticPipeline state) $ \staticA ->
           do let initEvState = EventLoopState state Nothing
              -- waitAnyCancel [ staticA ]
              loop initEvState
  where
    loop :: EventLoopState -> IO a
    loop st = do newCallback <- atomically (readTBQueue queue)
                 maybe (pure ()) cancel (evEphemeral st)
                 newAsync <- async (newCallback (evAppState st))
                 let st' = st { evEphemeral = Just newAsync }
                 loop st'

staticPipeline :: AppState -> IO ()
staticPipeline st =
  let songP = fromInput (apDBusTrack st)
  in runEffect $ songP >-> debugP "outputdbus " >-> noRepeatedFilter >->
                 debugP "filter " >-> getLyricsP (apDB st) >->
                 saveOnDb (apDB st) >-> updateNewLyricsC (apUI st)


debugP :: (Show a) => String -> Pipe a a IO b
debugP str = chain (\a -> putStr str >> print a)
