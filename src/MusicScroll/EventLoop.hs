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
import GHC.Conc (labelThread)
import Control.Monad (forever)

import MusicScroll.LyricsPipeline (noRepeatedFilter, getLyricsP, saveOnDb)
import MusicScroll.DatabaseUtils (getDBPath)
import MusicScroll.UIEvent (UIEvent, AppContext(..), updateNewLyricsC)
import MusicScroll.TrackInfo (TrackIdentifier)
import MusicScroll.MPRIS (dbusThreadP, dbusThreadP2)

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
       trackQ <- atomically $ newTBQueue 5
       errorQ <- atomically $ newTBQueue 5
       (outTrack, inTrack) <- spawn (newest 1)
       (outErr, inErr)     <- spawn (newest 1)
       let state = AppState ctx mconn inTrack inErr
       withAsync (dbusThreadP2 trackQ errorQ) $ \dbusA -> do
       -- withAsync (dbusThreadP outTrack outErr) $ \dbusA -> do
         labelThread (asyncThreadId dbusA) "dbusT"
         withAsync (staticPipeline2 trackQ state) $ \staticA -> do
         -- withAsync (staticPipeline state) $ \staticA -> do
           labelThread (asyncThreadId staticA) "staticA"
           let initEvState = EventLoopState state Nothing
              -- waitAnyCancel [ staticA ]
           loop initEvState
  where
    loop :: EventLoopState -> IO a
    loop st = do newCallback <- atomically (readTBQueue queue)
                 maybe (pure ()) cancel (evEphemeral st)
                 newAsync <- async (newCallback (evAppState st))
                 let st' = st { evEphemeral = Just newAsync }
                 loop st'

staticPipeline2 :: TBQueue TrackIdentifier -> AppState -> IO ()
staticPipeline2 trackQ st =
  let songP = forever $ liftIO (atomically (readTBQueue trackQ)) >>= yield
  in runEffect $ songP >-> debugP "outputdbus " >-> noRepeatedFilter >->
                 debugP "filter " >-> getLyricsP (apDB st) >->
                 debugPS "getLyrics" >->
                 saveOnDb (apDB st) >-> debugPS "saveOnDb " >-> updateNewLyricsC (apUI st)


staticPipeline :: AppState -> IO ()
staticPipeline st =
  let songP = fromInput (apDBusTrack st)
  in runEffect $ songP >-> debugP "outputdbus " >-> noRepeatedFilter >->
                 debugP "filter " >-> getLyricsP (apDB st) >->
                 debugPS "getLyrics" >->
                 saveOnDb (apDB st) >-> debugPS "saveOnDb " >-> updateNewLyricsC (apUI st)


debugP :: (Show a) => String -> Pipe a a IO b
debugP str = chain (\a -> putStr str >> print a)

debugPS :: String -> Pipe a a IO b
debugPS str = chain (const (putStrLn str))
