module MusicScroll.Pipeline where

import Control.Concurrent.Async
import Control.Concurrent.MVar
import Database.SQLite.Simple
import Pipes.Concurrent
import Pipes
import qualified Pipes.Prelude as PP

import MusicScroll.LyricsPipeline 
import MusicScroll.DatabaseUtils (getDBPath)
import MusicScroll.UIEvent (SearchResult(ErrorOn2), ErrorCause, AppContext(..), dischargeOnUI, dischargeOnUISingle)
import MusicScroll.TrackInfo (TrackIdentifier)
import MusicScroll.TrackSuplement


data AppState = AppState
  { apUI :: AppContext
  , apDB :: MVar Connection -- ^Enforce mutual exclusion zone
  , apDBusTrack :: Input TrackIdentifier -- ^ Always has the last value emitted
  , apDBusErr :: Input ErrorCause }

staticPipeline :: AppState -> IO ()
staticPipeline (AppState ctx db dbusTrack dbusErr) =
  let songP = fromInput dbusTrack
      errP  = fromInput dbusErr
      songPipe = songP >-> noRepeatedFilter >-> cleanTrackP >->
        getLyricsP db >-> saveOnDb db >-> dischargeOnUI ctx
      errorPipe = errP >-> PP.map ErrorOn2 >-> dischargeOnUI ctx
  in withAsync (runEffect songPipe) $ \songA ->
       withAsync (runEffect errorPipe) $ \errorA ->
         void $ waitAnyCancel [ songA, errorA ]

suplementPipeline :: TrackSuplement -> AppState -> IO ()
suplementPipeline supl (AppState ctx db dbusTrack _) =
  -- I don't think this is firing.
  let songP = fromInput dbusTrack -- will fire once
      pipeline = songP >-> mergeTrackSupl supl >-> getLyricsFromWebP
          >-> saveOnDb db >-> dischargeOnUISingle ctx
  in runEffect pipeline

-- debugP :: (Show a) => String -> Pipe a a IO b
-- debugP str = chain (\a -> putStr str >> print a)

-- debugPS :: String -> Pipe a a IO b
-- debugPS str = chain (const (putStrLn str))
