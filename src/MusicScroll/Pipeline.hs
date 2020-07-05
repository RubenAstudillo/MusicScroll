{-# language ScopedTypeVariables #-}
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
  , apDBusTrack1 :: Input TrackIdentifier -- ^ Always has the last value emitted
  , apDBusTrack2 :: Input TrackIdentifier -- ^ Always has the last value emitted
  , apDBusErr :: Input ErrorCause }

staticPipeline :: AppState -> IO ()
staticPipeline (AppState ctx db dbusTrack _ dbusErr) =
  let songP = fromInput dbusTrack
      errP  = fromInput dbusErr
      songPipe = songP >-> noRepeatedFilter >-> cleanTrackP >->
        getLyricsP db >-> saveOnDb db >-> dischargeOnUI ctx
      errorPipe = errP >-> PP.map ErrorOn2 >-> dischargeOnUI ctx
  in withAsync (runEffect songPipe) $ \songA ->
       withAsync (runEffect errorPipe) $ \errorA ->
         void $ waitAnyCancel [ songA, errorA ]

suplementPipeline :: TrackSuplement -> AppState -> IO ()
suplementPipeline supl (AppState ctx db _ dbusTrack2 _) =
  -- I don't think this is firing.
  let songP = fromInput dbusTrack2 -- will fire once
      pipeline = songP >-> mergeTrackSupl supl >-> getLyricsFromWebP
          >-> saveOnDb db >-> dischargeOnUISingle ctx
  in runEffect pipeline

debugPS :: String -> Pipe a a IO b
debugPS str = PP.chain (const (putStrLn str))

songSpawn :: IO (Output a, Input a, Input a)
songSpawn = do
  (out1, in1) <- spawn (newest 1)
  (out2, in2) <- spawn (newest 1)
  let joinOut = out1 <> out2
  pure $ (joinOut, in1, in2)
