{-# language PatternSynonyms #-}
module MusicScroll.Pipeline where

import Control.Concurrent.Async
import Control.Concurrent.MVar
import Control.Concurrent.STM.TVar (TVar)
import Database.SQLite.Simple
import Pipes.Concurrent
import Pipes
import qualified Pipes.Prelude as PP
import Data.Functor.Contravariant.Divisible

import MusicScroll.LyricsPipeline
import MusicScroll.UIContext (UIContext(..), dischargeOnUI, dischargeOnUISingle)
import MusicScroll.TrackInfo (TrackIdentifier, cleanTrack,
                              pattern OnlyMissingArtist)
import MusicScroll.TrackSuplement

data DBusSignal = Song TrackIdentifier | Error ErrorCause | NoInfo

data AppState = AppState
  { apUI :: UIContext
  , apDB :: MVar Connection -- ^ Enforce mutual exclusion zone
  , apSupl :: TVar (Maybe TrackSuplement)
  , apStaticinput :: (Input TrackIdentifier, Input ErrorCause)
  , apEphemeralInput :: Producer DBusSignal IO () -- ^ Emits only once.
  }

staticPipeline :: AppState -> IO ()
staticPipeline (AppState ctx db svar (dbusTrack, dbusErr) _) =
  let songP = fromInput dbusTrack >-> addSuplArtist svar
      errP  = fromInput dbusErr
      songPipe = songP >-> noRepeatedSongs >-> cleanTrack >->
        getLyricsFromAnywhere db >-> saveOnDb db >-> dischargeOnUI ctx
      errorPipe = errP >-> PP.map ErrorOn >-> dischargeOnUI ctx
  in withAsync (runEffect songPipe) $ \songA ->
       withAsync (runEffect errorPipe) $ \errorA ->
         void $ waitAnyCancel [ songA, errorA ]

suplementPipeline :: TrackSuplement -> AppState -> IO ()
suplementPipeline supl (AppState ctx db _ _ signal) =
  let justTracks a = case a of { Song track -> Just track ; _ -> Nothing }
      songP = signal >-> PP.mapFoldable justTracks
      pipeline = songP >-> mergeSuplement supl >-> getLyricsOnlyFromWeb
          >-> saveOnDb db >-> dischargeOnUISingle ctx
  in runEffect pipeline

-- | Use the `Output` Divisible instance to create a network. These are
--   1) An output for songs.
--   2) One for errors
--   3) A merge from the previous two.
-- The last one is special as it's non-work-stealing, so we can pass it to
-- multiple listeners and all will receive a signal. But we have to be
-- careful of only taking a single value of it, as it basically a `TVar a`.
musicSpawn :: IO ( Input TrackIdentifier, Input ErrorCause
                 , Producer DBusSignal IO ()
                 , Output TrackIdentifier, Output ErrorCause)
musicSpawn = do
  (protoTrackout, trackin) <- spawn (newest 1)
  (protoErrorout, errorin) <- spawn (newest 1)
  (allout, allin) <- spawn (latest NoInfo)

  let realTrackout = divide (\a -> (a, Song a)) protoTrackout allout
      realErrorout = divide (\a -> (a, Error a)) protoErrorout allout
      singleProd = fromInput allin >-> PP.take 1

  pure $ (trackin, errorin, singleProd, realTrackout, realErrorout)

addSuplArtist :: TVar (Maybe TrackSuplement) -> Pipe TrackIdentifier TrackIdentifier IO a
addSuplArtist svar = PP.mapM go
  where go :: TrackIdentifier -> IO TrackIdentifier
        go signal@(Left OnlyMissingArtist) = atomically (readTVar svar) >>=
            pure . maybe signal (Right . flip suplement signal)
        go other = pure other
