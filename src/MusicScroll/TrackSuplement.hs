module MusicScroll.TrackSuplement (TrackSuplement(..), mergeSuplement) where

import Data.Text
import Pipes (Pipe)
import qualified Pipes.Prelude as PP (map)

import MusicScroll.TrackInfo ( TrackInfo(..), TrackByPath(..)
                             , TrackIdentifier )

data TrackSuplement = TrackSuplement
  { tsTitle :: Text
  , tsArtist :: Text }

suplement :: TrackSuplement -> TrackIdentifier -> TrackInfo
suplement supl = either byPath byInfo
  where
    byPath :: TrackByPath -> TrackInfo
    byPath path = TrackInfo { tTitle  = tsTitle supl
                            , tArtist = tsArtist supl
                            , tUrl    = tpPath path }

    byInfo :: TrackInfo -> TrackInfo
    byInfo info = info { tTitle = tsTitle supl, tArtist = tsArtist supl}

mergeSuplement :: Functor m => TrackSuplement -> Pipe TrackIdentifier TrackInfo m a
mergeSuplement = PP.map . suplement
