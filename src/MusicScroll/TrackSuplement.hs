module MusicScroll.TrackSuplement where

import Data.Text

import MusicScroll.TrackInfo ( TrackInfo(..), TrackByPath(..)
                             , TrackIdentifier )

data TrackSuplement = TrackSuplement
  { tsTitle :: Text
  , tsArtist :: Text
  }

suplement :: TrackSuplement -> TrackIdentifier -> TrackInfo
suplement supl = either byPath byInfo
  where
    byPath :: TrackByPath -> TrackInfo
    byPath path = TrackInfo { tTitle  = tsTitle supl
                            , tArtist = tsArtist supl
                            , tUrl    = tpPath path }

    byInfo :: TrackInfo -> TrackInfo
    byInfo info = info { tTitle = tsTitle supl, tArtist = tsArtist supl}
