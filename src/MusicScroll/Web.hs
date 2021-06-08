{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module MusicScroll.Web (getLyricsFromWeb) where

import Control.Applicative (Alternative (empty))
import Control.Exception (try)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Text.Encoding (decodeUtf8)
import MusicScroll.Providers.Utils
import MusicScroll.TrackInfo (TrackInfo (..))
import Network.HTTP.Req

getLyricsFromWeb ::
  (MonadIO m, Alternative m) =>
  Provider ->
  TrackInfo ->
  m Lyrics
getLyricsFromWeb (Provider {..}) track =
  do
    let songUrl = toUrl track
    resp <- liftIO $ try @HttpException (getPage songUrl)
    let notValid =
          either
            (const True)
            ((/= 200) . responseStatusCode)
            resp
    if notValid
      then empty
      else
        let Right realResp = resp
            body = decodeUtf8 (responseBody realResp)
            lyrics = extractLyricsFromPage body
         in pure lyrics

getPage :: Url 'Https -> IO BsResponse
getPage url =
  runReq defaultHttpConfig $
    req GET url NoReqBody bsResponse mempty
