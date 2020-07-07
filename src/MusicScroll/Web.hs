{-# language OverloadedStrings, NamedFieldPuns, RecordWildCards #-}
{-# language TypeApplications, DataKinds #-}
module MusicScroll.Web (getLyricsFromWeb) where

import Control.Exception (try)
import Control.Applicative (Alternative(empty))
import Control.Monad.IO.Class (MonadIO(..))
import Data.Text.Encoding (decodeUtf8)
import Network.HTTP.Req

import MusicScroll.TrackInfo (TrackInfo(..))
import MusicScroll.Providers.Utils

getLyricsFromWeb :: (MonadIO m, Alternative m) => Provider -> TrackInfo
                  -> m Lyrics
getLyricsFromWeb (Provider {..}) track =
  do let songUrl = toUrl track
     resp <- liftIO $ try @HttpException (getPage songUrl)
     let notValid = either (const True)
                      ((/= 200) . responseStatusCode) resp
     if notValid then empty
       else let Right realResp = resp
                body   = decodeUtf8 (responseBody realResp)
                lyrics = extractLyricsFromPage body
            in pure lyrics

getPage :: Url 'Https -> IO BsResponse
getPage url = runReq defaultHttpConfig $
  req GET url NoReqBody bsResponse mempty
