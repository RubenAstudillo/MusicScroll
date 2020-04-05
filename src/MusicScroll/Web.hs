{-# language OverloadedStrings, NamedFieldPuns, RecordWildCards #-}
{-# language TypeApplications, DataKinds #-}
module MusicScroll.Web (getLyricsFromWeb) where

import Control.Exception (try, SomeException)
import Control.Applicative (Alternative(empty))
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Text.Encoding (decodeUtf8)
import Network.HTTP.Req
import Database.SQLite.Simple (Connection)

import MusicScroll.TrackInfo (TrackInfo(..))
import MusicScroll.DatabaseUtils (insertDBLyrics)
import MusicScroll.Providers.Utils

getLyricsFromWeb :: Provider -> TrackInfo -> ReaderT Connection IO Lyrics
getLyricsFromWeb (Provider {..}) track =
  do let songUrl = toUrl track
     resp <- liftIO $ try @SomeException (getPage songUrl)
     let notValid = either (const True)
                      ((/= 200) . responseStatusCode) resp
     if notValid then empty
       else let Right realResp = resp
                body   = decodeUtf8 (responseBody realResp)
                lyrics = extractLyricsFromPage body
            in insertDBLyrics track lyrics *> pure lyrics

getPage :: Url 'Https -> IO BsResponse
getPage url = runReq defaultHttpConfig $
  req GET url NoReqBody bsResponse mempty

