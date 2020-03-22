{-# language OverloadedStrings, DataKinds, NamedFieldPuns, TypeApplications #-}
module MusicScroll.AZLyrics (getLyricsFromWeb) where

import Control.Exception (try, SomeException)
import Control.Applicative (Alternative(empty))
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Text (Text)
import Data.Text as T hiding (filter, tail, map, empty)
import Data.Text.Encoding (decodeUtf8)
import Network.HTTP.Req
import Database.SQLite.Simple (Connection)

import MusicScroll.TrackInfo (TrackInfo(..))
import MusicScroll.TagParsing
import MusicScroll.DatabaseUtils (insertDBLyrics)

getLyricsFromWeb :: TrackInfo -> ReaderT Connection IO Lyrics
getLyricsFromWeb track@(TrackInfo {tArtist, tTitle}) =
  do let songUrl = toUrl tArtist tTitle
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

toUrl :: Text -> Text -> Url 'Https
toUrl artist song =
  let base :: Url 'Https
      base = https "www.azlyrics.com"

      quotedArtist = normalize artist
      quotedSong = normalize song <> ".html"
  in base /: "lyrics" /: quotedArtist /: quotedSong

normalize :: Text -> Text
normalize = let noSpaces = replace " " "" in noSpaces . toLower
