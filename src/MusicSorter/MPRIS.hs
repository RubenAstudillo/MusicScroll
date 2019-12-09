{-# language OverloadedStrings, ScopedTypeVariables #-}
module MusicSorter.MPRIS where

import Control.Monad (unless)
import DBus
import DBus.Client
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)

smplayerBus :: BusName
smplayerBus = "org.mpris.MediaPlayer2.smplayer"

mediaObject :: ObjectPath
mediaObject = "/org/mpris/MediaPlayer2"

mediaInterface :: InterfaceName
mediaInterface = "org.mpris.MediaPlayer2.Player"

dbusThread :: IO (Maybe Text)
dbusThread = do
  client <- connectSession
  -- resp <- call_ client (methodCall mediaObject mediaInterface "Metadata") {
  --   methodCallDestination = Just smplayerBus
  -- }
  -- let Just (dict :: Map Text Variant) = fromVariant (methodReturnBody resp !! 0)
  -- return (Map.lookup "xesam:artist" dict >>= fromVariant)
  resp <- getPropertyValue client (methodCall mediaObject mediaInterface "Metadata") {
    methodCallDestination = Just smplayerBus
  }
  case resp of
    Left err -> return Nothing
    Right (dict :: Map Text Variant) ->
      return (Map.lookup "xesam:artist" dict >>= fromVariant)
