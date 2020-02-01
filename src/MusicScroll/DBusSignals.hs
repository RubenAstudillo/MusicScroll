{-# language OverloadedStrings #-}
module MusicScroll.DBusSignals
  ( mediaPropChangeRule
  , waitForChange
  , changeClient
  ) where

import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TMVar (TMVar, takeTMVar, newEmptyTMVar, putTMVar)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.State (StateT, gets, modify)
import Data.Foldable (find)
import Data.Maybe (fromJust)

import DBus.Client
import DBus

import MusicScroll.DBusNames
import MusicScroll.ConnState

mediaPropChangeRule, busNameAddedRule :: MatchRule
mediaPropChangeRule = matchAny
  { matchPath      = pure mediaObject
  , matchInterface = pure "org.freedesktop.DBus.Properties"
  , matchMember    = pure "PropertiesChanged" }
busNameAddedRule = matchAny
  { matchSender    = pure "org.freedesktop.DBus" -- unique name
  , matchPath      = pure "/org/freedesktop/DBus"
  , matchInterface = pure "org.freedesktop.DBus"
  , matchMember    = pure "NameAcquired" }

waitForChange :: MatchRule -> StateT ConnState IO ()
waitForChange rule =
  do client <- gets cClient
     liftIO $ do
       trigger       <- atomically newEmptyTMVar
       disarmHandler <- gotSignalOfChange client trigger rule
       _ <- atomically $ takeTMVar trigger
       removeMatch client disarmHandler

gotSignalOfChange :: Client -> TMVar () -> MatchRule -> IO SignalHandler
gotSignalOfChange client trigger rule =
  addMatch client rule (\_ -> atomically ( putTMVar trigger () ))

changeClient :: StateT ConnState IO ()
changeClient =
  do client <- gets cClient
     availableStatus <- liftIO $ traverse (checkName client) allBuses
     let taggedBuses = zip allBuses availableStatus
     case fst <$> find snd taggedBuses of
       Just newBus -> modify (\s -> s { cBusActive = newBus })
       Nothing     -> do waitForChange busNameAddedRule
                         changeClient

checkName :: Client -> BusName -> IO Bool
checkName client name = do
  returnCall <- call_ client
      (methodCall "/org/freedesktop/DBus" "org.freedesktop.DBus" "NameHasOwner")
        { methodCallDestination = Just "org.freedesktop.DBus"
        , methodCallBody = [toVariant name] }
  return . fromJust . fromVariant . head . methodReturnBody $ returnCall
