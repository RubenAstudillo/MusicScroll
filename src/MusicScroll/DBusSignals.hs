{-# language OverloadedStrings, ExplicitForAll #-}
module MusicScroll.DBusSignals
  ( mediaPropChangeRule
  , waitForChange
  , waitForChangeP
  , changeMusicClient
  , changeMusicClientP
  ) where

import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TMVar (TMVar, takeTMVar, newEmptyTMVar, putTMVar)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.State (StateT, gets, modify)
import Data.Foldable (find)
import Data.Maybe (fromJust)
import Pipes

import DBus.Client
import DBus

import MusicScroll.DBusNames
import MusicScroll.ConnState
import MusicScroll.ConnStateP

mediaPropChangeRule, busNameAddedRule :: MatchRule
mediaPropChangeRule = matchAny
  { matchPath      = pure mediaObject
  , matchInterface = pure "org.freedesktop.DBus.Properties"
  , matchMember    = pure "PropertiesChanged" }
busNameAddedRule = matchAny
  { matchSender    = pure dbusBus -- unique name
  , matchPath      = pure "/org/freedesktop/DBus"
  , matchInterface = pure "org.freedesktop.DBus"
  , matchMember    = pure "NameOwnerChanged" }

waitForChange :: MatchRule -> StateT ConnState IO ()
waitForChange rule =
  do client <- gets cClient
     liftIO $ do
       trigger       <- atomically newEmptyTMVar
       disarmHandler <- armSignal client trigger rule
       _ <- atomically $ takeTMVar trigger
       removeMatch client disarmHandler

waitForChangeP :: MonadIO m => ConnStateP -> MatchRule -> m ()
waitForChangeP (ConnStateP client _) rule = liftIO $
  do trigger       <- atomically newEmptyTMVar
     disarmHandler <- armSignal client trigger rule
     _ <- atomically $ takeTMVar trigger
     removeMatch client disarmHandler

armSignal :: Client -> TMVar () -> MatchRule -> IO SignalHandler
armSignal client trigger rule =
  addMatch client rule (\_ -> atomically ( putTMVar trigger () ))

changeMusicClient :: StateT ConnState IO ()
changeMusicClient =
  do client <- gets cClient
     availableStatus <- liftIO $ traverse (checkName client) allBuses
     let taggedBuses = zip allBuses availableStatus
     case fst <$> find snd taggedBuses of
       Just newBus -> modify (setBus newBus)
       Nothing     -> do waitForChange busNameAddedRule
                         changeMusicClient

changeMusicClientP :: MonadIO m => ConnStateP -> m ConnStateP
changeMusicClientP state@(ConnStateP client bus) =
  do availableStatus <- liftIO $ traverse (checkName client) allBuses
     let taggedBuses = zip allBuses availableStatus
     case fst <$> find snd taggedBuses of
       Just newBus -> pure (ConnStateP client newBus)
       Nothing     -> do waitForChangeP state busNameAddedRule
                         changeMusicClientP state

checkName :: Client -> BusName -> IO Bool
checkName client name = do
  returnCall <- call_ client
      (methodCall "/org/freedesktop/DBus" "org.freedesktop.DBus" "NameHasOwner")
        { methodCallDestination = Just dbusBus
        , methodCallBody = [toVariant name] }
  -- We assume this call is correct, as it's done to the master dbus
  -- object. So fromJust/head are safe.
  return . fromJust . fromVariant . head . methodReturnBody $ returnCall
