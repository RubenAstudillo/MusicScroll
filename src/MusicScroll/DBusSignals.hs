{-# language OverloadedStrings, FlexibleContexts #-}
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
import Control.Monad.State.Class (MonadState(..))
import Control.Monad.Trans.State (StateT, gets, modify)
import Data.Foldable (find)
import Data.Maybe (fromJust)
import Pipes

import Debug.Trace

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

waitForChangeP :: (MonadState ConnStateP m, MonadIO m) => MatchRule -> m ()
waitForChangeP rule = do
  (ConnStateP client bus) <- get
  traceM $ "Recive2: " ++ show bus
  traceM $ "Recive2: antes wait"
  liftIO $ do
     trigger       <- atomically newEmptyTMVar
     disarmHandler <- armSignal client trigger rule
     traceIO "Recive2: arm"
     _ <- atomically $ takeTMVar trigger
     traceIO "Recive2: take"
     removeMatch client disarmHandler
     traceIO "Recive2: remove"
  traceM "Recive2: despues wait"

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

changeMusicClientP :: (MonadState ConnStateP m, MonadIO m) => m ()
changeMusicClientP =
  do state@(ConnStateP client bus) <- get
     availableStatus <- liftIO $ traverse (checkName client) allBuses
     let taggedBuses = zip allBuses availableStatus
     case fst <$> find snd taggedBuses of
       Just newBus -> put (ConnStateP client newBus)
       Nothing     -> do waitForChangeP busNameAddedRule
                         changeMusicClientP

checkName :: Client -> BusName -> IO Bool
checkName client name = do
  returnCall <- call_ client
      (methodCall "/org/freedesktop/DBus" "org.freedesktop.DBus" "NameHasOwner")
        { methodCallDestination = Just dbusBus
        , methodCallBody = [toVariant name] }
  -- We assume this call is correct, as it's done to the master dbus
  -- object. So fromJust/head are safe.
  return . fromJust . fromVariant . head . methodReturnBody $ returnCall
