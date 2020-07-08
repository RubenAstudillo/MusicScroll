{-# language ScopedTypeVariables #-}
module MusicScroll.RealMain (realMain) where

import Control.Concurrent.Async (withAsync, withAsyncBound, waitAnyCancel)
import Control.Concurrent.STM.TBQueue (newTBQueue)
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.TMVar
import Control.Concurrent.MVar
import Data.Functor (void)
import Control.Exception (bracket)
import Database.SQLite.Simple
import Pipes.Concurrent

import MusicScroll.Pipeline
import MusicScroll.MPRIS
import MusicScroll.UI
import MusicScroll.EventLoop
import MusicScroll.DatabaseUtils (getDBPath, sqlDBCreate)

realMain :: IO ()
realMain = do
  appCtxTMvar  <- atomically newEmptyTMVar
  suplTVar <- atomically (newTVar Nothing)
  uiCallbackTB <- atomically (newTBQueue 5)
  withAsyncBound (uiThread appCtxTMvar uiCallbackTB suplTVar) $ \uiA -> do
    (trackin, errorin, singleProd, trackout, errorout) <- musicSpawn
    withAsync (dbusThread trackout errorout) $ \dbusA -> do
      dbPath <- getDBPath
      bracket (open dbPath) close $ \conn -> do
        execute_ conn sqlDBCreate
        mconn <- newMVar conn
        ctx   <- atomically (takeTMVar appCtxTMvar)
        let state = AppState ctx mconn suplTVar (trackin, errorin) singleProd
        let evState = EventLoopState state uiCallbackTB Nothing
        withAsync (staticPipeline state) $ \staticA ->
          withAsync (eventLoop evState) $ \evLoopA ->
            void $ waitAnyCancel [ staticA, evLoopA, uiA, dbusA ]
