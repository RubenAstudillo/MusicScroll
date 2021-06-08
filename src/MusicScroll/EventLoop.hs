module MusicScroll.EventLoop where

import Control.Concurrent.Async
import Control.Concurrent.STM
import Data.Foldable (traverse_)
import MusicScroll.Pipeline

-- | This callbacks ought to update the UI themselves via postGUI
type UICallback = AppState -> IO ()

data EventLoopState = EventLoopState
  { evAppState :: AppState,
    evUiCallbacks :: TBQueue UICallback,
    evEphemeral :: Maybe (Async ())
  }

eventLoop :: EventLoopState -> IO a
eventLoop st =
  do
    newCallback <- atomically . readTBQueue $ evUiCallbacks st
    traverse_ cancel (evEphemeral st)
    newAsync <- async (newCallback (evAppState st))
    let st' = st {evEphemeral = Just newAsync}
    eventLoop st'
