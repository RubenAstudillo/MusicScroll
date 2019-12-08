module MusicSorter.MRIS where

import DBus.Client

dbusThread :: IO ()
dbusThread = do
  client <- connectSession
  -- todo
