{-# language OverloadedStrings #-}
module Main (main) where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text (readFile)
import Data.Coerce
import Test.Hspec

import MusicScroll.Providers.AZLyrics
import MusicScroll.Providers.Utils

import Paths_musicScroll

main :: IO ()
main = hspec $ do
  describe "parse multiple sources" $ do
    it "AZLyrics" $ do
      lyricsPath <- getDataFileName "azlyrics.html"
      lyrics <- Text.readFile lyricsPath
      let Provider _ extractor = azLyricsInstance
          lyrics2 = Text.lines . coerce . extractor $ lyrics
      lyrics2 `shouldBe` azTarget

azTarget :: [Text]
azTarget =
  [ "If you don't wanna see me"
  ,""
  , "Did a full one eighty, crazy"
  , "Thinking 'bout the way I was"
  , "Did the heartbreak change me? Maybe"
  , "But look at where I ended up"
  , ""
  , "I'm all good already"
  , "So moved on it's scary"
  , "I'm not where you left me at all"
  , ""
  , "So if you don't wanna see me dancing with somebody"
  , "If you wanna believe that anything could stop me"
  , ""
  , "Don't show up, don't come out"
  , "Don't start caring about me now"
  , "Walk away, you know how"
  , "Don't start caring about me now"
  , ""
  , "Aren't you the guy who tried to"
  , "Hurt me with the word \"goodbye\"?"
  , "Though it took some time to survive you"
  , "I'm better on the other side"
  , ""
  , "I'm all good already"
  , "So moved on it's scary"
  , "I'm not where you left me at all"
  , ""
  , "So if you don't wanna see me dancing with somebody"
  , "If you wanna believe that anything could stop me"
  , ""
  , "Don't show up, don't come out"
  , "Don't start caring about me now"
  , "Walk away, you know how"
  , "Don't start caring about me now"
  , ""
  , "Don't come out"
  , "Don't show up"
  , "Don't start now"
  , "Don't come out"
  , "I'm not where you left me at all"
  , ""
  , "So if you don't wanna see me dancing with somebody"
  , "If you wanna believe that anything could stop me"
  , ""
  , "Don't show up (Don't show up), don't come out (Don't come out)"
  , "Don't start caring about me now ('bout me now)"
  , "Walk away (Walk away), you know how (You know how)"
  , "Don't start caring about me now"
  , ""
  , "So (Don't come out, out, out)"
  , "(Don't show up, walk away, walk away)"
  , "So (Don't come out, out, out)"
  , "(Don't show up, walk away, walk away)"
  ]
