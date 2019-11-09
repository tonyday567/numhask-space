{-# OPTIONS_GHC -Wall #-}

module Main where

import Test.DocTest
import Prelude

main :: IO ()
main =
  doctest
    [ "src/NumHask/Histogram.hs",
      "src/NumHask/Point.hs",
      "src/NumHask/Range.hs",
      "src/NumHask/Rect.hs",
      "src/NumHask/Space/Time.hs"
    ]
