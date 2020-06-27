{-# OPTIONS_GHC -Wall #-}

module Main where

import Protolude
import Test.DocTest

main :: IO ()
main =
  doctest
    [ "src/NumHask/Space/Histogram.hs",
      "src/NumHask/Space/Point.hs",
      "src/NumHask/Space/Range.hs",
      "src/NumHask/Space/Rect.hs",
      "src/NumHask/Space/Time.hs"
    ]
