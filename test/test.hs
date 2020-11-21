{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE NegativeLiterals #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Main where

import NumHask.Prelude
import Test.DocTest

main :: IO ()
main =
  doctest
    [ "src/NumHask/Space.hs",
      "src/NumHask/Space/Histogram.hs",
      "src/NumHask/Space/Point.hs",
      "src/NumHask/Space/Range.hs",
      "src/NumHask/Space/Rect.hs",
      "src/NumHask/Space/Time.hs",
      "src/NumHask/Space/XY.hs"
    ]
