{-# OPTIONS_GHC -Wall #-}

-- | a continuous set of numbers
-- mathematics does not define a space, so library devs are free to experiment.
-- https://en.wikipedia.org/wiki/Interval_(mathematics)
module NumHask.Space
  ( -- * Space
    -- $space
    module NumHask.Space.Types,

    -- * Instances
    module NumHask.Point,
    module NumHask.Range,
    module NumHask.Rect,
  )
where

import NumHask.Point
import NumHask.Range
import NumHask.Rect
import NumHask.Space.Types

-- $space
-- The final frontier.

-- $instances
-- Some concrete data types that are usseful in charting.
