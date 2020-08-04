{-# OPTIONS_GHC -Wall #-}

-- | A continuous set of numbers.
--
-- Mathematics does not define a space, leaving library devs to experiment.
--
-- https://en.wikipedia.org/wiki/Space_(mathematics)
module NumHask.Space
  ( -- * Space
    -- $space
    module NumHask.Space.Types,

    -- * Instances
    -- $instances
    module NumHask.Space.Point,
    module NumHask.Space.Range,
    module NumHask.Space.Rect,
    module NumHask.Space.Time,
    module NumHask.Space.Histogram,
    module NumHask.Space.XY,
  )
where

import NumHask.Space.Histogram hiding ()
import NumHask.Space.Point hiding ()
import NumHask.Space.Range hiding ()
import NumHask.Space.Rect hiding ()
import NumHask.Space.Time hiding ()
import NumHask.Space.Types hiding ()
import NumHask.Space.XY hiding ()

-- $space
-- The final frontier.

{- $instances

 Space is an interesting cross-section of many programming domains.

 - A Range is a Space of numbers.

 - A Rect is a Space of Points.

 - A time span is a space containing moments of time.

 - A histogram is a divided Range with a count of elements within each division.

-}
