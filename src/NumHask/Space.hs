{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE RebindableSyntax #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

-- | A continuous set of numbers, with a lower and upper bound.
--
-- Mathematics does not rigorously define a space, leaving library devs free to push boundaries on what it all means.
--
-- https://en.wikipedia.org/wiki/Space_(mathematics)
module NumHask.Space
  ( -- * Usage
    -- $setup

    -- * Space
    -- $space
    module NumHask.Space.Types,

    -- * Instances
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

-- $setup
--
-- >>> :set -XRebindableSyntax
-- >>> :set -XNegativeLiterals
-- >>> import NumHask.Prelude
-- >>> import NumHask.Space
-- >>> Point 1 1
-- Point 1 1
--
-- >>> one :: Range Double
-- Range -0.5 0.5
--
-- >>> grid OuterPos (Range 0 50 :: Range Double) 5
-- [0.0,10.0,20.0,30.0,40.0,50.0]

-- $space
--
-- Space is an interesting cross-section of many programming domains.
--
-- - A 'Range' is a 'Space' of numbers.
--
-- - A 'Rect' is a 'Space' of 'Point's. It can also be a 'Space' of 'Rect's (but this is not yet coded up here).
--
-- - A time span is a 'Space' containing moments of time.
--
-- - A 'Histogram' is a divided 'Range' with a count of elements within each division.

-- $extensions
-- > :t Point 1.0 -1.0
-- Point 1.0 -1.0
--   :: (Subtractive a, FromRatio a Integer,
--       FromRatio (a -> Point a) Integer) =>
--      a -> Point a
--
-- > :set -XNegativeLiterals
-- > :t Point 1.0 -1.0
-- Point 1.0 -1.0 :: FromRatio a Integer => Point a
