{-# OPTIONS_GHC -Wall #-}

-- | A continuous set of numbers.
--
-- Mathematics does not define a space, leaving library devs to experiment.
--
-- https://en.wikipedia.org/wiki/Space_(mathematics)
--
module NumHask.Space
  ( -- * Space
    -- $space
    module NumHask.Space.Types,

    -- * Instances
    -- $instances
    module NumHask.Point,
    module NumHask.Range,
    module NumHask.Rect,
  )
where

import NumHask.Point hiding ()
import NumHask.Range hiding ()
import NumHask.Rect hiding ()
import NumHask.Space.Types hiding ()

-- $space
-- The final frontier.

-- $instances
-- A Range is a Space of numbers.
--
-- A Rect is a Space of Points.
