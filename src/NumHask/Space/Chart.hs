{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeFamilies #-}

-- | Coordinate charts and coordinate patches.
--
-- A 'Chart' is the differential-geometric notion of a homeomorphism from an
-- open subset of a manifold @m@ to an open subset of a model space of
-- coordinates @c@.  In this library the coordinate patch is represented by a
-- 'Space' (typically a 'Rect' for 2-D coordinates), so the existing
-- 'Space'/'Rect'/'Point' machinery becomes the chart infrastructure for free.
module NumHask.Space.Chart
  ( -- * Charts
    Chart (..),
    SomeSpace (..),
    chartMap,
    chartInverse,
    chartDomain,
    inverseChart,
    transition,

    -- * Common charts
    affineChart,
    polarChart,
  )
where

import NumHask.Prelude
import NumHask.Space.Point
import NumHask.Space.Rect
import NumHask.Space.Types

-- $setup
--
-- >>> :m -Prelude
-- >>> :set -XRebindableSyntax
-- >>> import NumHask.Prelude
-- >>> import NumHask.Space

-- | A coordinate chart on a manifold/model type @m@ with coordinates @c@.
--
-- The coordinate patch is any 'Space' whose elements have type @c@.  This is
-- usually a 'Rect' for 2-D coordinates @(Point a)@ or a 'Range' for 1-D
-- coordinates @a@.
data Chart m c where
  Chart ::
    (Space s, Element s ~ c) =>
    -- | coordinate-domain patch
    s ->
    -- | forward map: coordinates -> manifold point
    (c -> m) ->
    -- | inverse map: manifold point -> coordinates
    (m -> c) ->
    Chart m c

-- | An existential space whose elements have type @c@.
data SomeSpace c where
  SomeSpace :: (Space s, Element s ~ c) => s -> SomeSpace c

-- | Extract the forward map from a chart.
chartMap :: Chart m c -> c -> m
chartMap (Chart _ f _) = f

-- | Extract the inverse map from a chart.
chartInverse :: Chart m c -> m -> c
chartInverse (Chart _ _ g) = g

-- | Extract the coordinate-domain patch from a chart.
chartDomain :: Chart m c -> SomeSpace c
chartDomain (Chart d _ _) = SomeSpace d

-- | Invert a chart, supplying the codomain patch for the inverse.
inverseChart :: (Space s, Element s ~ m) => s -> Chart m c -> Chart c m
inverseChart s (Chart _ f g) = Chart s g f

-- | Transition map from the first chart to the second.
--
-- Both charts must cover (at least overlap on) the same manifold.  The result
-- is @φ₂ ∘ φ₁⁻¹@.
--
-- >>> let polar = polarChart (Rect 0.1 5 (-pi) pi)
-- >>> let cartesian = affineChart (Rect (-5) 5 (-5) 5) (Rect 0 1 0 1)
-- >>> transition cartesian polar (Point 1 0)
-- Point 0.6 0.5
transition :: Chart m c2 -> Chart m c1 -> c1 -> c2
transition c2 c1 = chartInverse c2 . chartMap c1

-- | An affine chart between two rectangular patches.
--
-- >>> let c = affineChart (Rect 0 1 0 1) (Rect 0 2 0 2)
-- >>> chartMap c (Point 0.5 0.5)
-- Point 1.0 1.0
affineChart :: (Field a, Ord a) => Rect a -> Rect a -> Chart (Point a) (Point a)
affineChart dom cod = Chart dom (project dom cod) (project cod dom)

-- | Polar coordinates @(r, θ)@ to cartesian points @(x, y)@.
--
-- >>> let c = polarChart (Rect 0.1 5 (-pi) pi)
-- >>> chartMap c (Point 1 0)
-- Point 1.0 0.0
polarChart :: (TrigField a, ExpField a, Ord a) => Rect a -> Chart (Point a) (Point a)
polarChart dom = Chart dom fwd back
  where
    fwd (Point r theta) = Point (r * cos theta) (r * sin theta)
    back (Point x y) = Point (sqrt (x * x + y * y)) (atan2 y x)
