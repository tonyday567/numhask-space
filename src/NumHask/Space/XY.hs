{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}

{- | Unification of a point on the XY-plane and a rectangle on the XY-plane.


-}

module NumHask.Space.XY
  ( XY(..),
    pattern P,
    pattern R,
    toRect,
    toPoint,
    projectOn,
    projectTo,
    fixRect,
  ) where

import NumHask.Prelude
import NumHask.Space.Point
import NumHask.Space.Rect
import NumHask.Space.Types

-- | unification of a point and rect on the plane
data XY a
  = PointXY (Point a)
  | RectXY (Rect a)
  deriving (Eq, Show, Functor)

-- | make an XY from a point
pattern P :: a -> a -> XY a
pattern P x y = PointXY (Point x y)

{-# COMPLETE P #-}

-- | make an XY from a rectangle
pattern R :: a -> a -> a -> a -> XY a
pattern R x z y w = RectXY (Rect x z y w)

{-# COMPLETE R #-}

instance (Additive a) => Additive (XY a) where
  PointXY (Point x y) + PointXY (Point x' y') = PointXY (Point (x + x') (y + y'))
  PointXY (Point x' y') + RectXY (Rect x z y w) = RectXY $ Rect (x + x') (z + x') (y + y') (w + y')
  RectXY (Rect x z y w) + PointXY (Point x' y') = RectXY $ Rect (x + x') (z + x') (y + y') (w + y')
  RectXY (Rect x z y w) + RectXY (Rect x' z' y' w') =
    RectXY $ Rect (x + x') (z + z') (y + y') (w + w')
  zero = PointXY (Point zero zero)

instance (Ord a, Field a) => Multiplicative (XY a) where
  x * y = RectXY $ toRect x * toRect y
  one = RectXY one

instance (Ord a, Subtractive a) => Subtractive (XY a) where
  negate (PointXY (Point x y)) = PointXY (Point (negate x) (negate y))
  negate (RectXY (Rect x z y w)) = RectXY (Rect (negate x) (negate z) (negate y) (negate w))

instance (Ord a, Field a, Signed a) => Signed (XY a) where
  abs x = PointXY $ abs <$> toPoint x
  sign x = PointXY $ sign <$> toPoint x

-- * Natural transformations

-- | Convert a spot to a Rect
toRect :: XY a -> Rect a
toRect (PointXY (Point x y)) = Rect x x y y
toRect (RectXY a) = a

-- | Convert a spot to a Point
toPoint :: (Ord a, Field a) => XY a -> Point a
toPoint (PointXY (Point x y)) = Point x y
toPoint (RectXY (Ranges x y)) = Point (mid x) (mid y)

instance (Ord a) => Semigroup (XY a) where
  (<>) a b = RectXY (toRect a `union` toRect b)

-- | project an XY from one Rect to another, preserving relative position.
--
-- >>> projectOn one (Rect 0 1 0 1) zero
-- PointXY Point -0.5 -0.5
projectOn :: Rect Double -> Rect Double -> XY Double -> XY Double
projectOn new old@(Rect x z y w) po@(PointXY (Point px py))
  | x == z && y == w = po
  | x == z = (P px py')
  | y == w = (P px' py)
  | otherwise = (P px' py')
  where
    (Point px' py') = project old new (toPoint po)
projectOn new old@(Rect x z y w) ao@(RectXY (Rect ox oz oy ow))
  | x == z && y == w = ao
  | x == z = (R ox oz ny nw)
  | y == w = (R nx nz oy ow)
  | otherwise = RectXY a
  where
    a@(Rect nx nz ny nw) = projectRect old new (toRect ao)

-- | project a [Spot a] from it's folded space to the given area
--
-- >>> projectTo one (SpotPoint <$> zipWith Point [0..2] [0..2])
-- [SpotPoint Point -0.5 -0.5,SpotPoint Point 0.0 0.0,SpotPoint Point 0.5 0.5]
projectTo :: Rect Double -> [XY Double] -> [XY Double]
projectTo _ [] = []
projectTo vb (x : xs) = projectOn vb (toRect $ sconcat (x :| xs)) <$> (x : xs)

-- | guard substituting singleton dimensions
fixRect :: Maybe (Rect Double) -> Rect Double
fixRect r = maybe one singletonUnit r
  where
    singletonUnit (Rect x z y w)
      | x == z && y == w = Rect (x - 0.5) (x + 0.5) (y - 0.5) (y + 0.5)
      | x == z = Rect (x - 0.5) (x + 0.5) y w
      | y == w = Rect x z (y - 0.5) (y + 0.5)
      | otherwise = Rect x z y w
