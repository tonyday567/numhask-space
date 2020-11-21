{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE PatternSynonyms #-}

-- | Unification of 'Point' and 'Rect'.
module NumHask.Space.XY
  ( XY (..),
    pattern P,
    pattern R,
    toRect,
    toPoint,
    projectOn,
    projectTo,
  )
where

import GHC.Show (show)
import NumHask.Prelude hiding (show)
import NumHask.Space.Point
import NumHask.Space.Rect
import NumHask.Space.Types

-- | unification of a point and rect on the plane
data XY a
  = PointXY (Point a)
  | RectXY (Rect a)
  deriving (Eq, Functor)

instance (Show a) => Show (XY a) where
  show (PointXY (Point x y)) = "P " <> show x <> " " <> show y
  show (RectXY (Rect x z y w)) = "R " <> show x <> " " <> show z <> " " <> show y <> " " <> show w

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

-- | Convert an XY to a Rect
toRect :: XY a -> Rect a
toRect (PointXY (Point x y)) = Rect x x y y
toRect (RectXY a) = a

-- | Convert an XY to a Point
toPoint :: (Ord a, Field a) => XY a -> Point a
toPoint (PointXY (Point x y)) = Point x y
toPoint (RectXY (Ranges x y)) = Point (mid x) (mid y)

instance (Ord a) => Semigroup (XY a) where
  (<>) a b = RectXY (toRect a `union` toRect b)

-- | project an XY from one Rect to another, preserving relative position.
--
-- >>> projectOn one (Rect 0 1 0 1) zero
-- P -0.5 -0.5
projectOn :: Rect Double -> Rect Double -> XY Double -> XY Double
projectOn new old (PointXY p) = PointXY $ projectOnP new old p
projectOn new old (RectXY r) = RectXY $ projectOnR new old r

-- | project an [XY a] from it's enclosing space to the given space
--
-- >>> projectTo one (zipWith P [0..2] [0..2])
-- [P -0.5 -0.5,P 0.0 0.0,P 0.5 0.5]
projectTo :: Rect Double -> [XY Double] -> [XY Double]
projectTo _ [] = []
projectTo vb (x : xs) = projectOn vb (toRect $ sconcat (x :| xs)) <$> (x : xs)
