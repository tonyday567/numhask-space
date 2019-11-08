{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wincomplete-patterns #-}

-- | a two-dimensional plane, implemented as a composite of a 'Point' of 'Range's.
module NumHask.Rect
  ( Rect (..),
    pattern Rect,
    pattern Ranges,
    corners,
    corners4,
    projectRect,
    addRect,
    multRect,
    unitRect,
    foldRect,
    addPoint,
    rotateRect,
    gridR,
    gridF,
    aspect,
    ratio,
  )
where

import Algebra.Lattice
import Data.Bool (bool)
import Data.Distributive as D
import Data.Functor.Compose
import Data.Functor.Rep
import Data.List.NonEmpty
import Data.Semigroup
import GHC.Exts
import GHC.Generics (Generic)
import NumHask.Point
import NumHask.Range
import NumHask.Space.Types
import Prelude

-- $setup

-- | a 'Point' of 'Ranges' that form a rectangle in what is often thought of as the XY plane.
--
-- >>> let a = Rect (-1) 1 (-2) 4
-- >>> a
-- Rect -1 1 -2 4
-- >>> let (Ranges x y) = a
-- >>> x
-- Range -1 1
-- >>> y
-- Range -2 4
-- >>> fmap (+1) (Rect 1 2 3 4)
-- Rect 2 3 4 5
-- >>> unitRect :: Rect Double
-- Rect -0.5 0.5 -0.5 0.5
--
-- as a Space instance
--
-- >>> project (Rect 0 1 (-1) 0) (Rect 1 4 10 0) (Point 0.5 1)
-- Point 2.5 -10.0
-- >>> gridSpace (Rect 0 10 0 1) (Point 2 2)
-- [Rect 0.0 5.0 0.0 0.5,Rect 0.0 5.0 0.5 1.0,Rect 5.0 10.0 0.0 0.5,Rect 5.0 10.0 0.5 1.0]
-- >>> grid MidPos (Rect 0 10 0 1) (Point 2 2)
-- [Point 2.5 0.25,Point 2.5 0.75,Point 7.5 0.25,Point 7.5 0.75]
newtype Rect a
  = Rect' (Compose Point Range a)
  deriving
    ( Eq,
      Functor,
      Applicative,
      Foldable,
      Traversable,
      Generic
    )

-- | pattern of Rect lowerx upperx lowery uppery
pattern Rect :: a -> a -> a -> a -> Rect a
pattern Rect a b c d = Rect' (Compose (Point (Range a b) (Range c d)))

{-# COMPLETE Rect #-}

-- | pattern of Ranges xrange yrange
pattern Ranges :: Range a -> Range a -> Rect a
pattern Ranges a b = Rect' (Compose (Point a b))

{-# COMPLETE Ranges #-}

instance (Show a) => Show (Rect a) where
  show (Rect a b c d) =
    "Rect " <> show a <> " " <> show b <> " " <> show c <> " " <> show d

instance Distributive Rect where
  collect f x =
    Rect (getA . f <$> x) (getB . f <$> x) (getC . f <$> x) (getD . f <$> x)
    where
      getA (Rect a _ _ _) = a
      getB (Rect _ b _ _) = b
      getC (Rect _ _ c _) = c
      getD (Rect _ _ _ d) = d

instance Representable Rect where

  type Rep Rect = (Bool, Bool)

  tabulate f =
    Rect (f (False, False)) (f (False, True)) (f (True, False)) (f (True, True))

  index (Rect a _ _ _) (False, False) = a
  index (Rect _ b _ _) (False, True) = b
  index (Rect _ _ c _) (True, False) = c
  index (Rect _ _ _ d) (True, True) = d

instance (Ord a) => Semigroup (Rect a) where
  (<>) = union

instance (Ord a) => Space (Rect a) where

  type Element (Rect a) = Point a

  union (Ranges a b) (Ranges c d) = Ranges (a `union` c) (b `union` d)

  intersection (Ranges a b) (Ranges c d) =
    Ranges
      (a `intersection` c)
      (b `intersection` d)

  (>.<) (Point l0 l1) (Point u0 u1) = Rect l0 u0 l1 u1

  lower (Rect l0 _ l1 _) = Point l0 l1

  upper (Rect _ u0 _ u1) = Point u0 u1

  singleton (Point x y) = Rect x x y y

  (...) p p' = (p /\ p') >.< (p \/ p')

  (|.|) a s = (a `meetLeq` lower s) && (upper s `meetLeq` a)

  (|>|) s0 s1 = lower s0 `meetLeq` upper s1

  (|<|) s0 s1 = lower s1 `joinLeq` upper s0

instance (Ord a, Fractional a, Num a) => FieldSpace (Rect a) where

  type Grid (Rect a) = Point Int

  grid o s n = (+ bool 0 (step / 2) (o == MidPos)) <$> posns
    where
      posns =
        (lower s +) . (step *) . fmap fromIntegral
          <$> [Point x y | x <- [x0 .. x1], y <- [y0 .. y1]]
      step = (/) (width s) (fromIntegral <$> n)
      (Point x0 y0, Point x1 y1) =
        case o of
          OuterPos -> (0, n)
          InnerPos -> (1, n - 1)
          LowerPos -> (0, n - 1)
          UpperPos -> (1, n)
          MidPos -> (0, n - 1)

  gridSpace (Ranges rX rY) (Point stepX stepY) =
    [ Rect x (x + sx) y (y + sy)
      | x <- grid LowerPos rX stepX,
        y <- grid LowerPos rY stepY
    ]
    where
      sx = width rX / fromIntegral stepX
      sy = width rY / fromIntegral stepY

-- | create a list of points representing the lower left and upper right corners of a rectangle.
corners :: (Ord a) => Rect a -> [Point a]
corners r = [lower r, upper r]

-- | the 4 corners
corners4 :: Rect a -> NonEmpty (Point a)
corners4 (Rect x z y w) =
  Point x y
    :| [ Point x w,
         Point z y,
         Point z w
       ]

-- | project a Rect from an old range to a new 1
projectRect ::
  (Ord a, Fractional a) =>
  Rect a ->
  Rect a ->
  Rect a ->
  Rect a
projectRect r0 r1 (Rect a b c d) = Rect a' b' c' d'
  where
    (Point a' c') = project r0 r1 (Point a c)
    (Point b' d') = project r0 r1 (Point b d)

-- | Rect addition
addRect :: (Num a) => Rect a -> Rect a -> Rect a
addRect (Rect a b c d) (Rect a' b' c' d') =
  Rect (a + a') (b + b') (c + c') (d + d')

-- | Rect multiplication
multRect :: (Ord a, Fractional a) => Rect a -> Rect a -> Rect a
multRect (Ranges x0 y0) (Ranges x1 y1) =
  Ranges (x0 `rtimes` x1) (y0 `rtimes` y1)
  where
    rtimes a b = bool (Range (m - r / 2) (m + r / 2)) 0 (a == 0 || b == 0)
      where
        m = mid a + mid b
        r = width a * width b

-- | a unit Rectangle, with values chosen so that width and height are one and mid is zero
unitRect :: (Fractional a) => Rect a
unitRect = Ranges rone rone
  where
    rone = Range (-0.5) 0.5

-- | union of Rect's
foldRect :: (Ord a) => [Rect a] -> Maybe (Rect a)
foldRect [] = Nothing
foldRect (x : xs) = Just $ sconcat (x :| xs)

-- | add a Point to a Rect
addPoint :: (Num a) => Point a -> Rect a -> Rect a
addPoint (Point x' y') (Rect x z y w) = Rect (x + x') (z + x') (y + y') (w + y')

-- | rotate the corners of a Rect by x degrees relative to the origin, and fold to a new Rect
rotateRect :: (Floating a, Ord a) => a -> Rect a -> Rect a
rotateRect d r =
  space1 $ rotate d <$> corners r

-- | Create Rects for a formulae y = f(x) across an x range
gridR :: (Ord a, Fractional a) => (a -> a) -> Range a -> Int -> [Rect a]
gridR f r g = (\x -> Rect (x - tick / 2) (x + tick / 2) 0 (f x)) <$> grid MidPos r g
  where
    tick = width r / fromIntegral g

-- | Create values c for Rects data for a formulae c = f(x,y)
gridF :: (Ord a, Fractional a) => (Point a -> b) -> Rect a -> Grid (Rect a) -> [(Rect a, b)]
gridF f r g = (\x -> (x, f (mid x))) <$> gridSpace r g

-- | convert a ratio of x-plane : y-plane to a ViewBox with a height of one.
aspect :: (Fractional a) => a -> Rect a
aspect a = Rect (a * (-0.5)) (a * 0.5) (-0.5) 0.5

-- | convert a Rect to a ratio
ratio :: (Fractional a) => Rect a -> a
ratio (Rect x z y w) = (z - x) / (w - y)
