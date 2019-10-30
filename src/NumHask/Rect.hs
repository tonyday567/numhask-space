{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wall #-}

-- | a two-dimensional plane, implemented as a composite of a 'Point' of 'Range's.
module NumHask.Rect
  ( Rect(..)
  , pattern Rect
  , pattern Ranges
  , corners
  , projectRect
  , mult
  , unit
  ) where

import Data.Bool (bool)
import GHC.Exts
import GHC.Generics (Generic)
import Data.Distributive as D
import Data.Functor.Compose
import Data.Functor.Rep
import Prelude
import NumHask.Range
import NumHask.Space
import NumHask.Point
import Algebra.Lattice

-- $setup
-- >>> :set -XNoImplicitPrelude

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
-- >>> one :: Rect Double
-- Rect -0.5 0.5 -0.5 0.5
-- >>> zero :: Rect Double
-- Rect Infinity -Infinity Infinity -Infinity
--
-- as a Field instance
--
-- >>> Rect 0 1 2 3 + zero
-- Rect 0.0 1.0 2.0 3.0
-- >>> Rect 0 1 (-2) (-1) + Rect 2 3 (-5) 3
-- Rect 0.0 3.0 -5.0 3.0
-- >>> Rect 1 1 1 1 - one
-- Rect 0.5 1.0 0.5 1.0
-- >>> Rect 0 1 0 1 * one
-- Rect 0.0 1.0 0.0 1.0
-- >>> Rect 0 1 0 1 / one
-- Rect 0.0 1.0 0.0 1.0
-- >>> singleton (Point 1.0 2.0) :: Rect Double
-- Rect 1.0 1.0 2.0 2.0
-- >>> abs (Rect 1 0 1 0)
-- Rect 0.0 1.0 0.0 1.0
-- >>> sign (Rect 1 0 1 0) == negate one
-- True
--
-- as a Space instance
--
-- >>> project (Rect 0 1 (-1) 0) (Rect 1 4 10 0) (Point 0.5 1)
-- Point 2.5 -10.0
-- >>> gridSpace (Rect 0 10 0 1) (Point 2 2)
-- [Rect 0.0 5.0 0.0 0.5,Rect 0.0 5.0 0.5 1.0,Rect 5.0 10.0 0.0 0.5,Rect 5.0 10.0 0.5 1.0]
-- >>> grid MidPos (Rect 0 10 0 1) (Point 2 2)
-- [Point 2.5 0.25,Point 2.5 0.75,Point 7.5 0.25,Point 7.5 0.75]
newtype Rect a =
  Rect' (Compose Point Range a)
  deriving (Eq, Functor, Applicative, Foldable, Traversable,
            Generic)

-- | pattern of Rect lowerx upperx lowery uppery
pattern Rect :: a -> a -> a -> a -> Rect a
pattern Rect a b c d = Rect' (Compose (Point (Range a b) (Range c d)))
{-# COMPLETE Rect#-}

-- | pattern of Ranges xrange yrange
pattern Ranges :: Range a -> Range a -> Rect a
pattern Ranges a b = Rect' (Compose (Point a b))
{-# COMPLETE Ranges#-}

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

instance (Eq a, Lattice a) => Semigroup (Rect a) where
  (<>) (Ranges x y) (Ranges x' y') = Ranges (x `union` x') (y `union` y')

instance (Spaceable a) => Monoid (Rect a) where
  mempty = Ranges mempty mempty

instance (Spaceable a) => Space (Rect a) where
  type Element (Rect a) = Point a

  union (Ranges a b) (Ranges c d) = Ranges (a `union` c) (b `union` d)
  intersection (Ranges a b) (Ranges c d) = Ranges (a `intersection` c) (b `intersection` d)

  (>.<) (Point l0 l1) (Point u0 u1) = Rect l0 u0 l1 u1

  lower (Rect l0 _ l1 _) = Point l0 l1
  upper (Rect _ u0 _ u1) = Point u0 u1

  singleton (Point x y) = Rect x x y y

instance (Fractional a, Spaceable a) => FieldSpace (Rect a) where
    type Grid (Rect a) = Point Int

    grid o s n = (+ bool 0 (step/2) (o==MidPos)) <$> posns
      where
      posns =
        (lower s +) . (step *) . fmap fromIntegral <$>
        [Point x y | x <- [x0 .. x1], y <- [y0 .. y1]]
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
      | x <- grid LowerPos rX stepX
      , y <- grid LowerPos rY stepY
      ]
      where
        sx = width rX / fromIntegral stepX
        sy = width rY / fromIntegral stepY

-- | create a list of pairs representing the lower left and upper right cormners of a rectangle.
corners :: (Spaceable a) => Rect a -> [Point a]
corners r = [lower r, upper r]

-- | project a Rect from an old range to a new 1
projectRect ::
     (Spaceable a, Fractional a)
  => Rect a
  -> Rect a
  -> Rect a
  -> Rect a
projectRect r0 r1 (Rect a b c d) = Rect a' b' c' d'
  where
    (Point a' c') = project r0 r1 (Point a c)
    (Point b' d') = project r0 r1 (Point b d)


-- | Rect projection maths: some sort of affine projection lurking under the hood?
-- > width one = one
-- > mid zero = zero

mult :: (Eq a, Lattice a, Fractional a) => Rect a -> Rect a -> Rect a
mult (Ranges x0 y0) (Ranges x1 y1) =
  Ranges (x0 `rtimes` x1) (y0 `rtimes` y1)
  where
    rtimes a b = bool (Range (m - r/2) (m + r/2)) 0 (a == 0 || b == 0)
      where
        m = mid a + mid b
        r = width a * width b

unit :: Rect Double
unit = Ranges rone rone where
    rone = Range (-0.5) 0.5

