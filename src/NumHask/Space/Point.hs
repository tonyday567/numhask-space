{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall #-}

-- | A 2-dimensional point.
module NumHask.Space.Point
  ( Point (..),
    rotate,
    gridP,
  )
where

import Data.Distributive
import Data.Functor.Classes
import Data.Functor.Rep
import GHC.Show (show)
import NumHask.Space.Range
import NumHask.Space.Types
import NumHask.Prelude hiding (show, Distributive, rotate)
import qualified NumHask.Prelude as P

-- $setup
-- >>> :set -XNoImplicitPrelude

-- | A 2-dim point of a's
--
-- A Point is functorial over both arguments, and is a Num instance.
--
-- >>> let p = Point 1 1
-- >>> p + p
-- Point 2 2
-- >>> (2*) <$> p
-- Point 2 2
--
-- A major reason for this bespoke treatment of a point is that Points do not have maximums and minimums but they form a lattice, and this is useful for folding points to find out the (rectangular) Space they occupy.
--
-- >>> Point 0 1 /\ Point 1 0
-- Point 0 0
-- >>> Point 0 1 \/ Point 1 0
-- Point 1 1
data Point a
  = Point a a
  deriving (Eq, Generic)

instance (Show a) => Show (Point a) where
  show (Point a b) = "Point " <> show a <> " " <> show b

instance Functor Point where
  fmap f (Point a b) = Point (f a) (f b)

instance Eq1 Point where
  liftEq f (Point a b) (Point c d) = f a c && f b d

instance Show1 Point where
  liftShowsPrec sp _ d (Point a b) = showsBinaryWith sp sp "Point" d a b

instance Applicative Point where
  pure a = Point a a

  (Point fa fb) <*> Point a b = Point (fa a) (fb b)

instance Monad Point where
  Point a b >>= f = Point a' b'
    where
      Point a' _ = f a
      Point _ b' = f b

instance Foldable Point where
  foldMap f (Point a b) = f a `mappend` f b

instance Traversable Point where
  traverse f (Point a b) = Point <$> f a <*> f b

instance (Semigroup a) => Semigroup (Point a) where
  (Point a0 b0) <> (Point a1 b1) = Point (a0 <> a1) (b0 <> b1)

instance (Semigroup a, Monoid a) => Monoid (Point a) where
  mempty = Point mempty mempty

  mappend = (<>)

instance (Bounded a) => Bounded (Point a) where
  minBound = Point minBound minBound

  maxBound = Point maxBound maxBound

instance (Additive a) => Additive (Point a) where
  (Point a0 b0) + (Point a1 b1) = Point (a0 + a1) (b0 + b1)
  zero = Point zero zero

instance (Subtractive a) => Subtractive (Point a) where
  negate = fmap negate

instance (Multiplicative a) => Multiplicative (Point a) where
  (Point a0 b0) * (Point a1 b1) = Point (a0 * a1) (b0 * b1)
  one = Point one one

instance (P.Distributive a) => P.Distributive (Point a)

instance (Field a) => Field (Point a)

instance (Signed a) => Signed (Point a) where
  sign = fmap sign
  abs = fmap abs

instance (Divisive a) => Divisive (Point a) where
  recip = fmap recip

instance Distributive Point where
  collect f x = Point (getL . f <$> x) (getR . f <$> x)
    where
      getL (Point l _) = l
      getR (Point _ r) = r

instance Representable Point where
  type Rep Point = Bool

  tabulate f = Point (f False) (f True)

  index (Point l _) False = l
  index (Point _ r) True = r

instance (Ord a) => JoinSemiLattice (Point a) where
  (\/) (Point x y) (Point x' y') = Point (max x x') (max y y')

instance (Ord a) => MeetSemiLattice (Point a) where
  (/\) (Point x y) (Point x' y') = Point (min x x') (min y y')

-- | rotate a point by x degrees relative to the origin
--
-- >>> rotate 90 (Point 0 1)
-- Point 1.0 6.123233995736766e-17
rotate :: (FromInteger a, TrigField a) => a -> Point a -> Point a
rotate d (Point x y) = Point (x * cos d' + y * sin d') (y * cos d' - x * sin d')
  where
    d' = d * pi / 180

-- | Create Points for a formulae y = f(x) across an x range
--
-- >>> gridP (**2) (Range 0 4) 4
-- [Point 0.0 0.0,Point 1.0 1.0,Point 2.0 4.0,Point 3.0 9.0,Point 4.0 16.0]
gridP :: (FieldSpace (Range a)) => (a -> a) -> Range a -> Grid (Range a) -> [Point a]
gridP f r g = (\x -> Point x (f x)) <$> grid OuterPos r g
