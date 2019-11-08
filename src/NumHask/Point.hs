{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wall #-}

-- | A 2-dimensional point.
module NumHask.Point
  ( Point (..),
    rotate,
    gridP,
  )
where

import Algebra.Lattice
import Data.Distributive as D
import Data.Functor.Classes
import Data.Functor.Rep
import GHC.Generics (Generic)
import NumHask.Range
import NumHask.Space.Types
import Text.Show
import Prelude

-- $setup
-- >>> :set -XNoImplicitPrelude
-- >>> :set -XFlexibleContexts

-- | A 2-dim point of a's
data Point a
  = Point a a
  deriving (Eq, Generic)

instance (Show a) => Show (Point a) where
  show (Point a b) = "Point " <> Text.Show.show a <> " " <> Text.Show.show b

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

unaryOp :: (a -> a) -> (Point a -> Point a)
unaryOp f (Point a b) = Point (f a) (f b)

instance (Num a) => Num (Point a) where

  (Point a0 b0) + (Point a1 b1) = Point (a0 + a1) (b0 + b1)

  negate = unaryOp negate

  (Point a0 b0) * (Point a1 b1) = Point (a0 * a1) (b0 * b1)

  signum = unaryOp signum

  abs = unaryOp abs

  fromInteger x = Point (fromInteger x) (fromInteger x)

instance (Fractional a) => Fractional (Point a) where

  fromRational x = Point (fromRational x) 0

  recip = unaryOp recip

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

instance (Ord a) => Lattice (Point a) where

  (\/) (Point x y) (Point x' y') = Point (max x x') (max y y')

  (/\) (Point x y) (Point x' y') = Point (min x x') (min y y')

-- | rotate a point by x degrees relative to the origin
rotate :: (Floating a) => a -> Point a -> Point a
rotate d (Point x y) = Point (x * cos d' + y * sin d') (y * cos d' - x * sin d')
  where
    d' = d * pi / 180

-- | Create Points for a formulae y = f(x) across an x range
gridP :: (Ord a, Fractional a) => (a -> a) -> Range a -> Int -> [Point a]
gridP f r g = (\x -> Point x (f x)) <$> grid OuterPos r g
