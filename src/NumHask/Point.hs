{-# LANGUAGE CPP #-}
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
  ( Point(..)
  , pattern Point
  ) where

import Prelude
import GHC.Generics (Generic)
import Data.Functor.Classes
import Text.Show
import Algebra.Lattice
import Data.Functor.Rep
import Data.Distributive as D

-- $setup
-- >>> :set -XNoImplicitPrelude
-- >>> :set -XFlexibleContexts
--

-- | A 2-dim point of a's, implemented as a tuple, but api represented as Point a a.
--
-- >>> fmap (+1) (Point 1 2)
-- Point 2 3
-- >>> pure one :: Point Int
-- Point 1 1
-- >>> (*) <$> Point 1 2 <*> pure 2
-- Point 2 4
-- >>> foldr (++) [] (Point [1,2] [3])
-- [1,2,3]
-- >>> Point "a" "pair" `mappend` pure " " `mappend` Point "string" "mappended"
-- Point "a string" "pair mappended"
--
-- As a Ring and Field class
-- 
-- >>> Point 0 1 + zero
-- Point 0 1
-- >>> Point 0 1 + Point 2 3
-- Point 2 4
-- >>> Point 1 1 - one
-- Point 0 0
-- >>> Point 0 1 * one
-- Point 0 1
-- >>> Point 0.0 1.0 / one
-- Point 0.0 1.0
-- >>> Point 11 12 `mod` (pure 6)
-- Point 5 0
newtype Point a =
  Point' (a, a)
  deriving (Eq, Generic)

-- | the preferred pattern
pattern Point :: a -> a -> Point a
pattern Point a b = Point' (a,b)
{-# COMPLETE Point#-}

instance (Show a) => Show (Point a) where
  show (Point a b) = "Point " <> Text.Show.show a <> " " <> Text.Show.show b

instance Functor Point where
  fmap f (Point a b) = Point (f a) (f b)

instance Eq1 Point where
  liftEq f (Point a b) (Point c d) = f a c && f b d

instance Show1 Point where
  liftShowsPrec sp _ d (Point' (a, b)) = showsBinaryWith sp sp "Point" d a b

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
    where getL (Point l _) = l
          getR (Point _ r) = r

instance Representable Point where
  type Rep Point = Bool
  tabulate f = Point (f False) (f True)
  index (Point l _) False = l
  index (Point _ r) True = r

instance (Lattice a) => Lattice (Point a) where
  (\/) = liftR2 (\/)
  (/\) = liftR2 (/\)

instance (BoundedLattice a) => BoundedJoinSemiLattice (Point a) where
  bottom = Point bottom bottom

instance (BoundedLattice a) => BoundedMeetSemiLattice (Point a) where
  top = Point top top

