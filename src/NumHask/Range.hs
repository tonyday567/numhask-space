{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

-- | An Space with no empty, a semigroup based on a convex hull union, and a monoid on a negative space.
module NumHask.Range
  ( Range(..)
  , pattern Range
  , gridSensible
 ) where

import Prelude
import Data.Functor.Rep
import Data.Distributive as D
import Data.Bool (bool)
import Data.Functor.Apply (Apply(..))
import Data.Functor.Classes
import Data.Semigroup.Foldable (Foldable1(..))
import Data.Semigroup.Traversable (Traversable1(..))
import GHC.Exts
import GHC.Generics (Generic)
import NumHask.Space as S
import Algebra.Lattice

-- $setup
-- >>> :set -XNoImplicitPrelude
-- >>> :set -XFlexibleContexts

-- | A continuous range over type a
--
-- >>> let a = Range (-1) 1
-- >>> a
-- Range -1 1
-- >>> fmap (+1) (Range 1 2)
-- Range 2 3
-- >>> one :: Range Double
-- Range -0.5 0.5
-- >>> zero :: Range Double
-- Range Infinity -Infinity

-- | as a Field instance
--
-- >>> Range 0 1 + zero
-- Range 0.0 1.0
-- >>> Range 0 1 + Range 2 3
-- Range 0.0 3.0
-- >>> Range 1 1 - one
-- Range 0.5 1.0
-- >>> Range 0 1 * one
-- Range 0.0 1.0
-- >>> Range 0 1 / one
-- Range 0.0 1.0
-- >>> abs (Range 1 0)
-- Range 0.0 1.0
-- >>> sign (Range 1 0) == negate one
-- True
--
-- Idempotent
--
-- >>> Range 0 2 + Range 0 2
-- Range 0.0 2.0
--
-- as a space instance
--
-- >>> NumHask.Space.project (Range 0 1) (Range 1 4) 0.5
-- 2.5
-- >>> NumHask.Space.grid NumHask.Space.OuterPos (Range 0 10) 5
-- [0.0,2.0,4.0,6.0,8.0,10.0]
-- >>> NumHask.Space.gridSpace (Range 0 1) 4
-- [Range 0.0 0.25,Range 0.25 0.5,Range 0.5 0.75,Range 0.75 1.0]
-- >>> gridSensible NumHask.Space.OuterPos (Range (-12.0) 23.0) 6
-- [-10.0,-5.0,0.0,5.0,10.0,15.0,20.0]

newtype Range a = Range' (a,a)
  deriving (Eq, Generic)

-- not sure if this is correct or needed
type role Range representational

-- | A tuple is the preferred concrete implementation of a Range, due to many libraries having substantial optimizations for tuples already (eg 'Vector').  'Pattern Synonyms' allow us to recover a constructor without the need for tuple syntax.
pattern Range :: a -> a -> Range a
pattern Range a b = Range' (a,b)
{-# COMPLETE Range#-}

instance (Show a) => Show (Range a) where
    show (Range a b) = "Range " <> show a <> " " <> show b

instance Eq1 Range where
    liftEq f (Range a b) (Range c d) = f a c && f b d

instance Show1 Range where
    liftShowsPrec sp _ d (Range' (a,b)) = showsBinaryWith sp sp "Range" d a b

instance Functor Range where
    fmap f (Range a b) = Range (f a) (f b)

instance Apply Range where
  Range fa fb <.> Range a b = Range (fa a) (fb b)

instance Applicative Range where
    pure a = Range a a
    (Range fa fb) <*> Range a b = Range (fa a) (fb b)

instance Foldable Range where
  foldMap f (Range a b) = f a `mappend` f b

instance Foldable1 Range

instance Traversable Range where
    traverse f (Range a b) = Range <$> f a <*> f b

instance Traversable1 Range where
    traverse1 f (Range a b) = Range <$> f a Data.Functor.Apply.<.> f b

instance D.Distributive Range where
  collect f x = Range (getL . f <$> x) (getR . f <$> x)
    where getL (Range l _) = l
          getR (Range _ r) = r

instance Representable Range where
  type Rep Range = Bool
  tabulate f = Range (f False) (f True)
  index (Range l _) False = l
  index (Range _ r) True = r

instance (Lattice a) => Lattice (Range a) where
  (\/) = liftR2 (\/)
  (/\) = liftR2 (/\)

instance (Eq a, BoundedLattice a) => BoundedJoinSemiLattice (Range a) where
  bottom = top >.< bottom

instance (Eq a, BoundedLattice a) => BoundedMeetSemiLattice (Range a) where
  top = bottom >.< top

instance (Eq a, Lattice a) => Space (Range a) where
  type Element (Range a) = a

  lower (Range l _) = l
  upper (Range _ u) = u

  (>.<) = Range

instance (Eq a, Lattice a, Fractional a) => FieldSpace (Range a) where
    type Grid (Range a) = Int

    grid o s n = (+ bool 0 (step/2) (o==MidPos)) <$> posns
      where
        posns = (lower s +) . (step *) . fromIntegral <$> [i0..i1]
        step = (/) (width s) (fromIntegral n)
        (i0,i1) = case o of
                    OuterPos -> (0,n)
                    InnerPos -> (1,n - 1)
                    LowerPos -> (0,n - 1)
                    UpperPos -> (1,n)
                    MidPos -> (0,n - 1)
    gridSpace r n = zipWith Range ps (drop 1 ps)
      where
        ps = grid OuterPos r n

-- | Monoid based on convex hull union
instance (Eq a, Lattice a) => Semigroup (Range a) where
  (<>) a b = getUnion (Union a <> Union b)

instance (Spaceable a) => Monoid (Range a) where
  mempty = getUnion mempty

-- | Numeric algebra based on Interval arithmetic
-- https://en.wikipedia.org/wiki/Interval_arithmetic
--

instance (Num a, Eq a, Lattice a) => Num (Range a) where
  (Range l u) + (Range l' u') = space1 [l+l',u+u']
  negate (Range l u) = negate u ... negate l
  (Range l u) * (Range l' u') =
    space1 [l * l', l * u', u * l', u * u']
  signum (Range l u) = bool (negate 1) 1 (u `joinLeq` l)
  abs (Range l u) = bool (u ... l) (l ... u) (u `joinLeq` l)
  fromInteger x = fromInteger x ... fromInteger x

instance (Spaceable a, Fractional a) => Fractional (Range a) where
  fromRational x = fromRational x ... fromRational x
  recip i@(Range l u)
    | 0 |.| i && not (1e-6 |.| i) = bottom ... recip l
    | 0 |.| i && not (negate 1e-6 |.| i) = top ... recip l
    | 0 |.| i = whole
    | otherwise = recip l ... recip u

stepSensible :: (Fractional a, RealFrac a, Floating a, Integral b) => Pos -> a -> b -> a
stepSensible tp span' n =
    step + bool 0 (step/2) (tp==MidPos)
  where
    step' = 10.0 ^^ (floor (logBase 10 (span'/fromIntegral n)) :: Integer)
    err = fromIntegral n / span' * step'
    step
      | err <= 0.15 = 10.0 * step'
      | err <= 0.35 = 5.0 * step'
      | err <= 0.75 = 2.0 * step'
      | otherwise = step'

gridSensible :: (Ord a, Lattice a, RealFrac a, Floating a, Integral b) =>
    Pos -> Bool -> Range a -> b -> [a]
gridSensible tp inside r@(Range l u) n =
    bool id (filter (`memberOf` r)) inside $
    (+ bool 0 (step/2) (tp==MidPos)) <$> posns
  where
    posns = (first' +) . (step *) . fromIntegral <$> [i0..i1]
    span' = u - l
    step = stepSensible tp span' n
    first' = step * fromIntegral (floor (l/step + 1e-6) :: Integer)
    last' =  step * fromIntegral (ceiling (u/step - 1e-6) :: Integer)
    n' = round ((last' - first')/step)
    (i0,i1) =
      case tp of
        OuterPos -> (0::Integer,n')
        InnerPos -> (1,n' - 1)
        LowerPos -> (0,n' - 1)
        UpperPos -> (1,n')
        MidPos -> (0,n' - 1)
