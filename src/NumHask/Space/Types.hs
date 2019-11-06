{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module NumHask.Space.Types
  ( Space(..)
  , Union(..)
  , Intersection(..)
  , FieldSpace(..)
  , mid
  , project
  , Pos(..)
  , space1
  , memberOf
  , contains
  , disjoint
  , width
  , (+/-)
  , monotone
  , eps
  , widen
  , widenEps
  , scale
  , move
  )

where

class Space s where

  -- | the underlying element in the space
  type Element s :: *

  -- | lower boundary
  lower :: s -> Element s

  -- | upper boundary
  upper :: s -> Element s

  -- | space containing a single element
  singleton :: Element s -> s
  singleton s = s >.< s

  -- | the intersection of two spaces
  intersection :: s -> s -> s

  default intersection :: (Ord (Element s)) => s -> s -> s
  intersection a b = l >.< u where
      l = lower a `max` lower b
      u = upper a `min` upper b

  -- | the union of two spaces
  union :: s -> s -> s
  default union :: (Ord (Element s)) => s -> s -> s
  union a b = l >.< u where
    l = lower a `min` lower b
    u = upper a `max` upper b

  -- | Normalise a space so that
  -- > lower a \/ upper a == lower a
  -- > lower a /\ upper a == upper a
  norm :: s -> s
  norm s = lower s ... upper s

  -- | create a normalised space from two elements
  infix 3 ...
  (...) :: Element s -> Element s -> s
  default (...) :: (Ord (Element s)) => Element s -> Element s -> s
  (...) a b = (a `min` b) >.< (a `max` b)

  -- | create a space from two elements without normalising
  infix 3 >.<
  (>.<) :: Element s -> Element s -> s

  -- | is an element in the space
  infixl 7 |.|
  (|.|) :: Element s -> s -> Bool
  default (|.|) :: (Ord (Element s)) => Element s -> s -> Bool
  (|.|) a s = (a >= lower s) && (upper s >= a)

  -- | is one space completely above the other
  infixl 7 |>|
  (|>|) :: s -> s -> Bool
  default (|>|) :: (Ord (Element s)) => s -> s -> Bool
  (|>|) s0 s1 =
    lower s0 >= upper s1

  -- | is one space completely below the other
  infixl 7 |<|
  (|<|) :: s -> s -> Bool
  default (|<|) :: (Ord (Element s)) => s -> s -> Bool
  (|<|) s0 s1 =
    lower s1 <= upper s0

-- | is a space contained within another?
contains :: (Space s) => s -> s -> Bool
contains s0 s1 =
  lower s1 |.| s0 &&
  upper s1 |.| s0

-- | are two spaces disjoint?
disjoint :: (Space s) => s -> s -> Bool
disjoint s0 s1 = s0 |>| s1 || s0 |<| s1

-- (|.|) a s = (a `joinLeq` lower s) && (upper s `meetLeq` a)
memberOf :: (Space s) => Element s -> s -> Bool
memberOf = (|.|)

-- | distance between boundaries
width :: (Space s, Num (Element s)) => s -> Element s
width s = upper s - lower s

-- | create a space centered on a plus or minus b
infixl 6 +/-
(+/-) :: (Space s, Num (Element s)) => Element s -> Element s -> s
a +/- b = a - b ... a + b

newtype Union a = Union { getUnion :: a }

instance (Space a) => Semigroup (Union a) where
  (<>) (Union a) (Union b) = Union (a `union` b)

newtype Intersection a = Intersection { getIntersection :: a }

instance (Space a) => Semigroup (Intersection a) where
  (<>) (Intersection a) (Intersection b) = Intersection (a `union` b)

-- | a space that can be divided neatly
--
class (Space s, Num (Element s)) => FieldSpace s where
  type Grid s :: *

  -- | create equally-spaced elements across a space
  grid :: Pos -> s -> Grid s -> [Element s]

  -- | create equally-spaced spaces from a space
  gridSpace :: s -> Grid s -> [s]

-- | Pos suggests where points should be placed in forming a grid across a field space.
data Pos = OuterPos | InnerPos | LowerPos | UpperPos | MidPos deriving (Show, Eq)

-- | mid-point of the space
mid :: (Space s, Fractional (Element s)) => s -> Element s
mid s = (lower s + upper s)/2.0

-- | project a data point from one space to another, preserving relative position
--
-- > project o n (lower o) = lower n
-- > project o n (upper o) = upper n
-- > project a a x = x
-- > project mempty one zero = NaN
-- > project one mempty zero = Infinity
-- > project one mempty one = NaN
--
project :: (Space s, Fractional (Element s)) => s -> s -> Element s -> Element s
project s0 s1 p =
  ((p-lower s0)/(upper s0-lower s0)) * (upper s1-lower s1) + lower s1

-- | the containing space of a non-empty Traversable
space1 :: (Space s, Traversable f) => f (Element s) -> s
space1 = foldr1 union . fmap singleton

-- | lift a monotone function (increasing or decreasing) over a given space
monotone :: (Space a, Space b) => (Element a -> Element b) -> a -> b
monotone f s = space1 [f (lower s), f (upper s)]

-- | a small space
eps ::
    ( Space s
    , Fractional (Element s)
    )
    => Element s -> Element s -> s
eps accuracy a = a +/- (accuracy * a * 1e-6)

-- | widen a space
widen ::
    ( Space s
    , Num (Element s))
    => Element s -> s -> s
widen a s = (lower s - a) >.< (upper s + a)

-- | widen by a small amount
widenEps ::
    ( Space s
    , Fractional (Element s)
    )
    => Element s -> s -> s
widenEps accuracy = widen (accuracy * 1e-6)

-- | scale a Space
scale :: (Num (Element s), Space s) => Element s -> s -> s
scale e s = (e * lower s) ... (e * upper s)

-- | move a Space
move :: (Num (Element s), Space s) => Element s -> s -> s
move e s = (e + lower s) ... (e + upper s)



