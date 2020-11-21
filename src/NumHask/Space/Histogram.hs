{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RebindableSyntax #-}
{-# OPTIONS_GHC -Wall #-}

-- | A histogram, if you squint, is a series of contiguous 'Range's, annotated with values.
module NumHask.Space.Histogram
  ( Histogram (..),
    DealOvers (..),
    fill,
    cutI,
    regular,
    makeRects,
    regularQuantiles,
    quantileFold,
    fromQuantiles,
    freq,
  )
where

import qualified Data.List as List
import qualified Data.Map as Map
import Data.TDigest
import NumHask.Prelude
import NumHask.Space.Range
import NumHask.Space.Rect
import NumHask.Space.Types

-- | This Histogram is a list of contiguous boundaries (a boundary being the lower edge of one bucket and the upper edge of another), and a value (usually a count) for each bucket, represented here as a map
--
-- Overs and Unders are contained in key = 0 and key = length cuts
data Histogram = Histogram
  { cuts :: [Double], -- bucket boundaries
    values :: Map.Map Int Double -- bucket counts
  }
  deriving (Show, Eq)

-- | Whether or not to ignore unders and overs.  If overs and unders are dealt with, IncludeOvers supplies an assumed width for the outer buckets.
data DealOvers = IgnoreOvers | IncludeOvers Double

-- | Fill a Histogram using pre-specified cuts
--
-- >>> fill [0,50,100] [1..100]
-- Histogram {cuts = [0.0,50.0,100.0], values = fromList [(1,50.0),(2,50.0)]}
fill :: (Foldable f) => [Double] -> f Double -> Histogram
fill cs xs = Histogram cs (foldl' (\x a -> Map.insertWith (+) (cutI cs a) 1 x) Map.empty xs)

-- | find the index of the bucket the value is contained in.
cutI :: (Ord a) => [a] -> a -> Int
cutI bs n = go bs 0
  where
    go [] i = i
    go (x : xs) i = bool i (go xs (i + 1)) (n > x)

-- | Make a histogram using n equally spaced cuts over the entire range of the data
--
-- >>> regular 4 [0..100]
-- Histogram {cuts = [0.0,25.0,50.0,75.0,100.0], values = fromList [(0,1.0),(1,25.0),(2,25.0),(3,25.0),(4,25.0)]}
regular :: Int -> [Double] -> Histogram
regular n xs = fill cs xs
  where
    cs = grid OuterPos (space1 xs :: Range Double) n

-- | Transform a Histogram to Rects
--
-- >>> makeRects IgnoreOvers (regular 4 [0..100])
-- [Rect 0.0 25.0 0.0 0.25,Rect 25.0 50.0 0.0 0.25,Rect 50.0 75.0 0.0 0.25,Rect 75.0 100.0 0.0 0.25]
makeRects :: DealOvers -> Histogram -> [Rect Double]
makeRects o (Histogram cs counts) = List.zipWith4 Rect x z y w'
  where
    y = repeat 0
    w =
      zipWith
        (/)
        ((\x' -> Map.findWithDefault 0 x' counts) <$> [f .. l])
        (zipWith (-) z x)
    f = case o of
      IgnoreOvers -> 1
      IncludeOvers _ -> 0
    l = case o of
      IgnoreOvers -> length cs - 1
      IncludeOvers _ -> length cs
    w' = (/ sum w) <$> w
    x = case o of
      IgnoreOvers -> cs
      IncludeOvers outw ->
        [List.head cs - outw]
          <> cs
          <> [List.last cs + outw]
    z = drop 1 x

-- | approx regular n-quantiles
--
-- >>> regularQuantiles 4 [0..100]
-- [0.0,24.75,50.0,75.25,100.0]
regularQuantiles :: Double -> [Double] -> [Double]
regularQuantiles n xs = quantileFold qs xs
  where
    qs = ((1 / n) *) <$> [0 .. n]

-- | one-pass approximate quantiles fold
quantileFold :: [Double] -> [Double] -> [Double]
quantileFold qs xs = done $ foldl' step begin xs
  where
    step x a = Data.TDigest.insert a x
    begin = tdigest ([] :: [Double]) :: TDigest 25
    done x = fromMaybe (0 / 0) . (`quantile` compress x) <$> qs

-- | take a specification of quantiles and make a Histogram
--
-- >>> fromQuantiles [0,0.25,0.5,0.75,1] (regularQuantiles 4 [0..100])
-- Histogram {cuts = [0.0,24.75,50.0,75.25,100.0], values = fromList [(1,0.25),(2,0.25),(3,0.25),(4,0.25)]}
fromQuantiles :: [Double] -> [Double] -> Histogram
fromQuantiles qs xs = Histogram xs (Map.fromList $ zip [1 ..] (diffq qs))
  where
    diffq [] = []
    diffq [_] = []
    diffq (x : xs') = (reverse . snd) $ foldl' step (x, []) xs'
    step (a0, xs') a = (a, (a - a0) : xs')

-- | normalize a histogram
--
-- > \h -> sum (values $ freq h) == one
--
-- >>> freq $ fill [0,50,100] [1..100]
-- Histogram {cuts = [0.0,50.0,100.0], values = fromList [(1,0.5),(2,0.5)]}
freq :: Histogram -> Histogram
freq (Histogram cs vs) = Histogram cs $ Map.map (* recip (sum vs)) vs
