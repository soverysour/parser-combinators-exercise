module HashMap
  ( HashMap
  , empty
  , insert
  , union
  ) where

import qualified Data.Vector as V
import           Data.Hashable (Hashable, hash)
import           Data.List (nub)

data HashMap v = HashMap
  { vsize :: !Int
  , buckets :: !(V.Vector [v])
  }

instance (Show v) => Show (HashMap v) where
  show (HashMap vsize' buckets') = show . filter ((/=0) . length) $ V.toList buckets'

contains_ :: (Eq v) => v -> [v] -> Bool
contains_ _ [] = False
contains_ x (y:ys) =
  if x == y
     then True
     else contains_ x ys

empty :: HashMap v
empty = HashMap vsize' (V.replicate vsize' [])
  where vsize' = 100

insert :: (Show v, Eq v) => v -> HashMap v -> HashMap v
insert v (HashMap vsize' buckets') = HashMap vsize' newBuckets
  where index' = length (show v) `mod` vsize'
        prevVal = buckets' V.! index'
        newBuckets = if contains_ v prevVal
                   then buckets'
                   else buckets' V.// [(index', v : prevVal)]

union :: (Eq v) => HashMap v -> HashMap v -> HashMap v
union (HashMap vsize1 buckets1) (HashMap vsize2 buckets2) = HashMap vsize' buckets'
  where vsize' = vsize1
        buckets' = V.zipWith f buckets1 buckets2
        f a b = nub $ a ++ b
