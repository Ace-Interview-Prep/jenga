-- TODO: give this a home
module Data.Map.Monoidal.Extras where

import qualified Data.Map as Map
import Data.Map (Map)
import Data.Set (Set)
import Data.Map.Monoidal (MonoidalMap(..))
import Data.Coerce (coerce)

restrictKeys :: Ord k => MonoidalMap k a -> Set k -> MonoidalMap k a
restrictKeys =
  (coerce
    :: (Map k a -> Set k -> Map k a)
    -> (MonoidalMap k a -> Set k -> MonoidalMap k a)
  )
  Map.restrictKeys
{-# INLINE restrictKeys #-}

withoutKeys :: Ord k => MonoidalMap k a -> Set k -> MonoidalMap k a
withoutKeys =
  (coerce
    :: (Map k a -> Set k -> Map k a)
    -> (MonoidalMap k a -> Set k -> MonoidalMap k a)
  )
  Map.withoutKeys
{-# INLINE withoutKeys #-}
