module Util where

import Data.Map ((!), Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)

(!@) :: (Ord k, Monoid a) => Map k a -> k -> a
map !@ key = Map.findWithDefault mempty key map

(!?) :: (Ord k) => Map k a -> k -> Maybe a
map !? key = Map.lookup key map

(??) :: Maybe a -> a -> a
(??) = flip fromMaybe
