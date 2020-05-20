module Util where

import Data.Map (Map, (!))
import qualified Data.Map as Map

(!@) :: (Ord k, Monoid a) => Map k a -> k -> a
map !@ key = Map.findWithDefault mempty key map

(!?) :: (Ord k) => Map k a -> k -> Maybe a
map !? key = Map.lookup key map