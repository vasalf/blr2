module Data.MyMap where

import qualified Data.Map.Strict as Map

data Map k a = MyMap { getMap  :: Map.Map k a
                     , updates :: Int
                     }
    deriving Show


empty :: Map k a
empty = MyMap Map.empty 0

lookup :: Ord k => k -> Map k a -> Maybe a
lookup x = Map.lookup x . getMap

insert :: Ord k => k -> a -> Map k a -> Map k a
insert x y (MyMap m z) = MyMap (Map.insert x y m) (z + 1)

notMember :: Ord k => k -> Map k a -> Bool
notMember x = Map.notMember x . getMap
