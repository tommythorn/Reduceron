module AssocTree
  ( AssocTree
  , Tree
  , initAT      -- :: AssocTree k v
  , listAT      -- :: AssocTree k v -> [(k,v)]
  , reorderAT   -- :: (AssocTree k v -> (j,u) -> AssocTree k v)
                --                       -> AssocTree j u -> AssocTree k v
  , addAT       -- :: Ord k =>
                --       AssocTree k v -> (v->v->v) -> k -> v -> AssocTree k v
  , lookupAT    -- :: Ord a =>
                --       AssocTree k v -> k -> Maybe v
  , updateAT    -- :: Ord k =>
                --       AssocTree k v -> k -> (v->v) -> AssocTree k v
  , mapAT       -- :: (v -> w) -> AssocTree k v -> AssocTree k w
  ) where

import Tree234

newtype AssocTree k v = AssocTree (Tree (k,v))

instance (Show k, Show v) => Show (AssocTree k v) where
  show t = show (listAT t)

initAT :: AssocTree k v
initAT = AssocTree initTree

listAT :: AssocTree k v -> [(k,v)]
listAT (AssocTree t) = treeMapList (:) t

reorderAT :: (AssocTree k v -> (j,u) -> AssocTree k v)
         -> AssocTree j u -> AssocTree k v
reorderAT translate t =
   foldl translate initAT (listAT t)

cmp1 :: (Ord a) => a -> (a, t) -> Ordering
cmp1 key (key',_value) = compare key key'

cmp2 :: (Ord a) => a -> t -> (a, t1) -> Ordering
cmp2 key _ (key',_value) = compare key key'

addAT :: Ord k => AssocTree k v -> (v->v->v) -> k -> v -> AssocTree k v
addAT (AssocTree t) comb key value =
  AssocTree $ treeAdd combine (cmp2 key) (key,value) t
 where
   combine (_k1,v1) (k2,v2) = (k2,comb v1 v2)

lookupAT :: Ord k => AssocTree k v -> k -> Maybe v
lookupAT (AssocTree t) key =
   treeSearch Nothing ok (cmp1 key) t
  where
   ok (_key,value) = Just value

updateAT :: Ord k => AssocTree k v -> k -> (v->v) -> AssocTree k v
updateAT (AssocTree t) key upd =
  AssocTree $ treeUpdate update (cmp1 key) t
 where
   update (k1,v1) = (k1,upd v1)

mapAT :: (v -> w) -> AssocTree k v -> AssocTree k w
mapAT f (AssocTree t) = AssocTree $ treeMap (\(k, v) -> (k, f v))  t
