-- Red-Black trees.
--  (was previously implemented as 2-3-4 trees, hence the module name.)
module Tree234
( Tree,
  initTree,
  treeAdd,
  treeAddList,
  treeFromList,
  treeSearch,
  treeUpdate,
  treeMap,
  treeMapList
) where

data Tree a
  = R (Tree a) a (Tree a)
  | B (Tree a) a (Tree a)
  | E
  deriving Show

initTree :: Tree a
initTree = E

treeAdd :: (a -> a -> a) -> (a -> a -> Ordering) -> a -> Tree a -> Tree a
treeAdd comb cmp a0 t0 = mkB (ins t0)
  where
    mkB (R l a r) = B l a r
    mkB t         = t

    ins E = R E a0 E
    ins (B l b r)
      = case (cmp a0 b) of
        LT -> lbal (ins l) b r
        EQ -> B l (a0 `comb` b) r
        GT -> rbal l b (ins r)
    ins (R l b r)
      = case (cmp a0 b) of
        LT -> R (ins l) b r
        EQ -> R l (a0 `comb` b)  r
        GT -> R l b (ins r)

    lbal :: Tree a -> a -> Tree a -> Tree a
    lbal (R (R a x b) y c) z d = R (B a x b) y (B c z d)
    lbal (R a x (R b y c)) z d = R (B a x b) y (B c z d)
    lbal a x b                 = B a x b

    rbal :: Tree a -> a -> Tree a -> Tree a
    rbal a x (R (R b y c) z d) = R (B a x b) y (B c z d)
    rbal a x (R b y (R c z d)) = R (B a x b) y (B c z d)
    rbal a x b                 = B a x b

treeAddList :: (a -> a -> a) -> (a -> a -> Ordering) -> [a] -> Tree a -> Tree a
treeAddList comb cmp xs t = foldr (treeAdd comb cmp) t xs

treeFromList :: (a -> a -> a) -> (a -> a -> Ordering) -> [a] -> Tree a
treeFromList comb cmp l = treeAddList comb cmp l E

treeSearch :: b -> (a -> b) -> (a -> Ordering) -> Tree a -> b
treeSearch myfail _cont _p E = myfail
treeSearch myfail cont p (R l a r)
  = case (p a) of
    LT -> treeSearch myfail cont p l
    EQ -> cont a
    GT -> treeSearch myfail cont p r
treeSearch myfail cont p (B l a r)
  = case (p a) of
    LT -> treeSearch myfail cont p l
    EQ -> cont a
    GT -> treeSearch myfail cont p r

treeUpdate :: (a -> a) -> (a -> Ordering) -> Tree a -> Tree a
treeUpdate _update _p E = E
treeUpdate update p (R l a r)
  = case (p a) of
    LT -> R (treeUpdate update p l) a r
    EQ -> R l (update a) r
    GT -> R l a (treeUpdate update p r)
treeUpdate update p (B l a r)
  = case (p a) of
    LT -> B (treeUpdate update p l) a r
    EQ -> B l (update a) r
    GT -> B l a (treeUpdate update p r)

treeMap :: (a -> b) -> Tree a -> Tree b
treeMap _f E         = E
treeMap f (B l a r) = B (treeMap f l) (f a) (treeMap f r)
treeMap f (R l a r) = R (treeMap f l) (f a) (treeMap f r)

treeMapList :: (a -> [b] -> [b]) -> Tree a -> [b]
treeMapList f t = treeFold f [] t

treeFold :: (a -> b -> b) -> b -> Tree a -> b
treeFold _f c E         = c
treeFold f c (B l a r) = treeFold f (a `f` treeFold f c r) l
treeFold f c (R l a r) = treeFold f (a `f` treeFold f c r) l
