{- |

A very modest library for join-lists, also known as conc-lists.

-}

module Lava.JList
  ( JList(..)     -- data JList a = Zero | One a | JList a :+: JList a
  , fromList      -- :: [a] -> Jlist a
  , toList        -- :: JList a -> [a]
  , map           -- :: (a -> b) -> JList a -> JList b
  , mapM          -- :: Monad m => (a -> m b) -> JList a -> m (JList b)
  , concat        -- :: JList (JList a) -> JList a
  , zipWith       -- :: (a -> b -> c) -> JList a -> JList b -> JList c
  , lazyZipWith   -- :: (a -> b -> c) -> JList a -> JList b -> JList c
  ) where

import Prelude hiding (map, mapM, concat, zipWith)
import Control.Applicative (Applicative(..))
import Control.Monad hiding (mapM)

data JList a = Zero | One a | JList a :+: JList a
  deriving (Show, Eq)

fromList :: [a] -> JList a
fromList as = foldr (:+:) Zero [One a | a <- as]

toList :: JList a -> [a]
toList a = flatten a []
  where
    flatten Zero rest = rest
    flatten (One a) rest = a:rest
    flatten (a :+: b) rest = flatten a (flatten b rest)

instance Functor JList where
  fmap f Zero = Zero
  fmap f (One a) = One (f a)
  fmap f (as :+: bs) = fmap f as :+: fmap f bs

instance Monad JList where
  return a = One a
  Zero >>= f = Zero
  One a >>= f = f a
  (as :+: bs) >>= f = (as >>= f) :+: (bs >>= f)

instance Applicative JList where
  pure  = return
  (<*>) = ap

map :: (a -> b) -> JList a -> JList b
map = fmap

mapM :: Monad m => (a -> m b) -> JList a -> m (JList b)
mapM f Zero = return Zero
mapM f (One a) = liftM One (f a)
mapM f (as :+: bs) = liftM2 (:+:) (mapM f as) (mapM f bs)

concat :: JList (JList a) -> JList a
concat Zero = Zero
concat (One as) = as
concat (as :+: bs) = concat as :+: concat bs

zipWith :: (a -> b -> c) -> JList a -> JList b -> JList c
zipWith f Zero Zero = Zero
zipWith f (One a) (One b) = One (f a b)
zipWith f (a0 :+: a1) (b0 :+: b1) = zipWith f a0 b0 :+: zipWith f a1 b1
zipWith f _ _ = error "JList.zipWith: incompatible structures"

lazyZipWith :: (a -> b -> c) -> JList a -> JList b -> JList c
lazyZipWith f Zero x = Zero
lazyZipWith f (One a) x = let One b = x in One (f a b)
lazyZipWith f (a0 :+: a1) x =
  let b0 :+: b1 = x in lazyZipWith f a0 b0 :+: lazyZipWith f a1 b1
