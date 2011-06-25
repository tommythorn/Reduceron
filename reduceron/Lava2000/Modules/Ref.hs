module Ref
  ( Ref       -- * -> * ; Eq, Show
  , ref       -- :: a     -> Ref a
  , deref     -- :: Ref a -> a
  , memoRef   -- :: (Ref a -> b) -> (Ref a -> b)

  , TableIO   -- :: * -> * -> * ; Eq
  , tableIO   -- :: IO (TableIO a b)
  , extendIO  -- :: TableIO a b -> Ref a -> b -> IO ()
  , findIO    -- :: TableIO a b -> Ref a -> IO (Maybe b)
  , memoRefIO -- :: (Ref a -> IO b) -> (Ref a -> IO b)

  , TableST   -- :: * -> * -> * -> * ; Eq
  , tableST   -- :: ST (TableST s a b)
  , extendST  -- :: TableST s a b -> Ref a -> b -> ST s ()
  , findST    -- :: TableST s a b -> Ref a -> ST s (Maybe b)
  , memoRefST -- :: (Ref a -> ST s b) -> (Ref a -> ST s b)
  )
 where

import MyST

import System.IO
import System.IO.Unsafe
import Data.IORef

import UnsafeCoerce
  ( unsafeCoerce
  )

{-

Warning! One should regard this module as a portable
extension to the Haskell language. It is not Haskell.

-}

{-

Here is how we implement Tables of Refs:

A Table is nothing but a unique tag, of type TableTag.
TableTag can be anything, as long as it is easy
to create new ones, and we can compare them for
equality. (I chose IORef ()).

So how do we store Refs in a Table? We do not
want the Tables keeping track of their Refs
(which would be disastrous when the table
becomes big, and we would not have any garbage
collection).

Instead, every Ref keeps track of the value it
has in each table it is in. This has the advantage
that we have a constant lookup time (if the number of
Tables we are using is small), and we get garbage
collection of table entries for free.

The disadvantage is that, since the types of the
Tables vary, the Ref has no idea what type of
values it is supposed to store. So we use dynamic
types.

A Ref is implemented as follows: it has two pieces
of information. The first one is an updatable
list of entries for each table it is a member in.
Since it is an updatable list, it is an IORef, which
we also use to compare two Refs. The second part is
just the value the Ref is pointing at (this can never
change anyway).

-}

-----------------------------------------------------------------
-- Ref

data Ref a
  = Ref (IORef [(TableTag, Dyn)]) a

instance Eq (Ref a) where
  Ref r1 _ == Ref r2 _ = r1 == r2

instance Show a => Show (Ref a) where
  showsPrec _ (Ref _ a) = showChar '{' . shows a . showChar '}'

ref :: a -> Ref a
ref a = unsafePerformIO $
  do r <- newIORef []
     return (Ref r a)

deref :: Ref a -> a
deref (Ref _ a) = a

-----------------------------------------------------------------
-- Table IO

type TableTag
  = IORef ()

newtype TableIO a b
  = TableIO TableTag
 deriving Eq

tableIO :: IO (TableIO a b)
tableIO = TableIO `fmap` newIORef ()

findIO :: TableIO a b -> Ref a -> IO (Maybe b)
findIO (TableIO t) (Ref r _) =
  do list <- readIORef r
     return (fromDyn `fmap` lookup t list)

extendIO :: TableIO a b -> Ref a -> b -> IO ()
extendIO (TableIO t) (Ref r _) b =
  do list <- readIORef r
     writeIORef r ((t,toDyn b) : filter ((/= t) . fst) list)

-----------------------------------------------------------------
-- Table ST

newtype TableST s a b
  = TableST (TableIO a b)
 deriving Eq

tableST :: ST s (TableST s a b)
tableST = unsafeIOtoST (TableST `fmap` tableIO)

findST :: TableST s a b -> Ref a -> ST s (Maybe b)
findST (TableST tab) r = unsafeIOtoST (findIO tab r)

extendST :: TableST s a b -> Ref a -> b -> ST s ()
extendST (TableST tab) r b = unsafeIOtoST (extendIO tab r b)

-----------------------------------------------------------------
-- Memo

memoRef :: (Ref a -> b) -> (Ref a -> b)
memoRef f = unsafePerformIO . memoRefIO (return . f)

memoRefIO :: (Ref a -> IO b) -> (Ref a -> IO b)
memoRefIO f = unsafePerformIO $
  do tab <- tableIO
     let f' r = do mb <- findIO tab r
                   case mb of
                     Just b  -> do return b
                     Nothing -> fixIO $ \b ->
                                  do extendIO tab r b
                                     f r
     return f'

memoRefST :: (Ref a -> ST s b) -> (Ref a -> ST s b)
memoRefST f = unsafePerformST $
  do tab <- tableST
     let f' r = do mb <- findST tab r
                   case mb of
                     Just b  -> do return b
                     Nothing -> fixST $ \b ->
                                  do extendST tab r b
                                     f r
     return f'

-----------------------------------------------------------------
-- Dyn

data Dyn
  = Dyn

toDyn :: a -> Dyn
toDyn = unsafeCoerce

fromDyn :: Dyn -> a
fromDyn = unsafeCoerce

-----------------------------------------------------------------
-- the end.



