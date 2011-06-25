module Property
  ( Gen
  , generate
  , ChoiceWithSig
  , Fresh(..)
  , CoFresh(..)
  , double
  , triple
  , list
  , listOf
  , results
  , sequential
  , forAll
  , Property(..)
  , Checkable(..)
  , ShowModel(..)
  , Model
  , properties
  )
 where

import Signal
import Generic

import Monad
  ( liftM2
  , liftM3
  , liftM4
  , liftM5
  )

import List
  ( intersperse
  , transpose
  )

import Data.IORef
import System.IO.Unsafe

----------------------------------------------------------------
-- Gen Monad

newtype Gen a
  = Gen (Tree Int -> a)

instance Functor Gen where
  fmap f (Gen m) = Gen (\t -> f (m t))

instance Monad Gen where
  return a =
    Gen (\t -> a)

  Gen m >>= k =
    Gen (\(Fork _ t1 t2) -> let a = m t1 ; Gen m2 = k a in m2 t2)

generate :: Gen a -> IO a
generate (Gen m) =
  do t <- newTree
     return (m t)

-- Gen Tree

data Tree a
  = Fork a (Tree a) (Tree a)

newTree :: IO (Tree Int)
newTree =
  do var <- newIORef 0
     tree var
 where
  tree var = unsafeInterleaveIO $
    do n  <- new var
       t1 <- tree var
       t2 <- tree var
       return (Fork n t1 t2)

  new var = unsafeInterleaveIO $
    do n <- readIORef var
       let n' = n+1
       n' `seq` writeIORef var n'
       return n

----------------------------------------------------------------
-- Fresh

class Fresh a where
  fresh :: Gen a

instance Fresh () where
  fresh = return ()

instance ConstructiveSig a => Fresh (Signal a) where
  fresh = Gen (\(Fork n _ _) -> varSig ("i" ++ show n))

instance (Fresh a, Fresh b) => Fresh (a,b) where
  fresh = liftM2 (,) fresh fresh

instance (Fresh a, Fresh b, Fresh c) => Fresh (a,b,c) where
  fresh = liftM3 (,,) fresh fresh fresh

instance (Fresh a, Fresh b, Fresh c, Fresh d) => Fresh (a,b,c,d) where
  fresh = liftM4 (,,,) fresh fresh fresh fresh

instance (Fresh a, Fresh b, Fresh c, Fresh d, Fresh e) => Fresh (a,b,c,d,e) where
  fresh = liftM5 (,,,,) fresh fresh fresh fresh fresh

instance (Fresh a, Fresh b, Fresh c, Fresh d, Fresh e, Fresh f) => Fresh (a,b,c,d,e,f) where
  fresh = liftM6 (,,,,,) fresh fresh fresh fresh fresh fresh

instance (Fresh a, Fresh b, Fresh c, Fresh d, Fresh e, Fresh f, Fresh g) => Fresh (a,b,c,d,e,f,g) where
  fresh = liftM7 (,,,,,,) fresh fresh fresh fresh fresh fresh fresh

-- CoFresh

class ChoiceWithSig a where
  ifThenElseWithSig :: Choice b => Signal a -> (b, b) -> b

class CoFresh a where
  cofresh :: (Choice b, Fresh b) => Gen b -> Gen (a -> b)

instance ChoiceWithSig Bool where
  ifThenElseWithSig = ifThenElse

instance CoFresh () where
  cofresh gen =
    do b <- gen
       return (\_ -> b)

instance ChoiceWithSig a => CoFresh (Signal a) where
  cofresh gen =
    do b1 <- gen
       b2 <- gen
       return (\a -> ifThenElseWithSig a (b1, b2))

instance (CoFresh a, CoFresh b) => CoFresh (a, b) where
  cofresh gen =
    do f <- cofresh (cofresh gen)
       return (\(a, b) -> f a b)

instance (CoFresh a, CoFresh b, CoFresh c) => CoFresh (a, b, c) where
  cofresh gen =
    do f <- cofresh (cofresh (cofresh gen))
       return (\(a, b, c) -> f a b c)

instance (CoFresh a, CoFresh b, CoFresh c, CoFresh d) => CoFresh (a, b, c, d) where
  cofresh gen =
    do f <- cofresh (cofresh (cofresh (cofresh gen)))
       return (\(a, b, c, d) -> f a b c d)

instance (CoFresh a, CoFresh b, CoFresh c, CoFresh d, CoFresh e) => CoFresh (a, b, c, d, e) where
  cofresh gen =
    do f <- cofresh (cofresh (cofresh (cofresh (cofresh gen))))
       return (\(a, b, c, d, e) -> f a b c d e)

instance (CoFresh a, CoFresh b, CoFresh c, CoFresh d, CoFresh e, CoFresh f) => CoFresh (a, b, c, d, e, f) where
  cofresh gen =
    do f <- cofresh (cofresh (cofresh (cofresh (cofresh (cofresh gen)))))
       return (\(a, b, c, d, e, g) -> f a b c d e g)

instance (CoFresh a, CoFresh b, CoFresh c, CoFresh d, CoFresh e, CoFresh f, CoFresh g) => CoFresh (a, b, c, d, e, f, g) where
  cofresh gen =
    do f <- cofresh (cofresh (cofresh (cofresh (cofresh (cofresh (cofresh gen))))))
       return (\(a, b, c, d, e, g, h) -> f a b c d e g h)

instance (CoFresh a, Choice b, Fresh b) => Fresh (a -> b) where
  fresh = cofresh fresh

instance CoFresh a => CoFresh [a] where
  cofresh gen =
    do fs <- sequence [ cofreshList i gen | i <- [0..] ]
       return (\as -> (fs !! length as) as)

instance (Finite a, CoFresh b) => CoFresh (a -> b) where
  cofresh gen =
    do f <- cofresh gen
       return (\g -> f (map g domain))

cofreshList :: (CoFresh a, Choice b, Fresh b) => Int -> Gen b -> Gen ([a] -> b)
cofreshList 0 gen =
  do x <- gen
     return (\[] -> x)

cofreshList k gen =
  do f <- cofreshList (k-1) (cofresh gen)
     return (\(a:as) -> f as a)

----------------------------------------------------------------
-- some combinators

double :: Gen a -> Gen (a,a)
double gen = liftM2 (,) gen gen

triple :: Gen a -> Gen (a,a,a)
triple gen = liftM3 (,,) gen gen gen

list :: Fresh a => Int -> Gen [a]
list n = sequence [ fresh | i <- [1..n] ]

listOf :: Int -> Gen a -> Gen [a]
listOf n gen = sequence [ gen | i <- [1..n] ]

----------------------------------------------------------------
-- combinators for circuits

results :: Int -> Gen (a -> b) -> Gen (a -> [b])
results n gen =
  do fs <- sequence [ gen | i <- [1..n] ]
     return (\a -> map ($ a) fs)

sequential :: (CoFresh a, Fresh b, Choice b) => Int -> Gen (a -> b)
sequential n =
  do fb     <- fresh
     fstate <- results n fresh
     let circ inp = (fb inp, fstate inp)
     return (loop circ)
 where
  loop circ a = b
    where
      (b, state) = circ (a, delay (zeroList n) (state :: [Signal Bool]))

----------------------------------------------------------------
-- checkable

newtype Property
  = P (Gen ([Signal Bool], Model -> [[String]]))

class Checkable a where
  property :: a -> Property

instance Checkable Property where
  property p = p

instance Checkable Bool where
  property b = property (bool b)

instance Checkable a => Checkable (Signal a) where
  property (Signal s) = P (return ([Signal s], \_ -> []))

instance Checkable a => Checkable [a] where
  property as = P $
    do sms <- sequence [ gen | (P gen) <- map property as ]
       return (concat (map fst sms), \model -> concat (map (($ model) . snd) sms))

instance Checkable a => Checkable (Gen a) where
  property m = P (do a <- m ; let P m' = property a in m')

instance (Fresh a, ShowModel a, Checkable b) => Checkable (a -> b) where
  property f = forAll fresh f

----------------------------------------------------------------
-- quantify

type Model
  = [(String, [String])]

name :: Signal a -> String
name (Signal s) =
  case unsymbol s of
    VarBool v -> v
    VarInt v  -> v
    _         -> error "no name"

class ShowModel a where
  showModel :: Model -> a -> [String]

instance ShowModel (Signal a) where
  showModel model sig =
    case lookup (name sig) model of
      Just xs -> xs
      Nothing -> []

instance ShowModel () where
  showModel model () =
    ["()"]

instance (ShowModel a, ShowModel b) => ShowModel (a, b) where
  showModel model (a, b) =
    zipWith' (\x y -> "(" ++ x ++ "," ++ y ++ ")")
      (showModel model a) (showModel model b)

instance (ShowModel a, ShowModel b, ShowModel c) => ShowModel (a, b, c) where
  showModel model (a, b, c) =
    zipWith3' (\x y z -> "(" ++ x ++ "," ++ y ++ "," ++ z ++ ")")
      (showModel model a) (showModel model b) (showModel model c)

instance (ShowModel a, ShowModel b, ShowModel c, ShowModel d) => ShowModel (a, b, c, d) where
  showModel model (a, b, c, d) =
    zipWith4' (\x y z v -> "(" ++ x ++ "," ++ y ++ "," ++ z ++ "," ++ v ++ ")")
      (showModel model a) (showModel model b) (showModel model c) (showModel model d)

instance (ShowModel a, ShowModel b, ShowModel c, ShowModel d, ShowModel e) => ShowModel (a, b, c, d, e) where
  showModel model (a, b, c, d, e) =
    zipWith5' (\x y z v w -> "(" ++ x ++ "," ++ y ++ "," ++ z ++ "," ++ v ++ "," ++ w ++ ")")
      (showModel model a) (showModel model b) (showModel model c) (showModel model d) (showModel model e)

instance (ShowModel a, ShowModel b, ShowModel c, ShowModel d, ShowModel e, ShowModel f) => ShowModel (a, b, c, d, e, f) where
  showModel model (a, b, c, d, e, f) =
    zipWith6' (\x y z v w p -> "(" ++ x ++ "," ++ y ++ "," ++ z ++ "," ++ v ++ "," ++ w ++ "," ++ p ++ ")")
      (showModel model a) (showModel model b) (showModel model c) (showModel model d) (showModel model e) (showModel model f)

instance (ShowModel a, ShowModel b, ShowModel c, ShowModel d, ShowModel e, ShowModel f, ShowModel g) => ShowModel (a, b, c, d, e, f, g) where
  showModel model (a, b, c, d, e, f, g) =
    zipWith7' (\x y z v w p q -> "(" ++ x ++ "," ++ y ++ "," ++ z ++ "," ++ v ++ "," ++ w ++ "," ++ p ++ "," ++ q ++ ")")
      (showModel model a) (showModel model b) (showModel model c) (showModel model d) (showModel model e) (showModel model f) (showModel model g)

instance ShowModel a => ShowModel [a] where
  showModel model as =
    map (\xs -> "[" ++ concat (intersperse "," xs)
                    ++ "]")
      (transpose (map (showModel model) as))

instance ShowModel (a -> b) where
  showModel model sig = []

zipWith' f []     []     = []
zipWith' f []     ys     = zipWith' f ["?"] ys
zipWith' f xs     []     = zipWith' f xs ["?"]
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

zipWith3' f []     []     []     = []
zipWith3' f []     ys     zs     = zipWith3' f ["?"] ys zs
zipWith3' f xs     []     zs     = zipWith3' f xs ["?"] zs
zipWith3' f xs     ys     []     = zipWith3' f xs ys ["?"]
zipWith3' f (x:xs) (y:ys) (z:zs) = f x y z : zipWith3' f xs ys zs

zipWith4' f []     []     []     []     = []
zipWith4' f []     ys     zs     vs     = zipWith4' f ["?"] ys zs vs
zipWith4' f xs     []     zs     vs     = zipWith4' f xs ["?"] zs vs
zipWith4' f xs     ys     []     vs     = zipWith4' f xs ys ["?"] vs
zipWith4' f xs     ys     zs     []     = zipWith4' f xs ys zs ["?"]
zipWith4' f (x:xs) (y:ys) (z:zs) (v:vs) = f x y z v : zipWith4' f xs ys zs vs

zipWith5' f []     []     []     []     []     = []
zipWith5' f []     ys     zs     vs     ws     = zipWith5' f ["?"] ys zs vs ws
zipWith5' f xs     []     zs     vs     ws     = zipWith5' f xs ["?"] zs vs ws
zipWith5' f xs     ys     []     vs     ws     = zipWith5' f xs ys ["?"] vs ws
zipWith5' f xs     ys     zs     []     ws     = zipWith5' f xs ys zs ["?"] ws
zipWith5' f xs     ys     zs     vs     []     = zipWith5' f xs ys zs vs ["?"]
zipWith5' f (x:xs) (y:ys) (z:zs) (v:vs) (w:ws) = f x y z v w : zipWith5' f xs ys zs vs ws

zipWith6' f []     []     []     []     []     []     = []
zipWith6' f []     ys     zs     vs     ws     ps     = zipWith6' f ["?"] ys zs vs ws ps
zipWith6' f xs     []     zs     vs     ws     ps     = zipWith6' f xs ["?"] zs vs ws ps
zipWith6' f xs     ys     []     vs     ws     ps     = zipWith6' f xs ys ["?"] vs ws ps
zipWith6' f xs     ys     zs     []     ws     ps     = zipWith6' f xs ys zs ["?"] ws ps
zipWith6' f xs     ys     zs     vs     []     ps     = zipWith6' f xs ys zs vs ["?"] ps
zipWith6' f xs     ys     zs     vs     ws     []     = zipWith6' f xs ys zs vs ws ["?"]
zipWith6' f (x:xs) (y:ys) (z:zs) (v:vs) (w:ws) (p:ps) = f x y z v w p : zipWith6' f xs ys zs vs ws ps

zipWith7' f []     []     []     []     []     []     []     = []
zipWith7' f []     ys     zs     vs     ws     ps     qs     = zipWith7' f ["?"] ys zs vs ws ps qs
zipWith7' f xs     []     zs     vs     ws     ps     qs     = zipWith7' f xs ["?"] zs vs ws ps qs
zipWith7' f xs     ys     []     vs     ws     ps     qs     = zipWith7' f xs ys ["?"] vs ws ps qs
zipWith7' f xs     ys     zs     []     ws     ps     qs     = zipWith7' f xs ys zs ["?"] ws ps qs
zipWith7' f xs     ys     zs     vs     []     ps     qs     = zipWith7' f xs ys zs vs ["?"] ps qs
zipWith7' f xs     ys     zs     vs     ws     []     qs     = zipWith7' f xs ys zs vs ws ["?"] qs
zipWith7' f xs     ys     zs     vs     ws     ps     []     = zipWith7' f xs ys zs vs ws ps ["?"]
zipWith7' f (x:xs) (y:ys) (z:zs) (v:vs) (w:ws) (p:ps) (q:qs) = f x y z v w p q : zipWith7' f xs ys zs vs ws ps qs

forAll :: (ShowModel a, Checkable b) => Gen a -> (a -> b) -> Property
forAll gen body = P $
  do a <- gen
     let P m = property (body a)
     (sigs, mod) <- m
     return (sigs, \model -> showModel model a : mod model)

----------------------------------------------------------------
-- evaluate

properties :: Checkable a => a -> IO ([Signal Bool], Model -> [[String]])
properties a = generate gen
 where
  (P gen) = property a

----------------------------------------------------------------
-- bweeuh

liftM6 f g1 g2 g3 g4 g5 g6 =
  do a1 <- g1
     a2 <- g2
     a3 <- g3
     a4 <- g4
     a5 <- g5
     a6 <- g6
     return (f a1 a2 a3 a4 a5 a6)

liftM7 f g1 g2 g3 g4 g5 g6 g7 =
  do a1 <- g1
     a2 <- g2
     a3 <- g3
     a4 <- g4
     a5 <- g5
     a6 <- g6
     a7 <- g7
     return (f a1 a2 a3 a4 a5 a6 a7)

----------------------------------------------------------------
-- the end.

