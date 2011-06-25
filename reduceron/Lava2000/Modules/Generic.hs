module Generic where

import Signal
import Sequent
import Error

import LavaRandom
  ( Rnd
  , split
  , next
  )

import List
  ( transpose
  )

----------------------------------------------------------------
-- Struct

data Struct a
  = Compound [Struct a]
  | Object a
 deriving (Eq, Show)

flatten :: Struct a -> [a]
flatten (Object a)    = [a]
flatten (Compound ss) = concatMap flatten ss

transStruct :: Struct [a] -> [Struct a]
transStruct (Object as)   = map Object as
transStruct (Compound ss) =
  map Compound . transpose . map transStruct $ ss

-- structural operations

instance Functor Struct where
  fmap f (Object a)    = Object (f a)
  fmap f (Compound xs) = Compound (map (fmap f) xs)

instance Sequent Struct where
  sequent (Object m) =
    do a <- m
       return (Object a)
  
  sequent (Compound xs) =
    do as <- sequence [ sequent x | x <- xs ]
       return (Compound as)

----------------------------------------------------------------
-- Generic datatypes

class Generic a where
  struct    :: a -> Struct Symbol
  construct :: Struct Symbol -> a

instance Generic Symbol where
  struct    s          = Object s
  construct (Object s) = s

instance Generic (Signal a) where
  struct    (Signal s) = Object s
  construct (Object s) = Signal s

instance Generic () where
  struct    ()            = Compound []
  construct (Compound []) = ()

instance Generic a => Generic [a] where
  struct    xs            = Compound (map struct xs)
  construct (Compound xs) = map construct xs

instance (Generic a, Generic b) => Generic (a,b) where
  struct    (a,b)            = Compound [struct a, struct b]
  construct (Compound [a,b]) = (construct a, construct b)

instance (Generic a, Generic b, Generic c) => Generic (a,b,c) where
  struct    (a,b,c)            = Compound [struct a, struct b, struct c]
  construct (Compound [a,b,c]) = (construct a, construct b, construct c)

instance (Generic a, Generic b, Generic c, Generic d) => Generic (a,b,c,d) where
  struct    (a,b,c,d)            = Compound [struct a, struct b, struct c, struct d]
  construct (Compound [a,b,c,d]) = (construct a, construct b, construct c, construct d)

instance (Generic a, Generic b, Generic c, Generic d, Generic e) => Generic (a,b,c,d,e) where
  struct    (a,b,c,d,e)            = Compound [struct a, struct b, struct c, struct d, struct e]
  construct (Compound [a,b,c,d,e]) = (construct a, construct b, construct c, construct d, construct e)

instance (Generic a, Generic b, Generic c, Generic d, Generic e, Generic f) => Generic (a,b,c,d,e,f) where
  struct    (a,b,c,d,e,f)            = Compound [struct a, struct b, struct c, struct d, struct e, struct f]
  construct (Compound [a,b,c,d,e,f]) = (construct a, construct b, construct c, construct d, construct e, construct f)

instance (Generic a, Generic b, Generic c, Generic d, Generic e, Generic f, Generic g) => Generic (a,b,c,d,e,f,g) where
  struct    (a,b,c,d,e,f,g)            = Compound [struct a, struct b, struct c, struct d, struct e, struct f, struct g]
  construct (Compound [a,b,c,d,e,f,g]) = (construct a, construct b, construct c, construct d, construct e, construct f, construct g)

----------------------------------------------------------------
-- Ops

data Ops
  = Ops { equalSymbol :: Symbol -> Symbol -> Signal Bool
        , delaySymbol :: Symbol -> Symbol -> Symbol
        , ifSymbol    :: Signal Bool -> (Symbol, Symbol) -> Symbol
        , varSymbol   :: String -> Symbol
        , zeroSymbol  :: Symbol
        }

opsBool :: Ops
opsBool =
  Ops { equalSymbol = \x y     -> equalBool (Signal x) (Signal y)
      , delaySymbol = \x y     -> unSignal $ delayBool (Signal x) (Signal y)
      , ifSymbol    = \c (x,y) -> unSignal $ ifBool c  (Signal x,  Signal y)
      , varSymbol   = \s       -> symbol (VarBool s)
      , zeroSymbol  =             symbol (Bool False)
      }

opsInt :: Ops
opsInt =
  Ops { equalSymbol = \x y     -> equalInt (Signal x) (Signal y)
      , delaySymbol = \x y     -> unSignal $ delayInt (Signal x) (Signal y)
      , ifSymbol    = \c (x,y) -> unSignal $ ifInt c  (Signal x,  Signal y)
      , varSymbol   = \s       -> symbol (VarInt s)
      , zeroSymbol  =             symbol (Int 0)
      }

unSignal :: Signal a -> Symbol
unSignal (Signal s) = s

ops :: Symbol -> Ops
ops s =
  case unsymbol s of
    Bool b         -> opsBool
    Inv s          -> opsBool
    And xs         -> opsBool
    Or xs          -> opsBool
    Xor xs         -> opsBool

    Xorcy a b      -> opsBool
    Mux s a b c    -> opsBool
    Fde a b c d    -> opsBool

    Int n          -> opsInt
    Neg s          -> opsInt
    Div s1 s2      -> opsInt
    Mod s1 s2      -> opsInt
    Plus xs        -> opsInt
    Times xs       -> opsInt
    Gte x y        -> opsBool
    Equal xs       -> opsBool
    If x y z       -> opsInt

    DelayBool s s' -> opsBool
    DelayInt  s s' -> opsInt
    VarBool s      -> opsBool
    VarInt  s      -> opsInt

    Multi _ _ _ _  -> opsBool
    MultiSel _ _   -> opsBool

----------------------------------------------------------------
-- generic definitions

equal :: Generic a => (a, a) -> Signal Bool
equal (x, y) = eq (struct x) (struct y)
 where
  eq (Object a)    (Object b)    = equalSymbol (ops a) a b
  eq (Compound as) (Compound bs) = eqs as bs
  eq _             _             = low
  
  eqs []     []     = high
  eqs (a:as) (b:bs) = andl [eq a b, eqs as bs]
  eqs _      _      = low

delay :: Generic a => a -> a -> a
delay x y = construct (del (struct x) (struct y))
 where
  del (Object a)    ~(Object b)    = Object (delaySymbol (ops a) a b)
  del (Compound as) ~(Compound bs) = Compound (lazyZipWith del as bs)
  del _             _              = wrong Error.IncompatibleStructures

zeroify :: Generic a => a -> a
zeroify x = construct (zero (struct x))
 where
  zero (Object a)    = Object (zeroSymbol (ops a))
  zero (Compound as) = Compound [ zero a | a <- as ]

symbolize :: Generic a => String -> a -> a
symbolize s x = construct (sym s (struct x))
 where
  sym s (Object a)    = Object (varSymbol (ops a) s)
  sym s (Compound as) = Compound [ sym (s ++ "_" ++ show i) a
                                 | (a,i) <- as `zip` [0..]
                                 ]

pickSymbol :: Generic a => String -> a -> Symbol
pickSymbol s a = pick (numbers s) (struct a)
 where
  pick _      (Object a)    = a
  pick (n:ns) (Compound as) = pick ns (as !! n)
  
  numbers ('_':s) = read s1 : numbers s2
   where
    s1 = takeWhile (/= '_') s
    s2 = dropWhile (/= '_') s

----------------------------------------------------------------
-- Constructive

class ConstructiveSig a where
  zeroSig   :: Signal a
  varSig    :: String -> Signal a
  randomSig :: Rnd -> Signal a

class Generic a => Constructive a where
  zero   :: a
  var    :: String -> a
  random :: Rnd -> a

zeroList :: Constructive a => Int -> [a]
zeroList n = replicate n zero

varList :: Constructive a => Int -> String -> [a]
varList n s = [ var (s ++ "_" ++ show i) | i <- [0..(n-1)] ]

randomList :: Constructive a => Int -> Rnd -> [a]
randomList n rnd = take n [ random rnd' | rnd' <- splitRndList rnd ]

splitRndList :: Rnd -> [Rnd]
splitRndList rnd = rnd1 : splitRndList rnd2 where (rnd1, rnd2) = split rnd

valRnd :: Rnd -> Int
valRnd rnd = i where (i, _) = next rnd

-- instances

instance ConstructiveSig Bool where
  zeroSig       = low
  varSig        = varBool
  randomSig rnd = looping (take n [ bit rnd' | rnd' <- splitRndList rnd2 ])
   where
    (rnd1,rnd2) = split rnd
    n           = 30 + (valRnd rnd1 `mod` 10)
    bit rnd     = bool (even (valRnd rnd))
    looping xs  = out where out = foldr delay out xs

instance ConstructiveSig Int where
  zeroSig     = int 0
  varSig      = varInt
  randomSig rnd = looping (take n [ num rnd' | rnd' <- splitRndList rnd2 ])
   where
    (rnd1,rnd2) = split rnd
    n           = 30 + (valRnd rnd1 `mod` 10)
    num rnd     = int (20 + (valRnd rnd `mod` 20))
    looping xs  = out where out = foldr delay out xs

instance ConstructiveSig a => Constructive (Signal a) where
  zero   = zeroSig
  var    = varSig
  random = randomSig

instance Constructive () where
  zero       = ()
  var s      = ()
  random rnd = ()

instance (Constructive a, Constructive b)
      => Constructive (a, b) where
  zero       = (zero, zero)
  var s      = (var (s ++ "_1"), var (s ++ "_2"))
  random rnd = (random rnd1, random rnd2)
   where (rnd1, rnd2) = split rnd

instance (Constructive a, Constructive b, Constructive c)
      => Constructive (a, b, c) where
  zero     = (zero, zero, zero)
  var s    = (var (s ++ "_1"), var (s ++ "_2"), var (s ++ "_3"))
  random rnd = (random rnd1, random rnd2, random rnd3)
   where (rnd1: rnd2 : rnd3 : _) = splitRndList rnd

instance (Constructive a, Constructive b, Constructive c, Constructive d)
      => Constructive (a, b, c, d) where
  zero     = (zero, zero, zero, zero)
  var s    = (var (s ++ "_1"), var (s ++ "_2"), var (s ++ "_3"), var (s ++ "_4"))
  random rnd = (random rnd1, random rnd2, random rnd3, random rnd4)
   where (rnd1: rnd2 : rnd3 : rnd4 : _) = splitRndList rnd

instance (Constructive a, Constructive b, Constructive c, Constructive d, Constructive e)
      => Constructive (a, b, c, d, e) where
  zero     = (zero, zero, zero, zero, zero)
  var s    = (var (s ++ "_1"), var (s ++ "_2"), var (s ++ "_3"), var (s ++ "_4"), var (s ++ "_5"))
  random rnd = (random rnd1, random rnd2, random rnd3, random rnd4, random rnd5)
   where (rnd1: rnd2 : rnd3 : rnd4 : rnd5 : _) = splitRndList rnd

instance (Constructive a, Constructive b, Constructive c, Constructive d, Constructive e, Constructive f)
      => Constructive (a, b, c, d, e, f) where
  zero     = (zero, zero, zero, zero, zero, zero)
  var s    = (var (s ++ "_1"), var (s ++ "_2"), var (s ++ "_3"), var (s ++ "_4"), var (s ++ "_5"), var (s ++ "_6"))
  random rnd = (random rnd1, random rnd2, random rnd3, random rnd4, random rnd5, random rnd6)
   where (rnd1: rnd2 : rnd3 : rnd4 : rnd5 : rnd6 : _) = splitRndList rnd

instance (Constructive a, Constructive b, Constructive c, Constructive d, Constructive e, Constructive f, Constructive g)
      => Constructive (a, b, c, d, e, f, g) where
  zero     = (zero, zero, zero, zero, zero, zero, zero)
  var s    = (var (s ++ "_1"), var (s ++ "_2"), var (s ++ "_3"), var (s ++ "_4"), var (s ++ "_5"), var (s ++ "_6"), var (s ++ "_7"))
  random rnd = (random rnd1, random rnd2, random rnd3, random rnd4, random rnd5, random rnd6, random rnd7)
   where (rnd1: rnd2 : rnd3 : rnd4 : rnd5 : rnd6 : rnd7 : _) = splitRndList rnd

----------------------------------------------------------------
-- Finite

class ConstructiveSig a => FiniteSig a where
  domainSig :: [Signal a]

class Constructive a => Finite a where
  domain :: [a]

domainList :: Finite a => Int -> [[a]]
domainList 0 = [[]]
domainList n = [ a:as | a <- domain, as <- domainList (n-1) ]

-- instances

instance FiniteSig Bool where
  domainSig = [low, high]

instance FiniteSig a => Finite (Signal a) where
  domain = domainSig

instance Finite () where
  domain = [ () ]

instance (Finite a, Finite b)
      => Finite (a, b) where
  domain = [ (a,b) | a <- domain, b <- domain ]

instance (Finite a, Finite b, Finite c)
      => Finite (a, b, c) where
  domain = [ (a,b,c) | a <- domain, b <- domain, c <- domain ]

instance (Finite a, Finite b, Finite c, Finite d)
      => Finite (a, b, c, d) where
  domain = [ (a,b,c,d) | a <- domain, b <- domain, c <- domain, d <- domain ]

instance (Finite a, Finite b, Finite c, Finite d, Finite e)
      => Finite (a, b, c, d, e) where
  domain = [ (a,b,c,d,e) | a <- domain, b <- domain, c <- domain, d <- domain, e <- domain ]

instance (Finite a, Finite b, Finite c, Finite d, Finite e, Finite f)
      => Finite (a, b, c, d, e, f) where
  domain = [ (a,b,c,d,e,f) | a <- domain, b <- domain, c <- domain, d <- domain, e <- domain, f <- domain ]

instance (Finite a, Finite b, Finite c, Finite d, Finite e, Finite f, Finite g)
      => Finite (a, b, c, d, e, f, g) where
  domain = [ (a,b,c,d,e,f,g) | a <- domain, b <- domain, c <- domain, d <- domain, e <- domain, f <- domain, g <- domain ]

----------------------------------------------------------------
-- Choice

class Choice a where
  ifThenElse :: Signal Bool -> (a, a) -> a

-- instances

instance Choice Symbol where
  ifThenElse cond (x, y) = ifSymbol (ops x) cond (x, y)

instance Choice (Signal a) where
  ifThenElse cond (Signal x, Signal y) =
    Signal (ifThenElse cond (x, y))

instance Choice () where
  ifThenElse cond (_, _) = ()

instance Choice a => Choice [a] where
  ifThenElse cond (xs, ys) =
    strongZipWith (curry (ifThenElse cond)) xs ys

instance (Choice a, Choice b) => Choice (a,b) where
  ifThenElse cond ((x1,x2),(y1,y2)) =
    (ifThenElse cond (x1,y1), ifThenElse cond (x2,y2))

instance (Choice a, Choice b, Choice c) => Choice (a,b,c) where
  ifThenElse cond ((x1,x2,x3),(y1,y2,y3)) =
    (ifThenElse cond (x1,y1), ifThenElse cond (x2,y2), ifThenElse cond (x3,y3))

instance (Choice a, Choice b, Choice c, Choice d) => Choice (a,b,c,d) where
  ifThenElse cond ((x1,x2,x3,x4),(y1,y2,y3,y4)) =
    (ifThenElse cond (x1,y1), ifThenElse cond (x2,y2), ifThenElse cond (x3,y3), ifThenElse cond (x4,y4))

instance (Choice a, Choice b, Choice c, Choice d, Choice e) => Choice (a,b,c,d,e) where
  ifThenElse cond ((x1,x2,x3,x4,x5),(y1,y2,y3,y4,y5)) =
    (ifThenElse cond (x1,y1), ifThenElse cond (x2,y2), ifThenElse cond (x3,y3), ifThenElse cond (x4,y4), ifThenElse cond (x5,y5))

instance (Choice a, Choice b, Choice c, Choice d, Choice e, Choice f) => Choice (a,b,c,d,e,f) where
  ifThenElse cond ((x1,x2,x3,x4,x5,x6),(y1,y2,y3,y4,y5,y6)) =
    (ifThenElse cond (x1,y1), ifThenElse cond (x2,y2), ifThenElse cond (x3,y3), ifThenElse cond (x4,y4), ifThenElse cond (x5,y5),
     ifThenElse cond (x6,y6))

instance (Choice a, Choice b, Choice c, Choice d, Choice e, Choice f, Choice g) => Choice (a,b,c,d,e,f,g) where
  ifThenElse cond ((x1,x2,x3,x4,x5,x6,x7),(y1,y2,y3,y4,y5,y6,y7)) =
    (ifThenElse cond (x1,y1), ifThenElse cond (x2,y2), ifThenElse cond (x3,y3), ifThenElse cond (x4,y4), ifThenElse cond (x5,y5),
     ifThenElse cond (x6,y6), ifThenElse cond (x7,y7))

instance Choice b => Choice (a -> b) where
  ifThenElse cond (f, g) =
    \a -> ifThenElse cond (f a, g a)

mux :: Choice a => (Signal Bool, (a, a)) -> a
mux (cond, (a, b)) = ifThenElse cond (b, a)

----------------------------------------------------------------
-- helper functions

strongZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
strongZipWith f (x:xs) (y:ys) = f x y : strongZipWith f xs ys
strongZipWith f []     []     = []
strongZipWith f _      _      = wrong Error.IncompatibleStructures

lazyZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
lazyZipWith f []     _  = []
lazyZipWith f (x:xs) ys = f x (safe head ys) : lazyZipWith f xs (safe tail ys)
 where
  safe f [] = wrong Error.IncompatibleStructures
  safe f xs = f xs

----------------------------------------------------------------
-- the end.

