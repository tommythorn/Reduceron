module Signal where

import Ref
import Sequent
import Error

import List
  ( transpose
  )

----------------------------------------------------------------
-- Signal, Symbol, S

newtype Signal a
  = Signal Symbol

newtype Symbol
  = Symbol (Ref (S Symbol))

data S s
  = Bool      Bool
  | Inv       s
  | And       [s]
  | Or        [s]
  | Xor       [s]
  | VarBool   String
  | DelayBool s s

  | Xorcy     s s
  | Mux       String s s s
  | Fde       Bool s s s
 
  | Int      Int
  | Neg      s
  | Div      s s
  | Mod      s s
  | Plus     [s]
  | Times    [s]
  | Gte      s s
  | Equal    [s]
  | If       s s s
  | VarInt   String
  | DelayInt s s

  | Multi    Int String String [s]
  | MultiSel Int s

symbol :: S Symbol -> Symbol
symbol = Symbol . ref

unsymbol :: Symbol -> S Symbol
unsymbol (Symbol r) = deref r

instance Eq (Signal a) where
  Signal (Symbol r1) == Signal (Symbol r2) = r1 == r2

----------------------------------------------------------------
-- operations

-- on bits

bool :: Bool -> Signal Bool
bool b = lift0 (Bool b)

low, high :: Signal Bool
low  = bool False
high = bool True

inv :: Signal Bool -> Signal Bool
inv = lift1 Inv

andl, orl, xorl :: [Signal Bool] -> Signal Bool
andl = liftl And
orl  = liftl Or
xorl = liftl Xor

xorcy :: (Signal Bool, Signal Bool) -> Signal Bool
xorcy (a, b) = lift2 Xorcy b a

muxcy :: (Signal Bool, (Signal Bool, Signal Bool)) -> Signal Bool
muxcy (a, (b, c)) = lift3 (Mux "muxcy") a c b

muxf5 :: (Signal Bool, (Signal Bool, Signal Bool)) -> Signal Bool
muxf5 (a, (b, c)) = lift3 (Mux "muxf5") a c b

muxf6 :: (Signal Bool, (Signal Bool, Signal Bool)) -> Signal Bool
muxf6 (a, (b, c)) = lift3 (Mux "muxf6") a c b

equalBool :: Signal Bool -> Signal Bool -> Signal Bool
equalBool x y = inv (xorl [x,y])

ifBool :: Signal Bool -> (Signal Bool, Signal Bool) -> Signal Bool
ifBool c (x,y) = orl[andl[c,x],andl[inv c,y]]

delayBool :: Signal Bool -> Signal Bool -> Signal Bool
delayBool = lift2 DelayBool

varBool :: String -> Signal Bool
varBool s = lift0 (VarBool s)

fde :: Bool -> Signal Bool -> Signal Bool -> Signal Bool
fde b en d = lift3 (Fde b) (if b then high else low) en d

-- define primitives with multiple outputs
-- only synthesis supported at the moment

data Multi = M

multi :: [Signal Bool] -> Int -> String -> String -> Signal Multi
multi inps n name opts = liftl (Multi n name opts) inps

multiSel :: Int -> Signal Multi -> Signal Bool
multiSel n s = lift1 (MultiSel n) s

{-

Here is how you might define a halfAdd primitive.

halfAdd (a, b) = (multiSel 1 res, multiSel 2 res)
  where
    res = multi [a, b] 2 "HA" ""

Then you would edit Vhdl.hs to support generation of "HA" instance.

-}

-- on ints

int :: Int -> Signal Int
int n = lift0 (Int n)

neg :: Signal Int -> Signal Int
neg = lift1 Neg

divide, modulo :: Signal Int -> Signal Int -> Signal Int
divide = lift2 Div
modulo = lift2 Mod

plusl, timesl :: [Signal Int] -> Signal Int
plusl  = liftl Plus
timesl = liftl Times

equall :: [Signal Int] -> Signal Bool
equall = liftl Equal

gteInt :: Signal Int -> Signal Int -> Signal Bool
gteInt = lift2 Gte

equalInt :: Signal Int -> Signal Int -> Signal Bool
equalInt x y = equall [x,y]

ifInt :: Signal Bool -> (Signal Int, Signal Int) -> Signal a
ifInt c (x,y) = lift3 If c x y

delayInt :: Signal Int -> Signal Int -> Signal Int
delayInt = lift2 DelayInt

varInt :: String -> Signal Int
varInt s = lift0 (VarInt s)

-- liftings

lift0 :: S Symbol -> Signal a
lift0 oper = Signal (symbol oper)

lift1 :: (Symbol -> S Symbol) -> Signal a -> Signal b
lift1 oper (Signal a) = Signal (symbol (oper a))

lift2 :: (Symbol -> Symbol -> S Symbol) -> Signal a -> Signal b -> Signal c
lift2 oper (Signal a) (Signal b) = Signal (symbol (oper a b))

lift3 :: (Symbol -> Symbol -> Symbol -> S Symbol)
      -> Signal a -> Signal b -> Signal c -> Signal d
lift3 oper (Signal a) (Signal b) (Signal c) = Signal (symbol (oper a b c))

liftl :: ([Symbol] -> S Symbol) -> [Signal a] -> Signal c
liftl oper sigas = Signal (symbol (oper (map (\(Signal a) -> a) sigas)))

----------------------------------------------------------------
-- evaluate

eval :: S (S a) -> S a
eval s =
  case s of
    Bool b       -> Bool b
    Inv (Bool b) -> Bool (not b)
    And xs       -> Bool . all bval $ xs
    Or xs        -> Bool . any bval $ xs
    Xor xs       -> Bool . (1 ==) . length . filter bval $ xs

    Xorcy (Bool a) (Bool b)          -> Bool (a /= b)
    Mux _ (Bool a) (Bool b) (Bool c) -> Bool (if a then b else c)
    Fde _ _ _ _  -> wrong Error.DelayEval

    Int n                 -> Int n
    Neg (Int n)           -> Int (-n)
    Div (Int n1) (Int n2) -> Int (n1 `div` n2)
    Mod (Int n1) (Int n2) -> Int (n1 `mod` n2)
    Plus xs               -> Int  . sum     . map nval $ xs
    Times xs              -> Int  . product . map nval $ xs
    Gte (Int n1) (Int n2) -> Bool (n1 >= n2)
    Equal xs              -> Bool . equal   . map nval $ xs
    If (Bool c) x y       -> if c then x else y

    DelayBool s s' -> wrong Error.DelayEval
    DelayInt  s s' -> wrong Error.DelayEval
    VarBool   s    -> wrong Error.VarEval
    VarInt    s    -> wrong Error.VarEval
 where
  bval (Bool b) = b
  nval (Int n)  = n
  
  equal (x:y:xs) = x == y && equal (y:xs)
  equal _        = True

evalLazy :: S (Maybe (S a)) -> Maybe (S a)
evalLazy s =
  case s of
    -- lazy
    And xs
      | any (`bval` False) xs        -> bans False
      
    Or xs
      | any (`bval` True) xs         -> bans True
      
    Xor xs
      | number (`bval` True) xs >= 2 -> bans False
    
    -- strict
    _ -> eval `fmap` sequent s
    
 where
  bans = Just . Bool
 
  bval (Just (Bool b)) b' = b == b'
  bval _               _  = False
  
  number p = length . filter p
  
arguments :: S a -> [a]
arguments s =
  case s of
    Bool b     -> []
    Inv s      -> [s]
    And xs     -> xs
    Or xs      -> xs
    Xor xs     -> xs

    Xorcy a b      -> [a, b]
    Mux _ a b c    -> [a, b, c]
    Fde _ a b c -> [a, b, c]

    Int n      -> []
    Neg s      -> [s]
    Div s1 s2  -> [s1,s2]
    Mod s1 s2  -> [s1,s2]
    Plus xs    -> xs
    Times xs   -> xs
    Gte x y    -> [x,y]
    Equal xs   -> xs
    If x y z   -> [x,y,z]

    DelayBool s s' -> [s,s']
    DelayInt  s s' -> [s,s']
    VarBool s      -> []
    VarInt  s      -> []

    Multi _ _ _ xs -> xs
    MultiSel n x -> [x]

zips :: S [a] -> [S a]
zips s =
  case s of
    Bool b     -> [Bool b]
    Inv s      -> map Inv s
    And xs     -> map And (transpose xs)
    Or xs      -> map Or  (transpose xs)
    Xor xs     -> map Xor (transpose xs)

    Xorcy a b      -> zipWith Xorcy a b
    Mux s a b c    -> zipWith3 (Mux s) a b c
    Fde init a b c -> zipWith3 (Fde init) a b c

    Int n      -> [Int n]
    Neg s      -> map Neg s
    Div s1 s2  -> zipWith Div s1 s2
    Mod s1 s2  -> zipWith Mod s1 s2
    Plus xs    -> map Plus  (transpose xs)
    Times xs   -> map Times (transpose xs)
    Gte x y    -> zipWith Gte x y
    Equal xs   -> map Equal (transpose xs)
    If x y z   -> zipWith3 If x y z

    DelayBool s s' -> zipWith DelayBool s s'
    DelayInt  s s' -> zipWith DelayInt s s'
    VarBool s      -> [VarBool s]
    VarInt  s      -> [VarInt  s]

    Multi n name opts xs -> map (Multi n name opts) (transpose xs)
    MultiSel n x -> map (MultiSel n) x

----------------------------------------------------------------
-- properties of S

instance Functor S where
  fmap f s =
    case s of
      Bool b    -> Bool b
      Inv x     -> Inv (f x)
      And xs    -> And (map f xs)
      Or  xs    -> Or  (map f xs)
      Xor xs    -> Xor (map f xs)

      Xorcy a b      -> Xorcy (f a) (f b)
      Mux s a b c    -> (Mux s) (f a) (f b) (f c)
      Fde init a b c -> (Fde init) (f a) (f b) (f c)

      Int   n   -> Int n
      Neg   x   -> Neg   (f x)
      Div   x y -> Div   (f x) (f y)
      Mod   x y -> Mod   (f x) (f y)
      Plus  xs  -> Plus  (map f xs)
      Times xs  -> Times (map f xs)
      Gte   x y -> Gte (f x) (f y)
      Equal xs  -> Equal (map f xs)
      If x y z  -> If (f x) (f y) (f z)

      DelayBool x y -> DelayBool (f x) (f y)
      DelayInt  x y -> DelayInt  (f x) (f y)
      VarBool   v   -> VarBool v
      VarInt    v   -> VarInt  v

      Multi n name opts xs -> Multi n name opts (map f xs)
      MultiSel n x -> MultiSel n (f x)


instance Sequent S where
  sequent s = 
    case s of
      Bool b    -> lift0 (Bool b)
      Inv x     -> lift1 Inv x
      And xs    -> liftl And xs
      Or  xs    -> liftl Or  xs
      Xor xs    -> liftl Xor xs

      Xorcy a b      -> lift2 Xorcy a b
      Mux s a b c    -> lift3 (Mux s) a b c
      Fde init a b c -> lift3 (Fde init) a b c

      Int   n   -> lift0 (Int n)
      Neg   x   -> lift1 Neg   x
      Div   x y -> lift2 Div   x y
      Mod   x y -> lift2 Mod   x y
      Plus  xs  -> liftl Plus  xs
      Times xs  -> liftl Times xs
      Gte   x y -> lift2 Gte   x y
      Equal xs  -> liftl Equal xs
      If x y z  -> lift3 If x y z

      DelayBool x y -> lift2 DelayBool x y
      DelayInt  x y -> lift2 DelayInt x y
      VarBool  v    -> lift0 (VarBool v)
      VarInt   v    -> lift0 (VarInt v)

      Multi n name opts xs -> liftl (Multi n name opts) xs
      MultiSel n x -> lift1 (MultiSel n) x
   where
    lift0 op =
      do return op

    lift1 op x =
      do x' <- x
         return (op x')

    lift2 op x y =
      do x' <- x
         y' <- y
         return (op x' y')

    lift3 op x y z =
      do x' <- x
         y' <- y
         z' <- z
         return (op x' y' z')

    liftl op xs =
      do xs' <- sequence xs
         return (op xs')

instance Show (Signal a) where
  showsPrec n (Signal s) =
    showsPrec n s

instance Show Symbol where
  showsPrec n sym =
    showsPrec n (unsymbol sym)

instance Show a => Show (S a) where
  showsPrec n s =
    case s of
      Bool True  -> showString "high"
      Bool False -> showString "low"

      Inv x      -> showString "inv"  . showList [x]
      And xs     -> showString "andl" . showList xs
      Or  xs     -> showString "orl"  . showList xs
      Xor xs     -> showString "xorl" . showList xs

      Xorcy a b      -> showString "xorcy" . showList [a, b]
      Mux s a b c    -> showString s . showList [a, b, c]
      Fde init a b c -> showString "Fde" . showList [a, b, c]

      Int   i    -> showsPrec n i
      Neg   x    -> showString "-" . showsPrec n x
      Div   x y  -> showString "idiv" . showList [x,y]
      Mod   x y  -> showString "imod" . showList [x,y]
      Plus  xs   -> showString "plusl" . showList xs
      Times xs   -> showString "timesl" . showList xs
      Gte   x y  -> showString "gte" . showList [x,y]
      Equal xs   -> showString "equall" . showList xs
      If x y z   -> showString "ifThenElse" . showList [x,y,z]

      DelayBool x y -> showString "delay" . showList [x,y]
      DelayInt  x y -> showString "delay" . showList [x,y]
      
      VarBool s     -> showString s
      VarInt  s     -> showString s

      Multi n name opts xs -> showString name . showList xs
      MultiSel n x -> showString "sel" . showList [x]

      _             -> showString "<<symbol>>"

----------------------------------------------------------------
-- the end.

