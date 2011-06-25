module Sem where

-- Taken from Semantics with Applications (Nielson and Nielson)

-- Abstract syntax

type Var = Int

data Aexp = N Int | V Var | Add Aexp Aexp | Sub Aexp Aexp

data  Bexp = TRUE | FALSE | Eq Aexp Aexp | Le Aexp Aexp
           | Neg Bexp | And Bexp Bexp

data  Stm = Ass Var Aexp | Skip | Comp Stm Stm
          | If Bexp Stm Stm | While Bexp Stm 

-- Evaluation of expressions

type Z = Int
type T = Bool

type State = [(Var, Z)]

undef :: a
undef = undef

value :: State -> Var -> (Int -> a) -> a
value [] v k = undef
value ((x,y):s) v k = if x == v then k y else value s v k

update :: State -> Var -> Int -> (State -> a) -> a
update [] v i k = k []
update ((x,y):s) v i k =
  if   x == v
  then update s v i (\s -> k ((v,i):s))
  else update s v i (\s -> k ((x,y):s))

int :: Int -> (Int -> a) -> a
int n k = if n == 0 then k 0 else k n
 
bool :: Bool -> (Bool -> a) -> a
bool False k = k False
bool True k = k True

a_val :: Aexp -> State -> (Z -> a) -> a
a_val (N n) s k = k n
a_val (V x) s k = value s x k
a_val (Add a1 a2) s k = a_val a1 s (\x -> a_val a2 s (\y -> int (x+y) k))
a_val (Sub a1 a2) s k = a_val a1 s (\x -> a_val a2 s (\y -> int (x-y) k))

b_val :: Bexp -> State -> (T -> a) -> a
b_val TRUE s k = k True
b_val FALSE s k = k False
b_val (Eq a1 a2) s k = a_val a1 s (\x -> a_val a2 s (\y -> bool (x == y) k))
b_val (Le a1 a2) s k = a_val a1 s (\x -> a_val a2 s (\y -> bool (x <= y) k))
b_val (Neg b) s k = b_val b s (\x -> bool (not x) k)
b_val (And a1 a2) s k = b_val a1 s (\x -> b_val a2 s (\y -> bool (x && y) k))

-- Semantics

data Config = Inter Stm State | Final State

sos_stm :: Stm -> State -> Config
sos_stm (Ass x a) s = a_val a s (\i -> update s x i Final)
sos_stm Skip s = Final s
sos_stm (Comp ss1 ss2) s =
  case sos_stm ss1 s of
    Inter ss1' s' -> Inter (Comp ss1' ss2) s'
    Final s' -> Inter ss2 s'
sos_stm (If b ss1 ss2) s =
  b_val b s (\c -> if c then Inter ss1 s else Inter ss2 s)
sos_stm (While b ss) s =
  Inter (If b (Comp ss (While b ss)) Skip) s

run (Inter ss s) = run (sos_stm ss s)
run (Final s) = s

s_sos ss s = run (Inter ss s)

-- Main

main :: Int
main = value (s_sos ndivs s_init) 5 id
  where
    divide = While (Le (V 1) (V 0))
               (Comp (Ass 0 (Sub (V 0) (V 1)))
                     (Ass 2 (Add (V 2) (N 1))))

    callDivide = Ass 0 (V 3)
          `Comp` Ass 1 (V 4)
          `Comp` divide

    ndivs = Ass 4 (V 3)
     `Comp` While (Neg (Eq (V 4) (N 0))) (
                     callDivide
              `Comp` If (Eq (V 0) (N 0)) (Ass 5 (Add (V 5) (N 1)))  Skip
              `Comp` Ass 4 (Sub (V 4) (N 1))
            )

    s_init :: State
    s_init = [ (0, 0)
             , (1, 0)
             , (2, 0)
             , (3, 10000)
             , (4, 0)
             , (5, 0)
             ]
