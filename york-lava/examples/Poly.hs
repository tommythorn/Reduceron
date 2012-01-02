module Poly where
import Lava
import Recipe
import Mult
import Stack
import Monad

-- A small language for expressing polynomials.

data Expr = X | N Integer | Expr :+: Expr | Expr :*: Expr

-- Semantics of the language.

eval :: Expr -> Integer -> Integer
eval X n = n
eval (N i) n = i
eval (e1 :+: e2) n = eval e1 n + eval e2 n
eval (e1 :*: e2) n = eval e1 n * eval e2 n

-- Compilation to a sequence of machine instructions.

data INSTR = LIT Integer | DUP | REV | ADD | MUL | HALT deriving Show

compile :: Expr -> [INSTR]
compile e = comp e ++ [HALT]

comp X = []
comp (N n) = [LIT n]
comp (e1 :+: e2) = DUP:comp e2 ++ [REV] ++ comp e1 ++ [ADD]
comp (e1 :*: e2) = DUP:comp e2 ++ [REV] ++ comp e1 ++ [MUL]

-- Semantics of the machine.

exec :: [INSTR] -> [Integer] -> Integer
exec (LIT m : c) (n:s)   = exec c (m:s)
exec (DUP   : c) (m:s)   = exec c (m:m:s)
exec (REV   : c) (m:n:s) = exec c (n:m:s)
exec (ADD   : c) (m:n:s) = exec c (m+n:s)
exec (MUL   : c) (m:n:s) = exec c (m*n:s)
exec (HALT  : c) (n:s)   = n

-- Bit-level encoding of instructions.

enc :: INSTR -> Integer
enc (LIT n) = 1 + 2*fromIntegral n
enc DUP     = 2
enc REV     = 4
enc ADD     = 8
enc MUL     = 16
enc HALT    = 32

type DataWidth = N8
type Value = Word DataWidth
type Instr = Word DataWidth

isLIT, isDUP, isREV, isADD, isMUL, isHALT :: Instr -> Bit
isLIT i  = i `vat` n0
isDUP i  = inv (isLIT i) <&> (i `vat` n1)
isREV i  = inv (isLIT i) <&> (i `vat` n2)
isADD i  = inv (isLIT i) <&> (i `vat` n3)
isMUL i  = inv (isLIT i) <&> (i `vat` n4)
isHALT i = inv (isLIT i) <&> (i `vat` n5)

getLIT :: Instr -> Value
getLIT i = vtail i <+ low

-- Hardware implementation of the machine.

data Poly = Poly
  { code  :: Stack DataWidth N10
  , stack :: Stack DataWidth N10
  , mult  :: Mult DataWidth
  , rtop  :: Reg DataWidth
  }

newPoly :: [Integer] -> Integer -> New Poly
newPoly code x =
       return Poly
  `ap` newStack code
  `ap` newStack []
  `ap` newMult
  `ap` newRegInit (fromIntegral x)

poly :: Poly -> Recipe
poly s =
  let instr = s!code!top in
    Seq [ Tick
        , While (instr!isHALT!inv) $
            Seq [ isLIT instr |> s!rtop <== getLIT instr
                , isDUP instr |> s!stack!push (s!rtop!val)
                , isREV instr |>
                    Seq [ s!rtop <== s!stack!top
                        , s!stack!pop
                        , s!stack!push (s!rtop!val)
                        ]
                , isADD instr |>
                    Seq [ s!rtop <== s!rtop!val + s!stack!top
                        , s!stack!pop
                        ]
                , isMUL instr |>
                    Seq [ s!mult!multiply (s!rtop!val) (s!stack!top)
                        , s!rtop <== s!mult!result!val
                        , s!stack!pop
                        ]
                , s!code!pop
                , Tick
                ]
        ]

expr :: Expr
expr = (X :+: X) :+: (N 2 :*: X) :+: X

simPoly :: Expr -> Integer -> Value
simPoly e x = simRecipe (newPoly code x) poly (val . rtop)
  where code = reverse $ map enc $ compile e

prop_poly :: Expr -> Integer -> Bool
prop_poly e x = eval e x == wordToInt (simPoly e x)

synPoly :: Expr -> Integer -> IO ()
synPoly e x =
  do let code = reverse $ map enc $ compile e
     let (s, done) = recipe (newPoly code x) poly (delay high low)
     writeVerilog "Poly"
               (s!rtop!val, done)
               (nameWord "result", name "done")

cPoly :: Expr -> Integer -> IO ()
cPoly e x =
  do let code = reverse $ map enc $ compile e
     let (s, done) = recipe (newPoly code x) poly (delay high low)
     writeC "Poly"
               (s!rtop!val, done)
               (nameWord "result", name "done")
