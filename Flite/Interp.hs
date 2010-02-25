module Flite.Interp (interp, frontend, Val (..)) where

import Flite.Syntax hiding (Lam)
import Data.Array
import Flite.InterpFrontend
import Flite.Inline

infixl :@

data Val =
    Error String
  | C Id Int Int [Val]
  | F Id
  | V Id
  | N Int
  | Lut (Array Int Val)
  | Val :@ Val
  | Lambda Id Val
  | Lam (Val -> Val)
  | Emit String Val

instance Show Val where
  show (Lam f) = "lambda!"
  show (C n _ _ vs) = "(" ++ unwords (n:map show vs) ++ ")"
  show (N i) = show i
  show (Error s) = "** Interpreter error: " ++ s
  show (Emit s k) = s ++ show k
  show _ = "*Thunk*"

lut :: [Val] -> Val
lut vs = Lut (listArray (0, length vs) vs)

app :: [Val] -> Val
app xs = foldl1 (:@) xs

val :: Exp -> Val
val (App e xs) = app (val e : map val xs)
val (Var v) = V v
val (Alts as _) = lut (map F as)
val (Ctr s arity i) = C s arity i []
val (Fun f) = F f
val (Int n) = N n
val Bottom = Error "_|_"
val (Let bs e) = elimLet vs (map val es) (val e)
  where (vs, es) = unzip bs

compile :: Prog -> [(Id, Val)]
compile p = [(f, comp $ lambdify args $ val e) | Func f args e <- p]
  where lambdify args e = foldr (\(Var v) -> Lambda v) e args

comp :: Val -> Val
comp (Lambda v x) = abstr v (comp x)
comp (e1 :@ e2) = comp e1 :@ comp e2
comp e = e

abstr :: Id -> Val -> Val
abstr v (e1 :@ e2) = opt (F "S" :@ abstr v e1 :@ abstr v e2)
abstr v (V w)
  | v == w = F "I"
  | otherwise = F "K" :@ V w
abstr v e = F "K" :@ e

opt :: Val -> Val
opt (F "S" :@ (F "K":@p) :@ (F "K" :@ q)) = F "K" :@ (p :@ q)
opt (F "S" :@ (F "K":@p) :@ F "I") = p
opt (F "S" :@ (F "K":@p) :@ (F "B" :@ q :@ r)) = F "B*" :@ p :@ q :@ r
opt (F "S" :@ (F "K":@p) :@ q) = F "B" :@ p :@ q
opt (F "S" :@ (F "B":@p:@q) :@ (F "K" :@ r)) = F "C'" :@ p :@ q :@ r
opt (F "S" :@ p :@ (F "K":@q)) = F "C" :@ p :@ q
opt (F "S" :@ (F "B":@p:@q) :@ r) = F "S'" :@ p :@ q :@ r
opt e = e

interp :: InlineFlag -> Prog -> Val
interp i p = case lookup "main" bs of
             Nothing -> Error "No 'main' function defined"
             Just e -> e
  where bs = prims ++ map (\(f, e) -> (f, link bs e)) (compile p')
        p' = frontend i p

link :: [(Id, Val)] -> Val -> Val
link bs (f :@ a) = link bs f @@ link bs a
link bs (Lut a) = Lut (fmap (link bs) a)
link bs (F f) = case lookup f bs of
                  Nothing -> Error ("Function '" ++ f ++ "' not defined")
                  Just e -> e
-- link bs Error = error "_|_"
link bs (V v) = Error ("Unknown identifier '" ++ v ++ "'")
link bs e = e

infixl 0 @@
(@@) :: Val -> Val -> Val
(Lam f) @@ x = f x
(C s 0 i args) @@ (Lut alts) = run (alts ! i) args @@ Lut alts
(C s arity i args) @@ x = C s (arity-1) i (x:args)
(Error s) @@ x = Error s
(Emit s k) @@ x = Emit s (k @@ x)
x @@ y = Error $ "Run-time type error : " ++ show x ++ " @@ " ++ show y

run :: Val -> [Val] -> Val
run e [] = e
run e (x:xs) = run e xs @@ x

prims :: [(Id, Val)]
prims = let (-->) = (,) in
 [ "I" --> (Lam $ \x -> x)
 , "K" --> (Lam $ \x -> Lam $ \y ->  x)
 , "S" --> (Lam $ \f -> Lam $ \g -> Lam $ \x -> f@@x@@(g@@x))
 , "B" --> (Lam $ \f -> Lam $ \g -> Lam $ \x -> f@@(g@@x))
 , "C" --> (Lam $ \f -> Lam $ \g -> Lam $ \x -> f@@x@@g)
 , "S'" --> (Lam $ \c -> Lam $ \f -> Lam $ \g -> Lam $ \x -> c@@(f@@x)@@(g@@x))
 , "B*" --> (Lam $ \c -> Lam $ \f -> Lam $ \g -> Lam $ \x -> c@@(f@@(g@@x)))
 , "C'" --> (Lam $ \c -> Lam $ \f -> Lam $ \g -> Lam $ \x -> c@@(f@@x)@@g)
 , "Y" --> (Lam $ \f -> fix f)
 , "(+)" --> arith2 (+)
 , "(-)" --> arith2 (-)
 , "(==)" --> logical2 (==)
 , "(/=)" --> logical2 (/=)
 , "(<=)" --> logical2 (<=)
 , "emit" --> (Lam $ \a -> Lam $ \k -> forceInt a $ \a' -> Emit [toEnum a'] k)
 , "emitInt" --> (Lam $ \a -> Lam $ \k -> forceInt a $ \a' -> Emit (show a') k)
 ]

fix :: Val -> Val
fix f = let a = f @@ a in a

forceInt :: Val -> (Int -> Val) -> Val
forceInt (N i) 		f = f i
forceInt (Emit s k)	f = Emit s (forceInt k f)
forceInt a			f = Error $ "Integer expected. " ++ show a

arith2 :: (Int -> Int -> Int) -> Val
arith2 op = Lam $ \a -> Lam $ \b ->
	forceInt a $ \a' ->
	forceInt b $ \b' ->
		N (op a' b')

logical2 :: (Int -> Int -> Bool) -> Val
logical2 op = Lam $ \a -> Lam $ \b ->
	forceInt a $ \a' ->
	forceInt b $ \b' ->
		if (op a' b') then true else false

false :: Val
false = C "False" 0 0 []

true :: Val
true = C "True" 0 1 []

-- Unfortunatly, handling recursive lets is a bit tricky.
-- Here's SPJ's solution, more-or-less.

elimLet :: [Id] -> [Val] -> Val -> Val
elimLet vs es e = (Lambda "#" $ sub e) :@ (F "Y" :@ Lambda "#" t)
  where
    t = app (tuple (length vs):map sub es)
    sels = [V "#" :@ select (length vs) i | i <- [0..]]
    sub e = subst (zip vs sels) e

tuple :: Int -> Val
tuple n = foldr Lambda (app $ map (V . var) (n:[0..n-1])) (map var [0..n])
  where var i = 'v':show i

select :: Int -> Int -> Val
select n i = foldr Lambda (V (var i)) (map var [0..n-1])
  where var i = 'v':show i

subst :: [(Id, Val)] -> Val -> Val
subst s (e1 :@ e2) = subst s e1 :@ subst s e2
subst s (V v) = case lookup v s of
                  Nothing -> V v
                  Just x -> x
subst s e = e
