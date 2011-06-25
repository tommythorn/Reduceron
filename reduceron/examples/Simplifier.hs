module Simplifier where

-- propositions in clausal form
type Clause = ([Int] , [Int])

-- general propositional formulae
data Prop =
  Sym Int |
  Neg Prop |
  Dis Prop Prop |
  Con Prop Prop

main :: Int
main = res 20

eqv a b = Con (Dis (Neg a) b) (Dis (Neg b) a)

res n = total (map clausify xs)
 where xs = take n (repeat (eqv (eqv a (eqv a a))
                                (eqv (eqv a (eqv a a))
                                     (eqv a (eqv a a)))))
       a = Sym 0

undef = undef

-- deriving a clause from a disjunction of literals
clauses :: [Prop] -> [Clause]
clauses = map (clause ([],[]))
  where
  clause ca    (Dis p q)     = clause (clause ca p) q
  clause (c,a) (Sym s)       = (ins s c , a)
  clause (c,a) (Neg p)       = case p of
                                 Sym s -> (c , ins s a)
                                 _ -> undef
  clause _ _                 = undef

-- testing whether a list contains an item
contains :: Eq a => [a] -> a -> Bool
contains []     y = False
contains (x:xs) y = x==y || contains xs y  

-- shifting disjunction within conjunction, resulting in CNF 
disin :: Prop -> Prop
disin (Con p q) = Con (disin p) (disin q)
disin (Dis p q) = din (disin p) (disin q)
disin p = p

-- din x y yields CNF of Dis x y assuming x and y are CNF
din :: Prop -> Prop -> Prop
din (Con p q) r = Con (din p r) (din q r)
din p (Con q r) = Con (din p q) (din p r)
din p q = Dis p q

-- inserting an item into an ordered list without duplicates
--ins :: Ord a => a -> [a] -> [a]
ins :: Int -> [Int] -> [Int]
ins x []     = [x]
ins x (y:ys) = if x == y
               then y:ys
               else if x <= y then x:y:ys else y : ins x ys

-- set intersection
inter :: Eq a => [a] -> [a] -> [a]
inter xs ys = filt (contains xs) ys

-- shifting negation innermost
negin :: Prop -> Prop
negin (Neg (Con p q)) = Dis (negin (Neg p)) (negin (Neg q))
negin (Neg (Dis p q)) = Con (negin (Neg p)) (negin (Neg q))
negin (Neg (Neg p))   = negin p
negin (Dis p q)       = Dis (negin p) (negin q)
negin (Con p q)       = Con (negin p) (negin q)
negin p               = p

-- throwing away tautologies from a list of clauses
nonTaut :: [Clause] -> [Clause]
nonTaut = filt (not . taut)
  where
  taut (c,a) = inter c a /= []

-- simplifying propositions to minimal clausal form
clausify =
    display .
    uniq . nonTaut . clauses .
    split . disin . negin

-- list of conjuncts from a CNF proposition
split :: Prop -> [Prop]
split = split' []
        where
        split' a (Con p q) = split' (split' a p) q
        split' a p = p : a

-- set union
union :: Eq a => [a] -> [a] -> [a]
union xs ys = xs ++ filt (not . contains xs) ys

-- set of items in a list
uniq :: Eq a => [a] -> [a]
uniq = foldr (union . (:[])) []

filt p [] = []
filt p (x:xs) = if p x then x : filt p xs else filt p xs

------------------------------------------------------------

display :: [Clause] -> Int
display = total . map (\(a, b) -> total a + total b)

total :: [Int] -> Int
total [] = 0
total (x:xs) = x + total xs
