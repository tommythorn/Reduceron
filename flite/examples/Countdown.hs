{

valid Add x y  =  True ;
valid Sub x y  =  not ((<=) x y) ;
valid Mul x y  =  True ;
valid Div x y  =  (==) (mod x y) 0 ;

apply Add x y  =  (+) x y ;
apply Sub x y  =  (-) x y ;
apply Mul x y  =  mul x y ;
apply Div x y  =  div x y ;

subs []         =  Cons [] [] ;
subs (Cons x xs) =  let { yss = subs xs } in append yss (map (Cons x) yss) ;

interleave x []         =  Cons (Cons x []) [] ;
interleave x (Cons y ys) =  Cons (Cons x (Cons y ys))
                                 (map (Cons y) (interleave x ys)) ;

perms []         =  Cons [] [] ;
perms (Cons x xs) =  concatMap (interleave x) (perms xs) ;

choices xs  =  concatMap perms (subs xs) ;

ops  =  Cons Add (Cons Sub (Cons Mul (Cons Div []))) ;

split (Cons x xs)  =  case null xs of {
                      True  -> [] ;
                      False -> Cons (Pair (Cons x []) xs)
                                    (map (cross (Pair (Cons x) id)) (split xs)) ;
                      } ;

results []         =  [] ;
results (Cons n ns) =  case null ns of {
                       True  -> Cons (Pair (Val n) n) [] ;
                       False -> concatMap combinedResults (split (Cons n ns)) ;
                       } ;

combinedResults (Pair ls rs)  = concatProdWith combine (results ls) (results rs) ;

concatProdWith f []         ys = [] ;
concatProdWith f (Cons x xs) ys = append (concatMap (f x) ys) (concatProdWith f xs ys) ;

combine (Pair l x) (Pair r y) =  concatMap (combi l x r y) ops ;

combi l x r y o = case valid o x y of {
                  True  -> Cons (Pair (App o l r) (apply o x y)) [] ;
                  False -> [] ;
                  } ;

solutions ns n = concatMap (solns n) (choices ns) ;

solns n ns = let { ems = results ns } in preImage n (results ns) ;

preImage n []                   = [] ;
preImage n (Cons (Pair e m) ems) = case (==) m n of {
                                   True  -> Cons e (preImage n ems) ;
                                   False -> preImage n ems ;
                                   } ;

not True   =  False ;
not False  =  True ;

div x y = case divMod x y of { Pair d m -> d ; } ;

mod x y = case divMod x y of { Pair d m -> m ; } ;

divMod x y = let { y2 = (+) y y } in
             case (<=) y2 x of {
             True  -> case divMod x y2 of {
                      Pair d2 m2 -> case (<=) y m2 of {
                                    True  -> Pair ((+) 1 ((+) d2 d2)) ((-) m2 y) ;
                                    False -> Pair ((+) d2 d2) m2 ;
                                    } ;
                      } ;
             False -> case (<=) y x of {
                      True  -> Pair 1 ((-) x y) ;
                      False -> Pair 0 x ;
                      } ;
             } ;

mul x n = case (==) n 1 of {
          True  -> x ;
          False -> case divMod n 2 of {
                   Pair d m -> (+) (mul ((+) x x) d)
                                   (case (==) m 0 of {True -> 0; False -> x;}) ;
                   } ;
          } ;

cross (Pair f g) (Pair x y) = Pair (f x) (g y) ;

id x = x ;

null []         = True ;
null (Cons x xs) = False ;

length []         = 0 ;
length (Cons x xs) = (+) 1 (length xs) ;

append []         ys = ys ;
append (Cons x xs) ys = Cons x (append xs ys) ;

map f []         = [] ;
map f (Cons x xs) = Cons (f x) (map f xs) ;

concatMap f []         = [] ;
concatMap f (Cons x xs) = append (f x) (concatMap f xs) ;

givens = Cons 1 (Cons 3 (Cons 7 (Cons 10 (Cons 25 (Cons 50 []))))) ;

target = 765 ;

main = emitInt (length (solutions givens target)) 0 ;

}
