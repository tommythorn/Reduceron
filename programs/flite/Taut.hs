{

find key (Cons (Pair k v) t) = case (==) key k of {
                               True  -> v ;
                               False -> find key t ;
                               } ;

eval s (Const b)       = b ;
eval s (Var x)         = find x s ;
eval s (Not p)         = case eval s p of {
                         True  -> False ;
                         False -> True ;
                         } ;
eval s (And p q)       = case eval s p of {
                         True  -> eval s q ;
                         False -> False ;
                         } ;
eval s (Implies p q)   = case eval s p of {
                         True  -> eval s q ;
                         False -> True ;
                         } ;

vars (Const b)         = Nil ;
vars (Var x)           = Cons x Nil ;
vars (Not p)           = vars p ;
vars (And p q)         = append (vars p) (vars q) ;
vars (Implies p q)     = append (vars p) (vars q) ;

bools n = case (==) n 0 of {
          True  -> Cons Nil Nil ;
          False -> let { m = (-) n 1 } in
                   append (map (Cons False) (bools m))
                          (map (Cons True) (bools m)) ;
          } ;

neq x y = (/=) x y;

rmdups Nil         = Nil ;
rmdups (Cons x xs) = Cons x (rmdups (filter (neq x) xs)) ;

substs p = let { vs = rmdups (vars p) } in
           map (zip vs) (bools (length vs)) ;

isTaut p = and (map (flip eval p) (substs p)) ;

flip f y x = f x y ;

length xs = lengthAcc 0 xs;

lengthAcc acc Nil = acc;
lengthAcc acc (Cons x xs) = lengthAcc ((+) acc 1) xs;

append Nil         ys = ys ;
append (Cons x xs) ys = Cons x (append xs ys) ;

map f Nil         = Nil ;
map f (Cons x xs) = Cons (f x) (map f xs) ;

and Nil         = True ;
and (Cons b bs) = case b of {
                  True  -> and bs ;
                  False -> False ;
                  } ;

filter p Nil         = Nil ;
filter p (Cons x xs) = case p x of {
                       True  -> Cons x (filter p xs) ;
                       False -> filter p xs ;
                       } ;

null Nil         = True ;
null (Cons x xs) = False;

zip Nil         ys          = Nil ;
zip (Cons x xs) Nil         = Nil ; 
zip (Cons x xs) (Cons y ys) = Cons (Pair x y) (zip xs ys) ;

foldr1 f (Cons x xs) = case null xs of {
                       True  -> x ;
                       False -> f x (foldr1 f xs) ;
                       } ;

imp v = Implies (Var 'q') (Var v) ;

names = "abcdefghijklmnop" ;

testProp = Implies
             (foldr1 And (map imp names))
             (Implies (Var 'q') (foldr1 And (map Var names))) ;

main = case isTaut testProp of {
       True  -> 1 ;
       False -> 0 ;
       } ;

}

