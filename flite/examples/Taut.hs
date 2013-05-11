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

vars (Const b)         = [] ;
vars (Var x)           = Cons x [] ;
vars (Not p)           = vars p ;
vars (And p q)         = append (vars p) (vars q) ;
vars (Implies p q)     = append (vars p) (vars q) ;

bools n = case (==) n 0 of {
          True  -> Cons [] [] ;
          False -> let { bss = bools ((-) n 1) } in
                   append (map (Cons False) bss)
                          (map (Cons True)  bss) ;
          } ;

neq x y = (/=) x y;

rmdups []         = [] ;
rmdups (Cons x xs) = Cons x (rmdups (filter (neq x) xs)) ;

substs p = let { vs = rmdups (vars p) } in
           map (zip vs) (bools (length vs)) ;

isTaut p = and (map (flip eval p) (substs p)) ;

flip f y x = f x y ;

length []         = 0 ;
length (Cons x xs) = (+) 1 (length xs) ;

append []         ys = ys ;
append (Cons x xs) ys = Cons x (append xs ys) ;

map f []         = [] ;
map f (Cons x xs) = Cons (f x) (map f xs) ;

and []         = True ;
and (Cons b bs) = case b of {
                  True  -> and bs ;
                  False -> False ;
                  } ;

filter p []         = [] ;
filter p (Cons x xs) = case p x of {
                       True  -> Cons x (filter p xs) ;
                       False -> filter p xs ;
                       } ;

null []         = True ;
null (Cons x xs) = False;

zip []         ys          = [] ;
zip (Cons x xs) []         = [] ;
zip (Cons x xs) (Cons y ys) = Cons (Pair x y) (zip xs ys) ;

foldr1 f (Cons x xs) = case null xs of {
                       True  -> x ;
                       False -> f x (foldr1 f xs) ;
                       } ;

imp v = Implies (Var 'p') (Var v) ;

names = "abcdefghijklmn" ;

testProp = Implies
             (foldr1 And (map imp names))
             (Implies (Var 'p') (foldr1 And (map Var names))) ;

main = case isTaut testProp of {
       True  -> emit 'T' 1 ;
       False -> emit 'F' 0 ;
       } ;

}
