{

map f Nil = Nil;
map f (Cons x xs) = Cons (f x) (map f xs);

clauses ps = map (clause (Pair Nil Nil)) ps;

clause (Pair c a) (Dis p q)     = clause (clause (Pair c a) p) q;
clause (Pair c a) (Sym s)       = Pair (ins s c) a;
clause (Pair c a) (Neg (Sym s)) = Pair c (ins s a);

or False x = x;
or True x = True;

contains eq Nil y = False;
contains eq (Cons x xs) y = or (eq x y) (contains eq xs y);

disin (Sym s) = Sym s;
disin (Neg p) = Neg p;
disin (Con p q) = Con (disin p) (disin q);
disin (Dis p q) = din (disin p) (disin q);

din (Con p q) r = Con (din p r) (din q r);
din (Dis p q) r = din2 (Dis p q) r;
din (Neg p) r = din2 (Neg p) r;
din (Sym s) r = din2 (Sym s) r;

din2 p (Con q r) = Con (din p q) (din p r);
din2 p (Dis q r) = Dis p (Dis q r);
din2 p (Neg q) = Dis p (Neg q);
din2 p (Sym s) = Dis p (Sym s);

ins x Nil = Cons x Nil;
ins x (Cons y ys) =
  case (==) x y of {
    True -> Cons y ys;
    False -> case (<=) x y of {
               True -> Cons x (Cons y ys);
               False -> Cons y (ins x ys);
             };
  };

filter p Nil = Nil;
filter p (Cons x xs) = case p x of {
                         True -> Cons x (filter p xs);
                         False -> filter p xs;
                       };

inter eq xs ys = filter (contains eq xs) ys;

negin (Neg (Con p q)) = Dis (negin (Neg p)) (negin (Neg q));
negin (Neg (Dis p q)) = Con (negin (Neg p)) (negin (Neg q));
negin (Neg (Neg p))   = negin p;
negin (Neg (Sym s))   = Neg (Sym s);
negin (Dis p q)       = Dis (negin p) (negin q);
negin (Con p q)       = Con (negin p) (negin q);
negin (Sym s)         = Sym s;

nonTaut cs = filter notTaut cs;

and False x = False;
and True x = x;

eqList f Nil Nil = True;
eqList f Nil (Cons y ys) = False;
eqList f (Cons x xs) Nil = False;
eqList f (Cons x xs) (Cons y ys) = and (f x y) (eqList f xs ys);

eq a b = (==) a b;

eqClause (Pair a b) (Pair c d) = and (eqList eq a c) (eqList eq b d);

null Nil = True;
null (Cons x xs) = False;

notTaut (Pair c a) = null (inter eq c a);

clausify p = uniq
           ( nonTaut
           ( clauses
           ( split
           ( disin
           ( negin p )))));

split p = spl Nil p;

spl a (Con p q) = spl (spl a p) q;
spl a (Dis p q) = Cons (Dis p q) a;
spl a (Neg p) = Cons (Neg p) a;
spl a (Sym s) = Cons (Sym s) a;

append Nil ys = ys;
append (Cons x xs) ys = Cons x (append xs ys);

comp f g x = f (g x);

not False = True;
not True = False;

union eq xs ys = append xs (filter (comp not (contains eq xs)) ys);

singleton x = Cons x Nil;

foldr f z Nil = z;
foldr f z (Cons x xs) = f x (foldr f z xs);

uniq xs = foldr (comp (union eqClause) singleton) Nil xs;

display Nil = 0;
display (Cons c cs) = (+) (emitClause c) (display cs);

emitClause (Pair c a) = (+) (sum c) (sum a);

sum xs = sumAcc 0 xs;

sumAcc acc Nil = acc;
sumAcc acc (Cons x xs) = sumAcc ((+) acc x) xs;

eqv a b = Con (Dis (Neg a) b) (Dis (Neg b) a);

replicate n a = case (==) n 0 of {
                  True -> Nil;
                  False -> Cons a (replicate ((-) n 1) a);
                };

main = let { p = eqv (eqv a (eqv a a))
                   (eqv (eqv a (eqv a a))
                     (eqv a (eqv a a)))
           ; a = Sym 0
           } in display (clausify (foldr Con a (replicate 80 p)));

}
