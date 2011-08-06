module Queens2;

import Flite;

tail (Cons x xs) = xs;

onep p Nil = Nil;
onep p (Cons x xs) = case p x of { True -> Cons x Nil ; False -> onep p xs };

map f Nil = Nil;
map f (Cons x xs) = Cons (f x) (map f xs);

append Nil ys = ys;
append (Cons x xs) ys = Cons x (append xs ys);

concatMap f Nil = Nil;
concatMap f (Cons x xs) = append (f x) (concatMap f xs);

len Nil = 0;
len (Cons x xs) = (+) 1 (len xs);

replicate n x =
  case (==) n 0 of {
    True -> Nil;
    False -> Cons x (replicate ((-) n 1) x);
  };

l = 0;
r = 1;
d = 2;

eq x y = (==) x y;

left  xs = map (onep (eq l)) (tail xs);
right xs = Cons Nil (map (onep (eq r)) xs);
down  xs = map (onep (eq d)) xs;

merge Nil ys = Nil;
merge (Cons x xs) Nil = Cons x xs;
merge (Cons x xs) (Cons y ys) = Cons (append x y) (merge xs ys);

next mask = merge (merge (down mask) (left mask)) (right mask);

fill Nil = Nil;
fill (Cons x xs) = append (lrd x xs) (map (Cons x) (fill xs));

lrd Nil ys = Cons (Cons (Cons l (Cons r (Cons d Nil))) ys) Nil;
lrd (Cons x xs) ys = Nil;

solve n mask =
  case (==) n 0 of {
    True -> Cons Nil Nil;
    False -> concatMap (sol ((-) n 1)) (fill mask);
  };

sol n row = map (Cons row) (solve n (next row));

nqueens n = len (solve n (replicate n Nil));

Start = nqueens 11;
