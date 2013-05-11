{

tail (Cons x xs) = xs;

one p [] = [];
one p (Cons x xs) = case p x of { True -> Cons x [] ; False -> one p xs };

map f [] = [];
map f (Cons x xs) = Cons (f x) (map f xs);

append [] ys = ys;
append (Cons x xs) ys = Cons x (append xs ys);

concatMap f [] = [];
concatMap f (Cons x xs) = append (f x) (concatMap f xs);

length [] = 0;
length (Cons x xs) = (+) 1 (length xs);

replicate n x =
  case (==) n 0 of {
    True -> [];
    False -> Cons x (replicate ((-) n 1) x);
  };

l = 0;
r = 1;
d = 2;

eq x y = (==) x y;

left  xs = map (one (eq l)) (tail xs);
right xs = Cons [] (map (one (eq r)) xs);
down  xs = map (one (eq d)) xs;

merge [] ys = [];
merge (Cons x xs) [] = Cons x xs;
merge (Cons x xs) (Cons y ys) = Cons (append x y) (merge xs ys);

next mask = merge (merge (down mask) (left mask)) (right mask);

fill [] = [];
fill (Cons x xs) = append (lrd x xs) (map (Cons x) (fill xs));

lrd [] ys = Cons (Cons (Cons l (Cons r (Cons d []))) ys) [];
lrd (Cons x xs) ys = [];

solve n mask =
  case (==) n 0 of {
    True -> Cons [] [];
    False -> concatMap (sol ((-) n 1)) (fill mask);
  };

sol n row = map (Cons row) (solve n (next row));

nqueens n = length (solve n (replicate n []));

main = emitInt (nqueens 10) 0;

}
