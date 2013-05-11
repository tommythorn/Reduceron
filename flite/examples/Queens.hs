{

and False a = False;
and True a = a;

map f [] = [];
map f (Cons x xs) = Cons (f x) (map f xs);

append [] ys = ys;
append (Cons x xs) ys = Cons x (append xs ys);

concatMap f [] = [];
concatMap f (Cons x xs) = append (f x) (concatMap f xs);

length [] = 0;
length (Cons x xs) = (+) 1 (length xs);

nsoln nq = length (gen nq nq);

gen nq n =
  case (==) n 0 of {
    True -> Cons [] [];
    False -> concatMap (gen1 nq) (gen nq ((-) n 1));
  };

gen1 nq b = concatMap (gen2 b) (toOne nq);

gen2 b q = case safe q 1 b of {
             True -> Cons (Cons q b) [];
             False -> [];
           };

safe x d [] = True;
safe x d (Cons q l) =
  and ((/=) x q) (
  and ((/=) x ((+) q d)) (
  and ((/=) x ((-) q d)) (
  safe x ((+) d 1) l)));

toOne n = case (==) n 1 of {
            True -> Cons 1 [];
            False -> Cons n (toOne ((-) n 1));
          };

main = emitInt (nsoln 10) 0;

}
