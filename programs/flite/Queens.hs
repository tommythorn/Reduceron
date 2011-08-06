{

and False a = False;
and True a = a;

map f Nil = Nil;
map f (Cons x xs) = Cons (f x) (map f xs);

append Nil ys = ys;
append (Cons x xs) ys = Cons x (append xs ys);

concatMap f Nil = Nil;
concatMap f (Cons x xs) = append (f x) (concatMap f xs);

length xs = lengthAcc 0 xs;

lengthAcc acc Nil = acc;
lengthAcc acc (Cons x xs) = lengthAcc ((+) 1 acc) xs;

nsoln nq = length (gen nq nq);

gen nq n =
  case (==) n 0 of {
    True -> Cons Nil Nil;
    False -> concatMap (gen1 nq) (gen nq ((-) n 1));
  };

gen1 nq b = concatMap (gen2 b) (toOne nq);

gen2 b q = case safe q 1 b of {
             True -> Cons (Cons q b) Nil;
             False -> Nil;
           };

safe x d Nil = True;
safe x d (Cons q l) =
  and ((/=) x q) (
  and ((/=) x ((+) q d)) (
  and ((/=) x ((-) q d)) (
  safe x ((+) d 1) l)));       

toOne n = case (==) n 1 of {
            True -> Cons 1 Nil;
            False -> Cons n (toOne ((-) n 1));
          };

main = nsoln 11;

}
