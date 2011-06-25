{

fst (Pair x y) = x;

snd (Pair x y) = y;

rng = map snd;

img lds l = fromJust (lookup l lds);

fromJust (Just x) = x;

lookup a Nil = Nothing;
lookup a (Cons (Pair b c) rest) =
  case (==) a b of {
    False -> lookup a rest;
    True  -> Just c;
  };

sum Nil = 0;
sum (Cons x xs) = (+) x (sum xs);

del x Nil = Nil;
del x (Cons y ys) = 
  case (==) x y of {
    False -> Cons y (del x ys);
    True  -> ys;
  };

member x Nil = False;
member x (Cons y ys) = 
  case (==) x y of {
    False -> member x ys;
    True -> True;
  };

flip f x y = f y x;

diff = foldl (flip del);

foldl f a Nil = a;
foldl f a (Cons x xs) = foldl f (f a x) xs;

repeat x = Cons x (repeat x);

zip Nil ys = Nil;
zip (Cons x xs) Nil = Nil;
zip (Cons x xs) (Cons y ys) = Cons (Pair x y) (zip xs ys);

map f Nil = Nil;
map f (Cons x xs) = Cons (f x) (map f xs);

bindings l ds lds =
  case lookup l lds of {
    Nothing -> map (flip Cons lds) (zip (repeat l) (diff ds (rng lds)));
    Just d -> 
      case member d ds of {
        True -> Cons lds Nil;
        False -> Nil;
      };
  };

append Nil ys = ys;
append (Cons x xs) ys = Cons x (append xs ys);

ofAll f Nil = Nil;
ofAll f (Cons x xs) = append (f x) (ofAll f xs);

ifNull Nil t e = t;
ifNull (Cons x xs) t e = e;

head (Cons x xs) = x;
tail (Cons x xs) = xs;

solutions Nil yys Nil clds =
  case (==) (fst clds) 0 of {
    False -> Nil;
    True -> Cons (snd clds) Nil;
  };
solutions Nil yys (Cons z Nil) clds = 
  case (==) (fst clds) 1 of {
    False -> Nil;
    True -> bindings z (Cons 1 Nil) (snd clds);
  };
solutions (Cons x xs) yys (Cons z zs) clds =
  ofAll (solns (fst clds) x (head yys) z (solutions xs (tail yys) zs))
    (ofAll (bindings (head yys) (fromTo (ifNull (tail yys) 1 0) 9))
           (bindings x (fromTo (ifNull xs 1 0) 9) (snd clds)));

curry f x y = f (Pair x y);

solns c x y z f s =
  let { qr = divMod10 ((+) ((+) (img s x) (img s y)) c) } in
    ofAll (curry f (fst qr))
          (bindings z (Cons (snd qr) Nil) s);

divMod10 n =
  case (<=) n 9 of {
    True -> Pair 0 n;
    False -> 
      case divMod10 ((-) n 10) of {
        Pair q r -> Pair ((+) q 1) r;
      };
  };

fromTo n m = Cons n (case (==) n m of {
                      True -> Nil;
                      False -> fromTo ((+) n 1) m;
                    });

isSingleton Nil = False;
isSingleton (Cons x Nil) = True;
isSingleton (Cons x (Cons y ys)) = False;

and False x = False;
and True x = x;

valid x y z = 
  and ((==) (length x) (length y))
      (and ((==) (length x) (length z))
           (isSingleton (solutions x y z (Pair 0 Nil))));

sumMap f xs = sumMapAcc f xs 0;

sumMapAcc f Nil acc = acc;
sumMapAcc f (Cons x xs) acc = sumMapAcc f xs ((+) (f x) acc);

count xs ys zs = sumMap (fx ys zs) xs;

fx ys zs x = sumMap (fy x zs) ys;

fy x zs y = sumMap (fz x y) zs;

fz x y z = case valid x y z of {
             True -> 1;
             False -> 0;
           };

length xs = lengthAcc 0 xs;

lengthAcc acc Nil = acc;
lengthAcc acc (Cons x xs) = lengthAcc ((+) acc 1) xs;

main = let { words = Cons "ANANAB" (
                     Cons "ELPPA" (
                     Cons "YRREHC" (
                     Cons "HCAEP" (
                     Cons "TOCIRPA" (
                     Cons "EVILO" (
                     Cons "NOMEL" (
                     Cons "AVAUG" (
                     Cons "ODACAVA" (
                     Cons "AYAPAP" (
                     Cons "IHCTIL" (
                     Cons "NOLEM" Nil))))))))))) }
       in count words words words;

}
