{

del x Nil = Nil;
del x (Cons y ys) =
  case (==) x y of { True -> ys ; False -> Cons y (del x ys) };

diff xs Nil = xs;
diff xs (Cons y ys) = diff (del y xs) ys;

head (Cons x xs) = x;
tail (Cons x xs) = xs;

length Nil = 0;
length (Cons x xs) = (+) 1 (length xs);

sum Nil = 0;
sum (Cons x xs) = (+) x (sum xs);

null Nil = True;
null (Cons x xs) = False;

single Nil = False;
single (Cons x xs) = null xs;

minimum (Cons x xs) = min x xs;

min m Nil = m;
min m (Cons x xs) = case (<=) x m of { True -> min x xs ; False -> min m xs };

break p Nil = Pair Nil Nil;
break p (Cons x xs) =
  case p x of {
    True -> Pair Nil (Cons x xs);
    False -> case break p xs of { Pair ys zs -> Pair (Cons x ys) zs };
  };

filter p Nil = Nil;
filter p (Cons x xs) =
  case p x of {
    True -> Cons x (filter p xs);
    False -> filter p xs;
  };

zipWith f Nil ys = Nil;
zipWith f (Cons x xs) Nil = Nil;
zipWith f (Cons x xs) (Cons y ys) = Cons (f x y) (zipWith f xs ys);

notElem x Nil = True;
notElem x (Cons y ys) = and ((/=) x y) (notElem x ys);

and False x = False;
and True x = x;

not False = True;
not True = False;

or False x = x;
or True x = True;

any p Nil = False;
any p (Cons x xs) = or (p x) (any p xs);

all p Nil = True;
all p (Cons x xs) = and (p x) (all p xs);

map f Nil = Nil;
map f (Cons x xs) = Cons (f x) (map f xs);

append Nil ys = ys;
append (Cons x xs) ys = Cons x (append xs ys);

concat Nil = Nil;
concat (Cons xs xss) = append xs (concat xss);

concatMap f Nil = Nil;
concatMap f (Cons x xs) = append (f x) (concatMap f xs);

take n Nil = Nil;
take n (Cons x xs) =
  case (==) n 0 of {
    True -> Nil;
    False -> Cons x (take ((-) n 1) xs);
  };

drop n Nil = Nil;
drop n (Cons x xs) =
  case (==) n 0 of {
    True -> Cons x xs;
    False -> drop ((-) n 1) xs;
  };

groupBy n xs =
  case null xs of {
    True -> Nil;
    False -> Cons (take n xs) (groupBy n (drop n xs));
  };

id x = x;

comp f g x = f (g x);

boardsize = 9;
boxsize   = 3;
cellvals  = Cons 1 (Cons 2 (Cons 3 (
            Cons 4 (Cons 5 (Cons 6 (
            Cons 7 (Cons 8 (Cons 9 Nil))))))));

blank x = (==) x 0;

nodups Nil = True;
nodups (Cons x xs) = and (notElem x xs) (nodups xs);

singleton x = Cons x Nil;

cols (Cons xs Nil) = map singleton xs;
cols (Cons xs (Cons ys yss)) = zipWith Cons xs (cols (Cons ys yss));

boxs m = map concat (concatMap cols (groupBy 3 (map (groupBy 3) m)));

choices b = map (map choose) b;

choose e = case blank e of { True -> cellvals ; False -> Cons e Nil };

fixed css = concat (filter single css);

reduce css = map (remove (fixed css)) css;

remove fs cs = case single cs of { True -> cs ; False -> diff cs fs };

prune m = pruneBy boxs (pruneBy cols (pruneBy id m));

pruneBy f m = f (map reduce (f m));

blocked cm = or (void cm) (not (safe cm));

void m = any (any null) m;

safe cm = and (all (comp nodups fixed) cm)
              (and (all (comp nodups fixed) (cols cm))
                   (all (comp nodups fixed) (boxs cm)));

best n cs = (==) (length cs) n;

expand cm =
  let { n = minchoice cm } in
    case break (any (best n)) cm of {
      Pair rows1 rows2 ->
        case break (best n) (head rows2) of {
          Pair row1 row2 -> map (exp row1 row2 rows1 rows2) (head row2);
        };
    };

exp row1 row2 rows1 rows2 c =
  append rows1 (append (Cons (append row1 (Cons (Cons c Nil)
                             (tail row2))) Nil)
                       (tail rows2));

minchoice m = minimum (filter gte2 (concatMap (map length) m));

gte2 x = (<=) 2 x;

search cm =
  case blocked cm of {
    True -> Nil;
    False -> case all (all single) cm of {
               True -> Cons cm Nil;
               False -> concatMap (comp search prune) (expand cm);
             };
  };

sudoku b = map (map (map head)) (search (prune (choices b)));

emitRow Nil k = emit '\n' k;
emitRow (Cons x xs) k = emitInt x (emit ' ' (emitRow xs k));

emitMatrix Nil k = k;
emitMatrix (Cons x xs) k = emitRow x (emitMatrix xs k);

main = emitMatrix (head (
       sudoku (Cons (Cons 0 (Cons 0 (Cons 0
                    (Cons 0 (Cons 0 (Cons 3
                    (Cons 0 (Cons 6 (Cons 0 Nil)))))))))
              (Cons (Cons 0 (Cons 0 (Cons 0
                    (Cons 0 (Cons 0 (Cons 0
                    (Cons 0 (Cons 1 (Cons 0 Nil)))))))))
              (Cons (Cons 0 (Cons 9 (Cons 7
                    (Cons 5 (Cons 0 (Cons 0
                    (Cons 0 (Cons 8 (Cons 0 Nil)))))))))
              (Cons (Cons 0 (Cons 0 (Cons 0
                    (Cons 0 (Cons 9 (Cons 0
                    (Cons 2 (Cons 0 (Cons 0 Nil)))))))))
              (Cons (Cons 0 (Cons 0 (Cons 8
                    (Cons 0 (Cons 7 (Cons 0
                    (Cons 4 (Cons 0 (Cons 0 Nil)))))))))
              (Cons (Cons 0 (Cons 0 (Cons 3
                    (Cons 0 (Cons 6 (Cons 0
                    (Cons 0 (Cons 0 (Cons 0 Nil)))))))))
              (Cons (Cons 0 (Cons 1 (Cons 0
                    (Cons 0 (Cons 0 (Cons 2
                    (Cons 8 (Cons 9 (Cons 0 Nil)))))))))
              (Cons (Cons 0 (Cons 4 (Cons 0
                    (Cons 0 (Cons 0 (Cons 0
                    (Cons 0 (Cons 0 (Cons 0 Nil)))))))))
              (Cons (Cons 0 (Cons 5 (Cons 0
                    (Cons 1 (Cons 0 (Cons 0
                    (Cons 0 (Cons 0 (Cons 0 Nil)))))))))
               Nil))))))))))) 0;

}
