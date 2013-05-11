{

del x [] = [];
del x (Cons y ys) =
  case (==) x y of { True -> ys ; False -> Cons y (del x ys) };

diff xs [] = xs;
diff xs (Cons y ys) = diff (del y xs) ys;

head (Cons x xs) = x;
tail (Cons x xs) = xs;

length [] = 0;
length (Cons x xs) = (+) 1 (length xs);

sum [] = 0;
sum (Cons x xs) = (+) x (sum xs);

null [] = True;
null (Cons x xs) = False;

single [] = False;
single (Cons x xs) = null xs;

minimum (Cons x xs) = min x xs;

min m [] = m;
min m (Cons x xs) = case (<=) x m of { True -> min x xs ; False -> min m xs };

break p [] = Pair [] [];
break p (Cons x xs) =
  case p x of {
    True -> Pair [] (Cons x xs);
    False -> case break p xs of { Pair ys zs -> Pair (Cons x ys) zs };
  };

filter p [] = [];
filter p (Cons x xs) =
  case p x of {
    True -> Cons x (filter p xs);
    False -> filter p xs;
  };

zipWith f [] ys = [];
zipWith f (Cons x xs) [] = [];
zipWith f (Cons x xs) (Cons y ys) = Cons (f x y) (zipWith f xs ys);

notElem x [] = True;
notElem x (Cons y ys) = and ((/=) x y) (notElem x ys);

and False x = False;
and True x = x;

not False = True;
not True = False;

or False x = x;
or True x = True;

any p [] = False;
any p (Cons x xs) = or (p x) (any p xs);

all p [] = True;
all p (Cons x xs) = and (p x) (all p xs);

map f [] = [];
map f (Cons x xs) = Cons (f x) (map f xs);

append [] ys = ys;
append (Cons x xs) ys = Cons x (append xs ys);

concat [] = [];
concat (Cons xs xss) = append xs (concat xss);

concatMap f [] = [];
concatMap f (Cons x xs) = append (f x) (concatMap f xs);

take n [] = [];
take n (Cons x xs) =
  case (==) n 0 of {
    True -> [];
    False -> Cons x (take ((-) n 1) xs);
  };

drop n [] = [];
drop n (Cons x xs) =
  case (==) n 0 of {
    True -> Cons x xs;
    False -> drop ((-) n 1) xs;
  };

groupBy n xs =
  case null xs of {
    True -> [];
    False -> Cons (take n xs) (groupBy n (drop n xs));
  };

id x = x;

comp f g x = f (g x);

boardsize = 9;
boxsize   = 3;
cellvals  = Cons 1 (Cons 2 (Cons 3 (
            Cons 4 (Cons 5 (Cons 6 (
            Cons 7 (Cons 8 (Cons 9 []))))))));

blank x = (==) x 0;

nodups [] = True;
nodups (Cons x xs) = and (notElem x xs) (nodups xs);

singleton x = Cons x [];

cols (Cons xs []) = map singleton xs;
cols (Cons xs (Cons ys yss)) = zipWith Cons xs (cols (Cons ys yss));

boxs m = map concat (concatMap cols (groupBy 3 (map (groupBy 3) m)));

choices b = map (map choose) b;

choose e = case blank e of { True -> cellvals ; False -> Cons e [] };

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
  append rows1 (append (Cons (append row1 (Cons (Cons c [])
                             (tail row2))) [])
                       (tail rows2));

minchoice m = minimum (filter gte2 (concatMap (map length) m));

gte2 x = (<=) 2 x;

search cm =
  case blocked cm of {
    True -> [];
    False -> case all (all single) cm of {
               True -> Cons cm [];
               False -> concatMap (comp search prune) (expand cm);
             };
  };

sudoku b = map (map (map head)) (search (prune (choices b)));

emitRow [] k = emit '\n' k;
emitRow (Cons x xs) k = emitInt x (emit ' ' (emitRow xs k));

emitMatrix [] k = k;
emitMatrix (Cons x xs) k = emitRow x (emitMatrix xs k);

main = emitMatrix (head (
       sudoku (Cons (Cons 0 (Cons 0 (Cons 0
                    (Cons 0 (Cons 0 (Cons 3
                    (Cons 0 (Cons 6 (Cons 0 [])))))))))
              (Cons (Cons 0 (Cons 0 (Cons 0
                    (Cons 0 (Cons 0 (Cons 0
                    (Cons 0 (Cons 1 (Cons 0 [])))))))))
              (Cons (Cons 0 (Cons 9 (Cons 7
                    (Cons 5 (Cons 0 (Cons 0
                    (Cons 0 (Cons 8 (Cons 0 [])))))))))
              (Cons (Cons 0 (Cons 0 (Cons 0
                    (Cons 0 (Cons 9 (Cons 0
                    (Cons 2 (Cons 0 (Cons 0 [])))))))))
              (Cons (Cons 0 (Cons 0 (Cons 8
                    (Cons 0 (Cons 7 (Cons 0
                    (Cons 4 (Cons 0 (Cons 0 [])))))))))
              (Cons (Cons 0 (Cons 0 (Cons 3
                    (Cons 0 (Cons 6 (Cons 0
                    (Cons 0 (Cons 0 (Cons 0 [])))))))))
              (Cons (Cons 0 (Cons 1 (Cons 0
                    (Cons 0 (Cons 0 (Cons 2
                    (Cons 8 (Cons 9 (Cons 0 [])))))))))
              (Cons (Cons 0 (Cons 4 (Cons 0
                    (Cons 0 (Cons 0 (Cons 0
                    (Cons 0 (Cons 0 (Cons 0 [])))))))))
              (Cons (Cons 0 (Cons 5 (Cons 0
                    (Cons 1 (Cons 0 (Cons 0
                    (Cons 0 (Cons 0 (Cons 0 [])))))))))
               []))))))))))) 0;

}
