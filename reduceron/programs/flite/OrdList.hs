{

implies False x = True;
implies True x = x;

and False x = False;
and True x = x;

andList Nil = True;
andList (Cons x xs) = and x (andList xs);

append Nil ys = ys;
append (Cons x xs) ys = Cons x (append xs ys);

map f Nil = Nil;
map f (Cons x xs) = Cons (f x) (map f xs);

ord Nil = True;
ord (Cons x Nil) = True;
ord (Cons x (Cons y ys)) = and (implies x y) (ord (Cons y ys));

insert x Nil = Cons x Nil;
insert x (Cons y ys) =
  case implies x y of {
    True -> Cons x (Cons y ys);
    False -> Cons y (insert x ys);
  };

prop x xs = implies (ord xs) (ord (insert x xs));

boolList Z = Cons Nil Nil;
boolList (S n) =
  append (boolList n)
         (append (map (Cons False) (boolList n))
                 (map (Cons True) (boolList n)));

top n = andList (append (map (prop True) (boolList n))
                        (map (prop False) (boolList n)));

main = let { num = S (S (S (S (S (S (S (S (S (S (S (S Z))))))))))) } in
         case top num of {
           False -> 0;
           True  -> 1;
         };

}
