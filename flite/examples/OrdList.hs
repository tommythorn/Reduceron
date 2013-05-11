{

implies False x = True;
implies True x = x;

and False x = False;
and True x = x;

andList [] = True;
andList (Cons x xs) = and x (andList xs);

append [] ys = ys;
append (Cons x xs) ys = Cons x (append xs ys);

map f [] = [];
map f (Cons x xs) = Cons (f x) (map f xs);

ord [] = True;
ord (Cons x []) = True;
ord (Cons x (Cons y ys)) = and (implies x y) (ord (Cons y ys));

insert x [] = Cons x [];
insert x (Cons y ys) =
  case implies x y of {
    True -> Cons x (Cons y ys);
    False -> Cons y (insert x ys);
  };

prop x xs = implies (ord xs) (ord (insert x xs));

boolList Z = Cons [] [];
boolList (S n) =
  append (boolList n)
         (append (map (Cons False) (boolList n))
                 (map (Cons True) (boolList n)));

top n = andList (append (map (prop True) (boolList n))
                        (map (prop False) (boolList n)));

main = let { eleven = S (S (S (S (S (S (S (S (S (S (S Z)))))))))) } in
         case top eleven of {
           False -> emitInt 0 0;
           True  -> emitInt 1 0;
         };

}
