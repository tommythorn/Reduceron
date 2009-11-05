{

and False x = False;
and True x = x;

head (Cons x xs) = x;

map f Nil = Nil;
map f (Cons x xs) = Cons (f x) (map f xs);

append Nil ys = ys;
append (Cons x xs) ys = Cons x (append xs ys);

concatMap f Nil = Nil;
concatMap f (Cons x xs) = append (f x) (concatMap f xs);

filter p Nil = Nil;
filter p (Cons x xs) = case p x of {
                         True -> Cons x (filter p xs);
                         False -> filter p xs;
                       };

place x Nil = Cons (Cons x Nil) Nil;
place x (Cons y ys) = Cons (Cons x (Cons y ys)) (map (Cons y) (place x ys));

perm Nil = Cons Nil Nil;
perm (Cons x xs) = concatMap (place x) (perm xs);

ord Nil = True;
ord (Cons x Nil) = True;
ord (Cons x (Cons y ys)) = and ((<=) x y) (ord (Cons y ys));

permSort xs = head (filter ord (perm xs));

emitList Nil k = emit '\n' k;
emitList (Cons x xs) k = emitInt x (emit ' ' (emitList xs k));

main = emitList (permSort (Cons 9 (Cons 8 (Cons 7 (
                          (Cons 6 (Cons 5 (Cons 4 (
                          (Cons 3 (Cons 2 (Cons 1 Nil)))))))))))) 0;

}
