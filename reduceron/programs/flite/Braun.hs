{

insert x Empty = Branch x Empty Empty;
insert x (Branch y t0 t1) = Branch x (insert y t1) t0;

and False y = False;
and True y = y;

size Empty = 0;
size (Branch x t0 t1) = (+) 1 ((+) (size t0) (size t1));

fromList Nil = Empty;
fromList (Cons x xs) = insert x (fromList xs);

toList Empty = Nil;
toList (Branch x t0 t1) = Cons x (ilv (toList t0) (toList t1));

ilv Nil ys = ys;
ilv (Cons x xs) Nil = Cons x xs;
ilv (Cons x xs) (Cons y ys) = Cons x (Cons y (ilv xs ys));

replicate n x = if (==) n 0 then Nil else Cons x (replicate ((-) n 1) x);

fromTo n m = case (<=) n m of {
               True -> Cons n (fromTo ((+) n 1) m);
               False -> Nil;
             };

repeat x = Cons x (repeat x);

equal Nil Nil = True;
equal Nil (Cons y ys) = False;
equal (Cons x xs) Nil = False;
equal (Cons x xs) (Cons y ys) =
  case (==) x y of {
    False -> False;
    True -> equal xs ys;
  };


prop xs = equal xs (toList (fromList xs));

all p Nil = True;
all p (Cons x xs) = and (p x) (all p xs);

int True = 1;
int False = 0;

main = int (all prop (replicate 6000 (fromTo 0 255)));

}
