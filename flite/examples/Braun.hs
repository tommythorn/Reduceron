{

xor False False = False;
xor False True = True;
xor True False = True;
xor True True = False;

eqv False False = True;
eqv False True = False;
eqv True False = False;
eqv True True = True;

and False y = False;
and True y = y;

not True = False;
not False = True;

isZero Nil = True;
isZero (Cons x xs) = if x then False else isZero xs;

inc x = incBy True x;

incBy x Nil = Nil;
incBy x (Cons y ys) = Cons (xor x y) (incBy (and x y) ys);

dec x = decBy True x;

decBy x Nil = Nil;
decBy x (Cons y ys) = Cons (xor y x) (decBy (and x (not y)) ys);

head (Cons x xs) = x;

half (Cons x xs) = xs;

equal Nil Nil = True;
equal Nil (Cons y ys) = False;
equal (Cons x xs) Nil = False;
equal (Cons x xs) (Cons y ys) = and (eqv x y) (equal xs ys);

insert x Empty = Branch x Empty Empty;
insert x (Branch y t0 t1) = Branch x (insert y t1) t0;

elemAt (Branch x t0 t1) i =
  case isZero i of {
    False -> if head i then elemAt t0 (half i) else elemAt t1 (dec (half i));
    True -> x;
  };

upto m n = if equal m n then Cons m Nil else Cons m (upto (inc m) n);

tree Nil = Empty;
tree (Cons x xs) = insert x (tree xs);

check t Nil = True;
check t (Cons x xs) = and (equal x (elemAt t x)) (check t xs);

replicate n x = if (==) n 0 then Nil else Cons x (replicate ((-) n 1) x);

int True = 1;
int False = 0;

main = 
  let { ns = upto (replicate 14 False) (replicate 14 True) }
  in int (check (tree ns) ns);

}
