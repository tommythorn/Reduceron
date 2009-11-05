{

map f Nil = Nil;
map f (Cons x xs) = Cons (f x) (map f xs);

bestOf Win v = Win;
bestOf Loss v = v;
bestOf Draw Win = Win;
bestOf Draw Draw = Draw;
bestOf Draw Loss = Draw;

inverse Loss = Win;
inverse Draw = Draw;
inverse Win  = Loss;

fromTo n m = case (<=) n m of {
               True -> Cons n (fromTo ((+) n 1) m);
               False -> Nil;
             };

cmp a b = 
  case (==) a b of {
    True -> EQ;
    False -> case (<=) a b of { True -> LT ; False -> GT };
  };

insert x Nil = Cons x Nil;
insert x (Cons y ys) = case (<=) x y of {
                         True -> Cons x (Cons y ys);
                         False -> Cons y (insert x ys);
                       };

foldr1 f (Cons x Nil) = x;
foldr1 f (Cons x (Cons y ys)) = f x (foldr1 f (Cons y ys));

diff Nil ys = Nil;
diff (Cons x xs) Nil = Cons x xs;
diff (Cons x xs) (Cons y ys) =
  case cmp x y of {
    LT -> Cons x (diff xs (Cons y ys));
    EQ -> diff xs ys;
    GT -> diff (Cons x xs) ys;
  };

null Nil = True;
null (Cons x xs) = False;

subset xs ys = null (diff xs ys);

or False x = x;
or True x = True;

hasLine p =
  or (subset (Cons 1 (Cons 2 (Cons 3 Nil))) p)
    (or (subset (Cons 4 (Cons 5 (Cons 6 Nil))) p)
      (or (subset (Cons 7 (Cons 8 (Cons 9 Nil))) p)
        (or (subset (Cons 1 (Cons 4 (Cons 7 Nil))) p)
          (or (subset (Cons 2 (Cons 5 (Cons 8 Nil))) p)
            (or (subset (Cons 3 (Cons 6 (Cons 9 Nil))) p)
              (or (subset (Cons 1 (Cons 5 (Cons 9 Nil))) p)
                (subset (Cons 3 (Cons 5 (Cons 7 Nil))) p)))))));

length Nil = 0;
length (Cons x xs) = (+) 1 (length xs);

gridFull ap pp = (==) ((+) (length ap) (length pp)) 9;

analysis ap pp =
  case hasLine pp of {
    True -> Loss;
    False ->
      case gridFull ap pp of {
        True -> Draw;
        False -> foldr1 bestOf (map (moveval ap pp)
                   (diff (diff (fromTo 1 9) ap) pp));
      };
  };

moveval ap pp m = inverse (analysis pp (insert m ap));

adjudicate os xs =
  case cmp (length os) (length xs) of {
    GT -> report (analysis xs os) X;
    EQ -> case hasLine xs of {
            True -> report Win X;
            False -> case hasLine os of {
                       True -> report Win O;
                       False -> report (analysis xs os) X;
                     };
          };
    LT -> report (analysis os xs) O;
  };

report Loss s = side (opp s);
report Win  s = side s;
report Draw p = 'D';

opp O = X;
opp X = O;

side O = 'O';
side X = 'X';

main = emit (adjudicate Nil Nil) 0;

}
