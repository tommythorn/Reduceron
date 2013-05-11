{

p n = length (partitions n) ;

partitions n = partitionsWith n (countDown n) ;

partitionsWith n ns = case (==) n 0 of {
                        True  -> Cons [] [] ;
                        False -> concatMap (partitionsWith0 n ns) ns ;
                      } ;

and False x = False;
and True x = x;

lt n m = and ((/=) n m) ((<=) n m);

partitionsWith0 n ns i =
  let { n0 = (-) n i ; m  = min i n0 } in
    map (Cons i) (partitionsWith n0 (dropWhile (lt m) ns)) ;

length [] = 0 ;
length (Cons x xs) = (+) 1 (length xs) ;

countDown n = case (<=) 1 n of {
                True  -> Cons n (countDown ((-) n 1)) ;
                False -> [] ;
              };

concatMap f [] = [] ;
concatMap f (Cons x xs) = append (f x) (concatMap f xs) ;

append []         ys = ys ;
append (Cons x xs) ys = Cons x (append xs ys) ;

min m n = case (<=) m n of {
            True  -> m ;
            False -> n ;
          };

map f [] = [] ;
map f (Cons x xs) = Cons (f x) (map f xs) ;

dropWhile p xs = case xs of {
                   []        -> [] ;
                   Cons x xs0 -> case p x of {
                                   True  -> dropWhile p xs0 ;
                                   False -> xs ;
                                 };
                 } ;

main = emitInt (p 20) 0;

}
