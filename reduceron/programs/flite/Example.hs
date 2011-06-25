{

sum Nil = 0;
sum (Cons x xs) = (+) x (sum xs);

double x = (+) x x;

main = double (sum (Cons 1 (Cons 2 Nil)));

}
