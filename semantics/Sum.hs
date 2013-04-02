{
sum l = case l of {
         Cons x xs -> (+) x (sum xs); Nil -> 0;};

main = 42;
}

