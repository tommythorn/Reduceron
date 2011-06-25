module Perms3 where

data List a = Nil | Cons a (List a) | Cons2 a a (List a) | Cons3 a a a (List a)

map3 f Nil = Nil
map3 f (Cons x xs) = Cons (f x) (map3 f xs)
map3 f (Cons2 x0 x1 xs) = Cons2 (f x0) (f x1) (map3 f xs)
map3 f (Cons3 x0 x1 x2 xs) = Cons3 (f x0) (f x1) (f x2) (map3 f xs)

sum3 :: List Int -> Int
sum3 Nil = 0
sum3 (Cons x xs) = x + sum3 xs
sum3 (Cons2 x0 x1 xs) = x0 + x1 + sum3 xs
sum3 (Cons3 x0 x1 x2 xs) = x0 + x1 + x2 + sum3 xs

concat3 Nil = Nil
concat3 (Cons x xs) = x `append3` concat3 xs
concat3 (Cons2 x0 x1 xs) = x0 `append3` x1 `append3` concat3 xs
concat3 (Cons3 x0 x1 x2 xs) =
  x0 `append3` x1 `append3` x2 `append3` concat3 xs

infixr 9 `append3`

append3 Nil ys = ys
append3 (Cons x xs) ys = Cons x (xs `append3` ys)
append3 (Cons2 x0 x1 xs) ys = Cons2 x0 x1 (xs `append3` ys)
append3 (Cons3 x0 x1 x2 xs) ys = Cons3 x0 x1 x2 (xs `append3` ys)

place1 x Nil = Cons (Cons x Nil) Nil
place1 x (Cons y ys) =
  Cons (Cons2 x y ys)
       (map3 (Cons y) (place1 x ys))
place1 x (Cons2 y0 y1 ys) =
  Cons2 (Cons3 x y0 y1 ys)
        (Cons3 y0 x y1 ys)
        (map3 (Cons2 y0 y1) (place1 x ys))
place1 x (Cons3 y0 y1 y2 ys) =
  Cons3 (Cons3 x y0 y1 (Cons y2 ys))
        (Cons3 y0 x y1 (Cons y2 ys))
        (Cons3 y0 y1 x (Cons y2 ys))
        (map3 (Cons3 y0 y1 y2) (place1 x ys))

place3 x0 x1 x2 Nil = combs3 x0 x1 x2 Nil
place3 x0 x1 x2 ys =
  concat3 (map3 (place1 x0)
    (concat3 (map3 (place1 x1)
                   (place1 x2 ys))))

combs3 x1 x2 x3 xs =
   Cons3 (Cons3 x1 x2 x3 xs)
         (Cons3 x2 x1 x3 xs)
         (Cons3 x2 x3 x1 xs)
  (Cons3 (Cons3 x1 x3 x2 xs)
         (Cons3 x3 x1 x2 xs)
         (Cons3 x3 x2 x1 xs) Nil)

perms3 Nil = Cons Nil Nil
perms3 (Cons x xs) = concat3 (map3 (place1 x) (perms3 xs))
perms3 (Cons2 x0 x1 xs) =
  concat3 (map3 (place1 x0) (concat3 (map3 (place1 x1) (perms3 xs))))
perms3 (Cons3 x0 x1 x2 xs) =
  concat3 (map3 (place3 x0 x1 x2) (perms3 xs))

perms1 Nil = Cons Nil Nil
perms1 (Cons x xs) = concat3 (map3 (place1 x) (perms1 xs))


{-
ordered3 :: List Int -> Bool
ordered3 Nil = True
ordered3 (Cons x xs) = 
  case xs of
    Nil -> True
    Cons y ys -> x <= y && ordered3 (Cons y ys)
    Cons2 y0 y1 ys -> x <= y0 && y0 <= y1 && ordered3 (Cons y1 ys)
    Cons3 y0 y1 y2 ys ->
      x <= y0 && y0 <= y1 && y1 <= y2 && ordered3 (Cons y2 ys)
ordered3 (Cons2 y0 y1 ys) = y0 <= y1 && ordered3 (Cons y1 ys)
ordered3 (Cons3 y0 y1 y2 ys) = y0 <= y1 && y1 <= y2 && ordered3 (Cons y2 ys)

filter3 :: (a -> Bool) -> List a -> List a
filter3 p Nil = Nil
filter3 p (Cons x xs) =
  if p x then Cons x (filter3 p xs) else filter3 p xs
filter3 p (Cons2 x0 x1 xs) =
  let rest0 = if p x0 then Cons x0 rest1 else rest1
      rest1 = if p x1 then Cons x1 rest2 else rest2
      rest2 = filter3 p xs
  in  rest0
filter3 p (Cons3 x0 x1 x2 xs) =
  let rest0 = if p x0 then Cons x0 rest1 else rest1
      rest1 = if p x1 then Cons x1 rest2 else rest2
      rest2 = if p x2 then Cons x2 rest3 else rest3
      rest3 = filter3 p xs
  in  rest0

undef :: a
undef = undef

head3 :: List a -> a
head3 Nil = undef
head3 (Cons x xs) = x
head3 (Cons2 x0 x1 xs) = x0
head3 (Cons3 x0 x1 x2 xs) = x0

permsort3 :: List Int -> List Int
permsort3 xs = head3 (filter3 ordered3 (perms3 xs))
-}

main :: Int
--main = head3 (permsort3 (Cons 9 (Cons 8 (Cons 7
--                        (Cons 6 (Cons 5 (Cons 4
--                        (Cons 3 (Cons 2 (Cons 1 Nil))))))))))
--main = head3 (permsort3 (Cons3 9 8 7 (Cons3 6 5 4 (Cons3 3 2 1 Nil))))
--main = sum3 (map3 sum3 (perms3 (Cons3 0 0 0 (Cons3 0 0 0 Nil))))
main = sum3 (map3 sum3 (perms1 (Cons 0 (Cons 0 (Cons 0
                               (Cons 0 (Cons 0 (Cons 0 Nil))))))))
