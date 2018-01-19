=============================
REDUCERON MEMO 27
Rotation and one-hot addition
Matthew N, 25 June 2009
=============================

Bit-vector rotation and one-hot addition are two useful operations in
the implementation of the Reduceron's wide stack.  The former
operation rotates a sequence of bits by a given number of places, and
the latter sums two one-hot numbers to give a resulting one-hot
number.  We define these two operations only to learn that they are
one and the same!  In the process we find an interesting property of
each operation which by equivalence also holds, somewhat surprisingly,
for the other.

This memo is literate Haskell.

> import List (transpose)

Unary numbers
-------------

A one-hot non-negative integer n is a list of booleans whose (n+1)th
element is True and whose other elements are all False.  For example,
the one-hot numbers of width 4, in increasing order, beginning with 0,
are:

  [True , False, False, False],
  [False, True , False, False],
  [False, False, True , False],
  [False, False, False, True ].

The negative one-hot numbers are defined such that for all non-zero
integers n, negate n = reverse n.

Rotation
--------

The function rot,

  rot :: [Bool] -> [Bool] -> [Bool]

is to be defined such that rot n xs rotates the list of bits xs to the
right by n places where n is a one-hot number.  It can be assumed that n
and xs have the same length.

Unary addition
--------------

The function add,

  add :: [Bool] -> [Bool] -> [Bool]

is to be defined such that add n m returns the one-hot representation of
(i+j) `mod` w, where i and j are the integers represented by n and m,
and w is the width of both n and m.

Challenge
---------

Please feel free to define these operations for yourself.  To ensure
that the functions are realisable as digital circuits, case analysis
on boolean values should be avoided; only standard boolean connectives
such as not, && and || should be used to process the boolean-valued
inputs.

Solution: Rotation
------------------

To rotate a bit-vector xs to the right by n places, we iteratively
compute all the right-rotations of xs and select nth one.

> left [] = []
> left (x:xs) = xs ++ [x]

> right = reverse . left . reverse

> rights xs = take (length xs) (iterate right xs)

> dot xs ys = or (zipWith (&&) xs ys)

> sel n xs = map (dot n) (transpose xs)

> rot n xs = sel n (rights xs)

Solution: Addition
------------------

To compute the nth bit of the one-hot sum, we find all the combinations
of pairs of input bits that add to n, and return the sum of the
products of this list of pairs.

> split [] = []
> split (x:xs) = ([x], xs) : [(x:y, z) | (y, z) <- split xs]

> tac (xs, ys) = reverse xs ++ reverse ys

> add a b = map (dot a) (map tac (split b))

Equivalence
-----------

After defining these two functions it becomes apparant that if

  map tac . split = transpose . rights

then the two are equivalent, which is indeed the case.

Observation 1: The add operation, by its specification, is
commutative, and if it is equivalent to rot, then rot must also be
commutative.

Observation 2: The add operation, by its specification, only needs to
rotate one-hot numbers, but if it is equivalent to rot, then it must be
able to rotate arbitrary bit-vectors too.

These two facts were quite surprising to learn!

Array rotation
--------------

The following function, used to implement the wide stack, rotates a
list of bit-vectors to the right by n places, where n is a given one-hot
number.

> rotate :: [Bool] -> [[Bool]] -> [[Bool]]
> rotate n = transpose . map (add n) . transpose 
