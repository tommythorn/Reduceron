=======================
REDUCERON MEMO 27
Design of the Octostack
Matthew N, 6 July 2009
=======================

This memo presents various hardware designs of a stack data structure
allowing:

  * the top N elements of the stack to be observed;
  * up to N elements to be popped off the stack;
  * and up to N elements to be pushed onto the stack.

In the Reduceron, where N is defined to be 8, this data structure is
referred to as the "Octostack".  The intention is that, on FPGA, the
pushing and popping operations each take a single clock-cycle to
perform (and can be performed in parallel), and the top elements can
always be observed in zero clock-cycles.

> import List
> import Maybe
> import Array

Interface
---------

The stack is parameterised by N, now referred to as 'blockSize'.

> blockSize :: Int
> blockSize = 8

A block is a container storing exactly 'blockSize' elements, although
this constraint is not enforced by the type system.

> type Block a = [a]

The data structure provides four operations.

> class Stack s where
>   empty  :: s a
>   size   :: s a -> Int
>   tops   :: s a -> Block (Maybe a)
>   update :: Int -> Block (Maybe a) -> s a -> s a

The 'Maybe' type is used to represent partial-definedness.  If the top
block of stack elements is requested and the stack contains fewer than
'blockSize' elements, then the returned block will contain some
undefined elements.  Similarly, when writing a new block of
elements to the stack, only the defined elements in the block are
written, not the undefined ones.

The 'update' operation is the only slightly unusual one.  It takes
three arguments:

  * an integer offset in the range '-blocksize' to 'blocksize';
  * a block of elements to write onto the top of the stack;
  * and a stack.

And it returns a new stack whose size is equal to the size of the
input stack plus the offset, and whose top 'blockSize' elements are
overwritten with the defined values in the write-block.

Example
-------

To illustrate the stack interface, let us start with the empty stack
and perform a few pushes and pops.

> s1, s2, s3, s4, s5 :: Stack s => s Int
> s1 = empty

We will use the following shorthands for 'Nothing' and 'Just'.

> n :: Maybe a
> n = Nothing

> j :: a -> Maybe a
> j a = Just a

To push the elements 1 to 5:

> s2 = update 5 [j 1,j 2,j 3,j 4,j 5,n,n,n] s1

To push the elements 10 to 13:

> s3 = update 4 [j 10,j 11,j 12,j 13,n,n,n,n] s2

To pop two elements:

> s4 = update (-2) [n,n,n,n,n,n,n,n] s3

To pop three elements, and in parallel, push the element 20:

> s5 = update (-2) [j 20,n,n,n,n,n,n,n] s4

Notice that the offset (passed as the first argument to 'update') is
in general equal to the number of pushes minus the number of pops.

Implementation 1
----------------

Since we are targeting FPGAs, we will make use of RAMs to implement
the stack.  The fact that the locations in a RAM may be uninitialised
is captured by the 'Maybe' type.

> type RAM a = Array Int (Maybe a)

It is important to account for the fact that RAMs have a capacity.

> ramCapacity :: Int
> ramCapacity = 1024

The actual value doesn't really matter here, but it is needed to
compute correct array indexes.  In particular, if a negative number,
say -n, is placed on the address bus of a RAM then it is reasonable to
expect the nth element from the end of the RAM appear on the data bus.
This is because, on FPGA, RAM addresses are in twos complement form.

> wrap :: Int -> Int
> wrap n = (if n < 0 then n + ramCapacity else n) `mod` ramCapacity

We represent a stack by a size/contents pair.

> data Stack1 a =
>   Stack1 {
>     size1 :: Int
>   , contents1 :: RAM a
>   }

Given the size of a stack, the following function computes the
addresses of the top 'blockSize' elements.

> addresses1 :: Int -> [Int]
> addresses1 n = [wrap (n-i) | i <- [1..blockSize]]

Now for the implementation.

> instance Stack Stack1 where
>   empty = Stack1 { size1 = 0, contents1 = ram }
>     where ram = listArray (0, ramCapacity-1) (replicate ramCapacity Nothing)
>
>   size s = size1 s
>
>   tops s = map (contents1 s !) (addresses1 (size1 s))
>
>   update offset writes s =
>     Stack1 { size1 = newSize, contents1 = contents1 s // updates }
>     where
>      newSize = size1 s + offset
>      updates = [u | u@(_, Just _) <- zip (addresses1 newSize) writes]

> example1 = tops (s5 :: Stack1 Int)

As a quick test, 'example1' has the following value.

  [Just 20,Just 2,Just 3,Just 4,Just 5,Nothing,Nothing,Nothing]

Nasty property of Implementation 1
----------------------------------

If the top 'blockSize' elements are to be read/written in a single
clock-cycle, then the above design requires a RAM with 'blockSize'
read/write ports.  Implementing a RAM with an arbitrary number of read
ports is easy on an FPGA, but to implement one with more than two
write ports is very difficult - in fact, it seems only possible using
flip-flops, which is not very scalable.

Nice property of Implementation 1
---------------------------------

An attractive property is that the same addresses are used both when
reading from and writing to the top 'blockSize' locations of the RAM.
This need not necessarily be the case: to write three elements onto
the stack, we only need to set three addresses correctly; it doesn't
matter what the other five addresses are set to.

However, if the addresses are the same when reading as when writing
then reading and writing can be done in parallel.  In particular,
after writing to the stack, we do not have to wait until the next
clock cycle in order to schedule a read of the new top elements.  We
can, for example, pop four elements from the stack, push two new ones
and, *at the same time*, schedule a read the top eight elements.  

(All this is thanks to the fact that FPGA block RAMS have separate
data busses for reading and writing, with a "WRITE_FIRST" semantics.)

Rotation functions
------------------

The following general-purpose functions will be useful in the next
stack implementations.

Rotate a list 'n' elements to the left; 'n' must be non-negative.

> left :: Int -> [a] -> [a]
> left n xs = drop n xs ++ take n xs

Rotate a list 'n' elements to the right; 'n' must be non-negative.

> right :: Int -> [a] -> [a]
> right n = reverse . left n . reverse

A few algebraic properties.

    reverse . right n
  = { definition of right }
    reverse . reverse . left n . reverse
  = { reverse . reverse = id }
    left n . reverse

Observing that

  left n = reverse . right n . reverse

we also have 

  reverse . left n = right n . reverse

Rotate a list 'n' elements to the right; if 'n' is negative the list is
rotated '|n|' elements to the left.

> rotate :: Int -> [a] -> [a]
> rotate n = if n < 0 then left (-n) else right n

(See Memo 26 for circuit-level implementations of these functions.)

Implementation 2
----------------

To avoid the need for a single RAM with 'blockSize' ports, we now use
'blockSize' RAMs, each with a single port.

> data Stack2 a =
>   Stack2 {
>     size2 :: Int
>   , contents2 :: Block (RAM a)
>   }

If the RAMs are numbered 0 to 7 then the RAM numbered 'n' stores the
elements with the following stack indices.  (Note about the term
'stack index': the element at the bottom of the stack has index 0, and
the element at the top has index 'm-1' where 'm' is the number of
elements on the stack.)

  n, n+blockSize, n+2*blockSize, n+3*blockSize, ...

For example, when 'blockSize' = 8:

  RAM   Stack indices of elements stored
  0     0,  8, 16, ...
  1     1,  9, 17, ...
  2     2, 10, 18, ...
  3     3, 11, 19, ...
  4     4, 12, 20, ...
  5     5, 13, 21, ...
  6     6, 14, 22, ...
  7     7, 15, 23, ...

Given the index of an element on the stack, we can determine which RAM
it is stored in.

> lower :: Int -> Int
> lower i = i `mod` blockSize

We can also determine the RAM location where an element is stored.

> upper :: Int -> Int
> upper i = i `div` blockSize

Given the size of the stack, the following function computes, for each
RAM 0 to 7, the address of that RAM's topmost element.

> addresses2 :: Int -> [Int]
> addresses2 n = [upper (wrap (n-i)) | i <- [1..blockSize]]

> instance Stack Stack2 where
>   empty = Stack2 { size2 = 0, contents2 = replicate blockSize ram }
>     where ram = listArray (0, ramCapacity-1) (replicate ramCapacity Nothing)
>
>   size s = size2 s

For example, considering a stack of size 19, the addresses of the
topmost element in RAMs 0 to 7 are 2,2,2,1,1,1,1,1 respectively.
Indexing the RAMs by these addresses gives the stack elements with
indices 16,17,18,11,12,13,14,15.  To obtain these top elements in
order, we rotate them by 'n' positions (here 3) to the left, where 'n'
is the number of the RAM holding of the top element.  More formally,
'n = lower m' where 'm' is the size of the stack.  Finally, we can
reverse this sequence to give the top elements in reverse order, so
that the top stack element comes first.  Recall that rotating left and
then reversing is equivalent to reversing and then rotating right.  We
prefer the latter.

>   tops s = right (lower (size2 s)) (reverse ramOuts)
>     where ramOuts = zipWith (!) (contents2 s) (addresses2 (size2 s))

A similar rotation method is used when updating the stack.  The main
difference is that instead of reversing and rotating the RAM outputs,
we reverse and rotate the RAM inputs.

>   update offset writes s =
>     Stack2 { size2 = newSize, contents2 = zipWith (//) (contents2 s) uss }
>     where
>       newSize = size2 s + offset
>       writes' = reverse (left (lower newSize) writes)
>       updates = zip (addresses2 newSize) writes'
>       uss     = [[u | isJust x] | u@(_,x) <- updates]

> example2 = tops (s5 :: Stack2 Int)

Implementation 3
----------------

The problem with the above implementation is that there are large
logic delays on the input and output busses of the RAMs --- observing
and updating the top stack elements incurs the RAM-access delay plus
the delay of the rotation logic.  One way to reduce this delay is to
store the top elements in registers.  The following is a wrapper
around the above implementation which caches the top 'blockSize'
elements in registers.

The cache is a block of registers, storing the top elements, with the
top element coming first.

> type Cache a = Block (Maybe a)

> data Stack3 a =
>   Stack3 {
>     cache :: Cache a
>   , stack :: Stack2 a
>   }

> instance Stack Stack3 where
>   empty = Stack3 { cache = replicate blockSize Nothing, stack = empty }
>
>   size s = size (stack s)
>
>   tops s = cache s

To update the stack:

  * the defined elements in the write-block are written into the cache
    registers;

  * if 'offset' is positive, then the values of the upper 'offset'
    cache registers are pushed onto the RAM stack;

  * if 'offset' is negative, then the top '|offset|' values on the RAM
    stack are popped and written into the upper '|offset|' cache
    registers;

  * any cache register 'n' not modified by the above three cases is
    updated to contain the value of cache register 'wrap (n+offset)'.

>   update offset writes s =
>     Stack3 {
>       cache = newCache
>     , stack = update offset push (stack s)
>     }
>     where
>       rotated  = rotate offset (cache s)
>       push     = [ if i <= offset then x else Nothing
>                  | (i, x) <- zip [1..blockSize] rotated ]
>       pushMask = [isJust x | x <- writes]
>       popMask  = reverse [(-i) >= offset | i <- [1..blockSize]]
>       ramTops  = rotate offset (tops (stack s))
>       newCache = zipWith5 choose writes pushMask ramTops popMask rotated

> choose push pushBit pop popBit rot
>   | pushBit   = push
>   | popBit    = pop
>   | otherwise = rot

> example3 = tops (s5 :: Stack3 Int)

Fusion
------

If we inline the 2nd implementation into the 3rd, two fusion
possibilities arise.

  1. The list 'ramTops' is defined to be the top RAM elements rotated
     by 'offset' positions.  The values returned by 'tops' in the 2nd
     implementation are rotated right by 'n' positions, where 'n' is
     the size of the stack.  These two rotations may be fused into a single
     rotation by 'n + offset' positions.

  2. The list 'writes'' is computed by left rotating a list 'writes'
     by 'n+offset' positions.  And the list 'writes' is computed by
     right rotating the cache by 'offset' positions.  The left and
     right rotations to some extent cancel each other out; the same
     result can be obtained by simply left rotating the cache by 'n'
     positions.
  
Testing
-------

> test :: Stack s => [(Int, Block (Maybe a))] -> s a
> test = foldl f empty 
>   where f s (offset, writes) = update offset writes s

> block :: [a] -> Block (Maybe a)
> block xs = take blockSize (map Just xs ++ repeat Nothing)

> test1 :: Stack s => s Char
> test1 = test [ (3, block "one")        -- Push 3
>              , (3, block "two")        -- Push 3
>              , (5, block "three")      -- Push 5
>              , (4, block "four")       -- Push 4
>              , (4, block "five")       -- Push 4
>              , (3, block "six")        -- Push 3
>              , (-7, block "")          -- Pop 7
>              , (-5, block "12345")     -- Pop 5
>              , (1, block "ok!")        -- Pop 2, Push 3
>              , (4, block "I am")       -- Push 4
>              , (-1, block "XY")        -- Pop 2, Push 1
>              ]

Final remarks
-------------

The FPGA implementation of the Octostack closely follows the above
code for 'Stack3' - see 'CachingOctostack.hs' in the Reduceron source
code.

For high performance, the user of the stack should compute the
'offset' argument to 'update' with as little delay as possible.  This
is because the 'offset' input is connected directly to the address
busses of the internal block RAMs.  In our experience, this is where
the critical path in a design often lies.

In future, it may be helpful to draw a diagram of this stack design.
A diagram of Implementation 2 is however already available in section
4.3 of [1].

References
----------

[1] Matthew Naylor and Colin Runciman.  Widening the von Neumann
Bottlneck for Graph Reduction using an FPGA.  In Implementation and
Application of Functional Languages (Revised Selected Papers),
Freiberg, Germany, 2007.
