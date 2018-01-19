================================
REDUCERON MEMO 12
An algorithm for arity-reduction
Matthew N, 7 January 2009
================================

The Reduceron reads the top eight stack elements into a register file
on every clock cycle.  This register file has eight read ports,
allowing fast block-instantiation of function bodies.  The problem is
that functions must take no more than eight arguments because then a
single block-read from stack memory is no longer sufficient.  Instead,
multiple reads would be needed, and block-reads would be of little
help when instantiating an application that refers to stack elements
eight or more addresses apart.  Things become more complicated and
less efficient without the arity limitation.

In Memo 7, I described a solution using an eight-port stack --- any
eight elements can be read in parallel, not just the top eight.
Indeed, this requires that we perform multiple reads, one for each
block of template nodes to be instantiated.  Although these reads can
be pipelined (reading of the next block of needed arguments can be
done in parallel with instantiation of the current block of template
nodes), there remains the one-cycle overhead of priming the pipeline
when instantiating the first application in the body.  There is no
such overhead in the original method because we always know exactly
what stack elements we'll need, namely the top eight, so we can always
pre-fetch the arguments a clock-cycle in advance.

So it seems that the current approach works quite well assuming that,
a lot of the time, functions take eight or fewer arguments anyway.
Still, the arity limitation must be solved.  To do so, we will
transform programs so that no function takes more than eight
arguments.  This memo defines such a transformation, called
arity-reduction.

Turner's abstraction algorithm
------------------------------

It is first useful to observe that arity-reduction is indeed possible.
Given a function of 'n' arguments, Turner's abstraction algorithm
produces one of 'n-1' arguments.  In the process, auxiliary functions
such as 'S' and 'K' might be introduced, but these functions
themselves take only three and two arguments respectively.  So
Turner's algorithm can be applied repeatedly until all functions in a
program take no more than eight arguments.

The only problem is that this may result in a lot of fine-grained
reductions of 'S' and 'K' being performed when executing functions of
more than eight arguments.  The algorithm below aims to be more
efficient; the introduced combinators do not need to be so
fine-grained, and they do not need to be the same for all programs.

Dijkstra's abstraction algorithm
--------------------------------

In reaction to Turner's work on combinators, Dijkstra developed his
own abstraction algorithm.  I find Dijkstra's algorithm clearer than
Turner's, and more amenable to tweaking.  So our algorithm is based on
Dijkstra's.

Dijkstra's main idea is to annotate the interior nodes of an
applicative expression with strings of combinators, rather than
introduce combinators into the expression by additional applicative
structure --- i.e. new identifiers and application nodes.  Dijkstra's
syntax for expressions is

> data Exp = Id String | App [Com] Exp Exp | Empty
>   deriving Show

An identifier is a variable (an argument to a function) or a function
name. A Com (combinator) is an S, B or C.

> data Com = S | B | C
>   deriving Show

An application containing an empty sequence of combinators is just a
standard binary application.  An application with a sequence of n > 0
combinators and sub-expressions e_0 and e_1 can be viewed as a lambda
expression of the form

  \v_1 .. v_n -> (e_0 vs) (e_1 ws)

where vs and ws are sequences containing zero or more of the variables
v_1 .. v_n as defined by the sequence of combinators.  More
specifically, if the i^th combinator in the sequence is S then v_i is
passed down to both e_0 and e_1; if it is B then v_i is passed down
only to e_1; if it is C then v_i is passed down only to e_0.

EXAMPLE 1. For some expressions e_0 and e_1, the expression

  App [S,B,C] e_0 e_1

can be viewed as the lambda expression

  \x y z -> (e_0 x z) (e_1 x y)

EXAMPLE 2. An expression can also be 'Empty'.  For some expression e,
the expression

  App [S] e Empty

can be viewed as the lambda expression

  \x -> (e x) x

'Empty' expressions are introduced during the abstraction process.

Here is Dijkstra's abstraction algorithm, defined in Haskell.

> dijkstra :: String -> Exp -> Exp
> dijkstra x (Id y) | x == y = Empty
> dijkstra x (App coms e0 e1)
>   | x `occursIn` e0 && x `occursIn` e1 =
>       App (S:coms) (dijkstra x e0) (dijkstra x e1)
>   | x `occursIn` e0 = App (C:coms) (dijkstra x e0) e1
>   | x `occursIn` e1 = App (B:coms) e0 (dijkstra x e1)

The auxiliary function 'occursIn' is defined as you'd expect.

> occursIn :: String -> Exp -> Bool
> x `occursIn` Empty = False
> x `occursIn` (Id y) = x == y
> x `occursIn` (App coms e0 e1) = x `occursIn` e0 || x `occursIn` e1

Dijkstra assumes that the variable to be abstracted from an expression
does indeed occur in that expression.  We will later remove this
assumption.

Converting Dijkstra's Expressions to Combinator Expressions
-----------------------------------------------------------

Dijkstra's expressions aren't supported by standard functional
evaluators such as the Reduceron, so we'd like to convert them to
expressions which are.  To do this, we move the combinator annotations
to identifiers occurring in the applicative structure.  So Dijkstra's
algorithm, composed with the conversion function below, has a similar
function to Turner's algorithm.

We assume that the identifiers I, B, C, and S are defined by the
following equations.

  I x = x
  B k f g x = k f (g x)
  C k f g x = k (f x) g
  S k f g x = k (f x) (g x)
 
With a left-associative operator for function application,

> infixl @@
> (@@) :: Exp -> Exp -> Exp
> e0 @@ e1 = App [] e0 e1

the conversion is defined as follows.

> convert :: Exp -> Exp
> convert Empty = Id "I"
> convert (Id x) = Id x
> convert (App [] e0 e1) = e0 @@ e1
> convert (App cs e0 e1) = 
>   foldr (@@) (Id "I") (map (Id . show) cs) @@ convert e0 @@ convert e1

(The third equation is unnecessary, but saves symbols.)

EXAMPLE 3.  The value of the expression

> example3 = convert (App [S,B,C] (Id "e0") (Id "e1"))

is 'S (B (C I)) e0 e1' (pretty-printed).

Vectored applications
---------------------

We now modify Dijkstra's algorithm to work on vectored applications,
rather than just binary ones.  This makes combinators more
coarse-grained because they don't just say "pass left" or "pass
right", but "pass to the i^th sub-expression".

First of all, the syntax is changed as follows.

> data Exp2 = Id2 String | App2 [Dir] [Exp2]
>   deriving Show

Strings of combinators are no longer represented as lists of S, B, or
C combinators, but as lists of director strings.

> type Dir = [Bool]

The i^th element of a director string states whether or not an
argument is passed to the i^th element of the application.

EXAMPLE 4. The application

  App [S,B] e_0 e_1

is now represented by

  App2 [[True,True],[False,True]] [e_0, e_1]

That is, S is equivalent to the director string [True, True] (pass the
argument to both sub-expressions) and B is equivalent to [False, True]
(pass the argument to the right-hand sub-expression only).

Note that an all-False director string is possible, e.g. [False,
False].  This allows us to abstract a variable from an expression in
which that variable does not occur.

We no longer have an explicit constructor for empty expressions; these
can be sensibly represented as nullary applications.

> empty :: Exp2
> empty = App2 [] []

The modified abstraction algorithm is as follows.

> dijkstra2 :: String -> Exp2 -> Exp2
> dijkstra2 x (Id2 y)
>   | x == y = empty
>   | otherwise = App2 [[False]] [Id2 y]
> dijkstra2 x (App2 dirs es) = App2 (bs:dirs) es'
>   where bs = map (x `occursIn2`) es
>         es' = [if b then dijkstra2 x e else e | (e, b) <- zip es bs]

This function can indeed abstract a variable from an expression in
which that variable does not occur.

The auxiliary function 'occursIn2' is defined as you'd expect.

> occursIn2 :: String -> Exp2 -> Bool
> x `occursIn2` (Id2 y) = x == y
> x `occursIn2` (App2 dirs es) = any (x `occursIn2`) es

The Modified Conversion Algorithm
---------------------------------

We must now modify the conversion algorithm for vectored applications.
In a further effort to increase the granularity of the combinators, we
attempt to introduce a single combinator for each sequence of director
strings rather than one for each director string.  As there is a limit
on the number of arguments that a function can take, this is not
always possible.  So the algorithm below takes the arity-limit as an
argument and introduces combinators that encode chunks of director
strings, where the chunks are as large as possible without breaking
the arity-limit.

> convert2 :: Int -> Exp2 -> Exp2
> convert2 n (Id2 x) = Id2 x
> convert2 n (App2 [] []) = Id2 "I"
> convert2 n (App2 [] es) = App2 [] es
> convert2 n (App2 ds es) =
>     App2 [] (foldr apply (Id2 "I") combs : map (convert2 n) es)
>   where
>     m = n - length es - 1
>     combs = map (\ds -> App2 ds []) (groupN m ds)
>     apply a b = App2 [] [a, b]

This function assumes that variable 'm' is never less than or equal to
zero.  Equivalently, vectored applications must be shorter than the
arity-limit minus one.  This condition can be easily established in an
initial application-splitting pass.

Above, we use the syntax 'App2 ds []' to represent a combinator,
specifically the combinator defined by 'comb ds'.

> comb :: [Dir] -> (Int, Exp2)
> comb (d:ds) = (1 + n + m, body)
>   where
>     n = length d
>     m = length (d:ds)
>     body = App2 [] (Id2 "0" : args)
>     args = zipWith apply [1..n] is
>     is = map (\bs -> [i | (b,i) <- zip bs [n+1..], b]) (transpose (d:ds))
>     apply n is = App2 [] (map (Id2 . show) (n:is))
 
This function takes a sequence of director strings and generates a
combinator definition in the form of a pair whose first element is the
arity of the combinator and whose second element is the body of the
combinator.
 
EXAMPLE 5.  The expression

> example5 = comb [[True,False,True],[True,False,False]]

generates the following combinator definition (pretty-printed).

  \v0 v1 v2 v3 v4 v5 -> v0 (v1 v4 v5) v2 (v3 v4)

Remaining Questions
-------------------

How do we deal with let expressions?

Auxiliary functions
------------------

> transpose :: [[a]] -> [[a]]
> transpose [] = []
> transpose [x] = map (: []) x
> transpose (x:xs) = zipWith (:) x (transpose xs)

> groupN :: Int -> [a] -> [[a]]
> groupN n [] = []
> groupN n xs = take n xs : groupN n (drop n xs)
