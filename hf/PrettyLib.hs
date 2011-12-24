{-# OPTIONS -fno-warn-incomplete-patterns #-}
{-
A simple pretty printer combinator library.

A pretty printer converts an inductively defined data structure (i.e. a tree)
into a text (i.e. a list of lines).
Indentation of lines reflects the tree form of the data structure.
However, to produce only a minimal number of lines substructures are put
on a single line as far as possible.

The combinators provided here enable simple definition of a pretty printer 
for any tree structure.
The pretty printer requires time linear in the size of the data structure
and space linear in the desired maximal width of the output.
The interface of the library is based on:
  Philip Wadler: A prettier printer, 
  http://cm.bell-labs.com/cm/cs/who/wadler/topics/recent.html
The implementation uses some ideas from
  Derek C Oppen: Prettyprinting, TOPLAS volume 2 number 4, ACM, 1980, 465-483.
(Wadler's implementation performs some backtracking and hence may require time
linear in both the size of the data structure and the maximal width.)
-}
module PrettyLib (nil,(<>),text,line,delimiter,fdelimiter,group,nest,groupNest
                 ,Doc
                 ,parens,brackets,braces,string
                 ,pretty,simple) where


{- Exported definitions ================================================== -}
-- Core pretty printer combinators ------------------------------------------

infixr 6 <>

nil :: Doc
nil = NIL

(<>) :: Doc -> Doc -> Doc
(<>) = (:<>)

text :: String -> Doc
text = TEXT

delimiter :: String -> Doc
delimiter = DELIMITER

{- 
Fill line, that is, 
only make newline if part up to next possible newline does not fit on line.
-}
fdelimiter :: String -> Doc
fdelimiter = FDELIMITER

{-
Force line break. The surrounding groups cannot fit on one line.
-}
line :: Doc
line = LINE

group :: Doc -> Doc
group = GROUP

nest :: Int -> Doc -> Doc
nest = NEST

-- Derived pretty printer combinators ---------------------------------------

{-
The often used combination of group and nest.
-}
groupNest :: Int -> Doc -> Doc
groupNest indentation doc = group (nest indentation doc)

parens :: Doc -> Doc
parens doc = text "(" <> doc <> text ")"

brackets :: Doc -> Doc
brackets doc = text "[" <> doc <> text "]"

braces :: Doc -> Doc
braces doc = text "{" <> doc <> text "}"

string :: String -> Doc
string s = text ('\"' : s ++ "\"")

{- Optimally pretty print the document within the given line width. -}
pretty :: Int -> Doc -> String
pretty width = layout width . normalise . removeFDelimiter . flatten 

{- Never turn a delimiter into a newline. -}

simple :: Doc -> String
simple = token2String . flatten


{- Implementation ========================================================= -}

data Doc = NIL | Doc :<> Doc 
         | TEXT String | DELIMITER String | FDELIMITER String | LINE 
         | GROUP Doc | NEST Int Doc
         deriving Show


data Token = Text String Int 
           | Delimiter String Int Int | FDelimiter String Int Int | Line Int 
           | Open | Close
           deriving Show


{- 
Staightforward conversion of tokens into a string,
ignoring brackets and possiblity of turning delimiters into newlines.
-}
token2String :: [Token] -> String

token2String [] = ""
token2String (Text s _ : tokens) = s ++ token2String tokens
token2String (Delimiter s _ _ : tokens) = s ++ token2String tokens
token2String (FDelimiter s _ _ : tokens) = s ++ token2String tokens
token2String (Line i : tokens) = '\n' : replicate i ' ' ++ token2String tokens
token2String (Open : tokens) = token2String tokens
token2String (Close : tokens) = token2String tokens
 

{-
Convert the tree structure of a document into a stream of tokens.
A group is repesented by an opening and a closing bracket.
-}
flatten :: Doc -> [Token]

flatten doc0 = go 0 doc0 []
  where
  go :: Int -> Doc -> [Token] -> [Token]
  {- use accumulator to perform flattening in linear time -}
  go _i NIL rest = rest
  go i (d1 :<> d2) rest = go i d1 (go i d2 rest)
  go _i (TEXT s) rest = Text s (length s) : rest
  go i (DELIMITER s) rest = Delimiter s (length s) i : rest
  go i (FDELIMITER s) rest = FDelimiter s (length s) i : rest
  go i LINE rest = Line i : rest
  go i (GROUP doc) rest = Open : go i doc (Close : rest)
  go i (NEST j doc) rest = go (i+j) doc rest


{-
Replace FDelimiter s l i by Open, Delimiter s l i, ..., Close.
So that Close is in front of the next (F)Delimiter s l i of the
same group or the next Close or the end.
-}
removeFDelimiter :: [Token] -> [Token]

removeFDelimiter = go 0 [] 
  where
  {-
  Invariants: elements of stack are strictly sorted, largest on top
              nesting depth is greater or equal top element of stack (if ex.)
  -}
  go :: Int {- nesting depth of brackets; without new ones -}
     -> [Int] {- stack for closing brackets that still have to be inserted -}
     -> [Token] {- stream of tokens with FDelimiter -}
     -> [Token] {- stream of tokens without FDelimiter -}
  go _ [] [] = [] -- first argument should be 0
  go _ _  [] = [Close] -- the stack can at most contain one entry
  go d toClose (Open : tokens) = Open : go (d+1) toClose tokens
  go d toClose (Close : tokens) = 
    possiblyClose (Close :) (subtract 1) id d toClose tokens
  go d toClose (FDelimiter s l i : tokens) =
    possiblyClose (\x -> Open : Delimiter s l i : x) id (d:) d toClose tokens
  go d toClose (Delimiter s l i : tokens) =
    possiblyClose (Delimiter s l i :) id id d toClose tokens
  go d toClose (Line i : tokens) =
    possiblyClose (Line i :) id id d toClose tokens
  go d toClose (text0 : tokens) = text0 : go d toClose tokens

  possiblyClose :: ([Token] -> [Token])  -- what to put into output stream
                -> (Int -> Int)          -- how to change nesting depth
                -> ([Int] -> [Int])      -- how to change stack
                -> Int                   -- nesting depth
                -> [Int]                 -- stack
                -> [Token]
                -> [Token]
  possiblyClose modOut modDepth modStack d (close : toClose) tokens
    | close == d = Close : modOut (go (modDepth d) (modStack toClose) tokens) 
  possiblyClose modOut modDepth modStack d toClose tokens =
    modOut (go (modDepth d) (modStack toClose) tokens)
  

{-
Normalise a stream of tokens wrt the following rewriting rules
Open, Text s l  ==> Text s l, Open
Close, Text s l ==> Text s l, Close
Then any block of brackets is followed by a Line s l i or the end of the stream
-}
normalise :: [Token] -> [Token]

normalise tokens0 = go 0 0 tokens0
  where
  go :: Int {- number of deferred closing brackets -} 
     -> Int {- number of deferred opening brackets -} 
     -> [Token] {- stream of tokens to normalise -}
     -> [Token] {- normalised stream of tokens -}
  go i _ [] = replicate i Close -- there should be no deferred opening brackets
  go i j (Open : tokens) = go i (j+1) tokens
  go i j (Close : tokens)
    | j == 0    = go (i+1) j tokens
    | otherwise = go i (j-1) tokens
  go i j (deli@(Delimiter _ _ _) : tokens) =
    replicate i Close ++ replicate j Open ++ deli : go 0 0 tokens
  go i j (line0@(Line _) : tokens) =
    replicate i Close ++ replicate j Open ++ line0 : go 0 0 tokens
  go i j (text0 : tokens) = text0 : go i j tokens
 

{-
The list of tokens is optimally pretty printed within the given width.
Precondition: between a `Close' and a `Text s l' is always a `Line s l i'.
For efficiency also better if no `Open' is directly followed by `Text s l'.
A normalised token list has this property.
-}
layout :: Int {- width of layout -}
       -> [Token] 
       -> String

layout width tokens0 = snd $ go width 1 empty1 0 tokens0
  where
  go :: Int -- space remaining in current line (in characters)
     -> Int -- current stream position (in characters)
     -> Q1 Int -- queue of maximal streamposition for end of group to fit
     -> Int -- how deep inside a fitting group (<= 0: not in fitting group)
     -> [Token]
     -> (Q2 Bool -- queue which states if groups fit
        ,String) -- optimally layouted output
  go _ _ _groupsMaxEnd _ [] = (empty2, "")
  go r p groupsMaxEnd fitDepth (Open : tokens) = (groupFits', string0)
    where
    (groupsMaxEnd', fits', groupFits') = cons (r+p) groupsMaxEnd groupsFits
    (groupsFits, string0) = go r p groupsMaxEnd' fitDepth' tokens
    fitDepth' = if fitDepth > 0 then succ fitDepth else if fits' then 1 else 0
  go r p groupsMaxEnd fitDepth (Close : tokens) 
    | isEmpty1 groupsMaxEnd = go r p groupsMaxEnd (pred fitDepth) tokens
    | otherwise = (groupFits', string0)
    where
    (_, groupsMaxEnd', groupFits') = lview groupsMaxEnd True groupFits
    (groupFits, string0) = go r p groupsMaxEnd' (pred fitDepth) tokens
  go r p groupsMaxEnd fitDepth (Delimiter s l i : tokens)  = 
    (groupFits', output ++ string0)
    where
    (output, newRemainingSpace) 
      | fitDepth > 0 = (s, r-l)
      | otherwise    = ('\n' : replicate i ' ', width-i)
    (groupFits', string0) = 
      checkGroupsMaxEnd newRemainingSpace (p+l) groupsMaxEnd fitDepth tokens 
  go _r p groupsMaxEnd fitDepth (Line i : tokens) =
    -- a compulsary newline makes all surrounding blocks not to fit
    (map1To2 (\_ -> False) groupsMaxEnd, '\n' : replicate i ' ' ++ string0)
    where
    (_, string0) = go (width-i) p empty1 fitDepth tokens
  go r p groupsMaxEnd fitDepth (Text s l : tokens) = (groupFits', s ++ string0)
    where
    (groupFits', string0) = 
      checkGroupsMaxEnd (r-l) (p+l) groupsMaxEnd fitDepth tokens 


  checkGroupsMaxEnd :: Int -> Int -> Q1 Int -> Int -> [Token] 
                    -> (Q2 Bool, String)
  checkGroupsMaxEnd r p groupsMaxEnd fitDepth tokens 
    | isEmpty1 groupsMaxEnd || p <= maxEnd = 
      go r p groupsMaxEnd fitDepth tokens
    | otherwise = (groupsFits', string0)
    where
    (groupsMaxEnd', maxEnd, groupsFits') = rview groupsMaxEnd groupsFits False
    (groupsFits, string0) = checkGroupsMaxEnd r p groupsMaxEnd' fitDepth tokens


{- Special double ended queues -------------------------------------------- 

Two related kinds of double ended queue types are defined.
A queue operation always operates on two queues, one of each kind.
Each operation performs an operation on one queue and exactly the inverse
operation on the other queue.
For example, an operation that splits one queue into its first element and
the remaining queue, also puts an element in front of the other queue to obtain
a longer queue.

The inverse operations are very lazy.
Let there be a sequence of queue operations such that the first queue 
resulting from an operation is always the input first queue to the 
*next* operation and the second queue resulting from an operation is always 
the input second queue to the *preceding* operation.
Let the first operation `cons' an element to the front of the first queue 
and the last operation remove exactly this element from the first queue 
(either front or rear). Then the last operation adds another element to 
the second queue and the first operation removes exactly this element.
The operations on the second queue are so lazy that for obtaining this 
element the second queue which is input to the last operation is not
demanded, that is, does not need to be evaluated.

Note that the inverse operations only work correctly, if the two queues
are passed in such a sequence. The implementation does not only assume
that the two queues handled by an operation have the same number of elements,
but also that they have the same internal structure.

Using a monad to ascertain correct passing of queues is possible but 
seems rather restrictive and hides the idea. However, maybe another
kind of sequencing interface would be good to ensure safe usage.

            element                            element
               v                                  ^
  queue1  -> +----+ -> +---------+ ->     -> +---------+ ->
             |cons|    |operation|    ...    |operation|
          <- +----+ <- +---------+ <-     <- +---------+ <- queue2
               v                                  ^
            element'                           element'

Except for the additional lazy inverse operations the implementation agrees 
with the Banker's queue given in:
  Chris Okasaki: Purely Functional Data Structures, CUP, 1998, Section 8.4.2 
-}

{-
q12List (Q1 _ f _ r) = f ++ reverse r
Furthermore, the structure contains the lengths of the two lists
Q2 does not contain lengths
-}

data Q1 a = Q1 !Int [a] !Int [a] deriving Show
data Q2 a = Q2 [a] [a] deriving Show


reverse1 :: Q1 a -> Q1 a
reverse1 (Q1 lenf f lenr r) = Q1 lenr r lenf f

reverse2 :: Q2 a -> Q2 a
reverse2 (Q2 f r) = Q2 r f


empty1 :: Q1 a
empty1 = Q1 0 [] 0 []

empty2 :: Q2 a
empty2 = Q2 [] []


isEmpty1 :: Q1 t -> Bool
isEmpty1 (Q1 lenf _ lenr _) = lenf + lenr == 0 


map1To2 :: (a -> b) -> Q1 a -> Q2 b
map1To2 g (Q1 _ f _ r) = Q2 (map g f) (map g r)


{-
Keep lenghts of the two lists in balance
-}

check :: Int -> [a] -> Int -> [a] -> Q2 b -> (Q1 a, [b], [b])

check lenf f lenr r q2 =
  if lenf > balanceConstant * lenr + 1 then
    let
      len = lenf + lenr
      lenf' = len `div` 2
      lenr' = len - lenf'
      (f', rf') = splitAt lenf' f
      (r2, rf2) = lsplitAt lenr r2'
    in (Q1 lenf' f' lenr' (r ++ reverse rf')
       ,lappend lenf' f2' (lreverse (lenr'-lenr) rf2)
       ,r2)
   else
    (Q1 lenf f lenr r, f2', r2')
  where
  Q2 f2' r2' = q2
  balanceConstant = 3 :: Int


{-
Put an element in front of the first list.
Inversely, split second list into front element and tail queue.
-}
cons :: a -> Q1 a -> Q2 b -> (Q1 a, b, Q2 b)

cons x (Q1 lenf f lenr r) q2' = (q', head f2, Q2 (tail f2) r2)
  where
  (q', f2, r2) = check (lenf+1) (x:f) lenr r q2'


{-
Split first list into initial queue and rear element.
Inversely, add an element to rear of second list.
-}
rview :: Q1 a -> Q2 b -> b -> (Q1 a, a, Q2 b)

rview (Q1 _ (x:_) _ []) _q2' y = (empty1, x, Q2 [y] [])
rview (Q1 _ [] _ []) _ _ = error "empty queue"
rview (Q1 lenf f lenr (x:r)) q2' y = (q', x, Q2 f2 (y:r2))
  where
  (q', f2, r2) = check lenf f (lenr-1) r q2'


{-
Split first list into first element and tail queue.
Inversely, add an element to the front of the second list.
-}
lview :: Q1 a -> b -> Q2 b -> (a, Q1 a, Q2 b)

lview q1 y q2 = (x, reverse1 q1', reverse2 q2')
  where
  (q1', x, q2') = rview (reverse1 q1) (reverse2 q2) y


{-
Some very lazy variants of standard list functions.
They get the length of the result or a list argument as first argument and 
thus are able to construct the list structure of the result without demanding
evaluation of any of its other arguments.
Demanding some list element of the result will naturally lead to 
more demand of the arguments.
-}


{-
The first argument gives the length of the argument/result list.
-}
lreverse :: Int -> [a] -> [a]

lreverse n0 xs0 = lreverseAcc n0 xs0 []
  where
  lreverseAcc 0 _ acc = acc
  lreverseAcc n xs acc = lreverseAcc (n-1) ys (y:acc)
    where
    y:ys = xs

{-
The first argument gives the length of the first list argument.
-}
lappend :: Int -> [a] -> [a] -> [a]

lappend 0 _ zs = zs
lappend n xs zs = y : lappend (n-1) ys zs
  where
  y:ys = xs


{-
The first argument gives the position at which the input list shall be split.
The list must be at least that long.
-}
lsplitAt :: Int -> [a] -> ([a], [a])
 
lsplitAt 0 xs = ([],xs)
lsplitAt n ys = (x:xs',xs'') 
  where 
  x:xs = ys
  (xs',xs'') = lsplitAt (n-1) xs

{- End PrettyLib ========================================================== -}
