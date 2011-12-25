{

min m n = case ((<=) m n) of { True -> m ; False -> n ; } ;

max m n = case ((<=) m n) of { True -> n ; False -> m ; } ;

gt  m n = case ((<=) m n) of { True -> False ; False -> True ; } ;

head (Cons x xs) = x ;

last (Cons x xs) = case null xs of {
                   True  -> x ;
                   False -> last xs ;
                   } ;

null Nil         = True ;
null (Cons x xs) = False ;

length xs = lengthAcc 0 xs;

lengthAcc acc Nil = acc;
lengthAcc acc (Cons x xs) = lengthAcc ((+) acc 1) xs;

append Nil         ys = ys ;
append (Cons x xs) ys = Cons x (append xs ys) ;

map f Nil         = Nil ;
map f (Cons x xs) = Cons (f x) (map f xs) ;

concatMap f Nil         = Nil ;
concatMap f (Cons x xs) = append (f x) (concatMap f xs) ;

elem x Nil         = False ;
elem x (Cons y ys) =
  case (==) x y of { True -> True ; False -> elem x ys ; } ;

foldr f z Nil         = z ;
foldr f z (Cons x xs) = f x (foldr f z xs) ;

filter p Nil         = Nil ;
filter p (Cons x xs) =
  case p x of { True -> Cons x (filter p xs) ; False -> filter p xs ; } ;

enumFromTo m n = 
  case (<=) m n of { True -> Cons m (enumFromTo ((+) m 1) n) ; False -> Nil ; } ;

assoc x (Cons (Pair y z) yzs) =
  case (==) x y of { True -> z ; False -> assoc x yzs ; } ;

assocm x Nil                   = Nothing ;
assocm x (Cons (Pair y z) yzs) =
  case (==) x y of { True -> Just z ; False -> assocm x yzs ; } ;

subset Nil         ys = True ;
subset (Cons x xs) ys =
  case elem x ys of { True -> subset xs ys ; False -> False ; } ;

union xs ys = foldr ins xs ys ;

ins x ys = case elem x ys of { True -> ys ; False -> Cons x ys ; } ;

histo xs = foldr histins Nil xs ;

histins x Nil           = Cons (Pair x 1) Nil ;
histins x (Cons yn yns) =
  case yn of {
  Pair y n -> case (==) x y of {
              True -> Cons (Pair y ((+) n 1)) yns ;
              False -> Cons yn (histins x yns) ;
              } ;
  } ;

sorted lt = foldr (ordins lt) Nil ;

ordins lt x Nil         = Cons x Nil ;
ordins lt x (Cons y ys) = 
  case lt x y of {
  True  -> Cons x (Cons y ys) ;
  False -> Cons y (ordins lt x ys) ;
  } ;

ends (K s a z n) = Cons a (Cons z Nil) ;

firstLetter (K s a z n) = a ;

lastLetter (K s a z n) = z ;

freqSorted ks =
  let { ft = freqTabOf ks ; } in
  Pair (sorted (decreasingFrequencyIn ft) ks) (length ft) ;

decreasingFrequencyIn ft (K s0 a x n0) (K s1 b y n1) =
  let { freq = flip assoc ft ; } in
  gt ((+) (freq a) (freq x)) ((+) (freq b) (freq y)) ;

flip f x y = f y x ;

freqTabOf ks = histo (concatMap ends ks) ;

blocked = blockedWith Nil ;

blockedWith ds Nil         = Nil ;
blockedWith ds (Cons k ks) = 
  let { dsk = union ds (ends k) ;
        eks = endsSubset dsk ;
        det = filter eks ks ;
        rest = filter (non eks) ks ; } in
  Cons k (append det (blockedWith dsk rest)) ;

non f x = case f x of { True  -> False ; False -> True ; } ;

endsSubset ds k = subset (ends k) ds ;

enKey k = K k (head k) (last k) (length k) ;

hashAssoc (Hash hs hf) = hf ;

findhash mv ks = 
  case hashes mv (length ks) ks (Hash (H Nothing Nothing Nil) Nil) of {
  Cons (Hash s f) hs -> Just f ;
  Nil                -> Nothing ;
  } ;

hashes maxval nk Nil         h = Cons h Nil ;
hashes maxval nk (Cons k ks) h =               
  concatMap (hashes maxval nk ks) (
  concatMap (insertKey nk k) (
  concatMap (assignUpto maxval (lastLetter k))
            (assignUpto maxval (firstLetter k) h))) ;

assignUpto maxval c h =
  case assocm c (hashAssoc h) of {
  Nothing -> map (assign c h) (enumFromTo 0 maxval) ;
  Just v  -> Cons h Nil ;
  } ;

insertKey nk k (Hash hs hf) =
  case hinsert nk (hash hf k) hs of {
  Nothing    -> Nil ;
  Just hsNew -> Cons (Hash hsNew hf) Nil ;
  } ;

assign c (Hash hs hf) v = Hash hs (Cons (Pair c v) hf) ;
           
hinsert nk h (H lo hi hs) =
    let { newlo = case lo of { Nothing -> h ; Just x -> min x h } ;
          newhi = case hi of { Nothing -> h ; Just x -> max x h } ;
        } in
    case elem h hs of {
    True  -> Nothing ;
    False -> case (<=) ((-) ((+) newhi 1) newlo) nk of {
             False -> Nothing ;
             True  -> Just (H (Just newlo) (Just newhi) (Cons h hs)) ;
             } ;
    } ;

hash hf (K s a z n) = (+) n ((+) (assoc a hf) (assoc z hf)) ; 

cichelli ss = case freqSorted (map enKey ss) of {
              Pair ks mv -> findhash mv (blocked ks) ;
              } ;

main =
  let {
    keywords =
      Cons "as" (
      Cons "case" (
      Cons "class" (
      Cons "data" (
      Cons "default" (
      Cons "deriving" (
      Cons "do" (
      Cons "else" (
      Cons "hiding" (
      Cons "if" (
      Cons "import" (
      Cons "in" (
      Cons "infix" (
      Cons "infixl" (
      Cons "infixr" (
      Cons "instance" (
      Cons "let" (
      Cons "module" (
      Cons "newtype" (
      Cons "of" (
      Cons "qualified" (
      Cons "then" (
      Cons "type" (
      Cons "where"
      Nil ))))))))))))))))))))))) ;
   } in
   case cichelli keywords of {
       Just hf -> emitHashFun hf ;
       Nothing -> 0 ;
       } ;

emitHashFun Nil = 0 ;
emitHashFun (Cons (Pair c n) hf) = (+) ((+) c n) (emitHashFun hf);

}
