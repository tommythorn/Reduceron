{

inc i  =  (+) i 1 ;

dec i  =  (-) i 1 ;

dis True  q  =  True ;
dis False q  =  q ;

con True  q  =  q ;
con False q  =  False ;

not True   =  False ;
not False  =  True ;

non f x  =  not (f x) ;

isNothing Nothing   =  True ;
isNothing (Just x)  =  False ;

isJust Nothing   =  False ;
isJust (Just x)  =  True ;

fromJust (Just x)  =  x ;

maybe n j Nothing   =  n ;
maybe n j (Just x)  =  j x ; 

fst (Pair x y)  =  x ;

snd (Pair x y)  =  y ;

id x  =  x ;

curry f x y  =  f (Pair x y) ;

uncurry f (Pair x y)  =  f x y ;

flip f x y  =  f y x ;

cross f g (Pair x y)  =  Pair (f x) (g y) ;

both p (Pair x y)  =  con (p x) (p y) ;

null Nil          =  True ;
null (Cons x xs)  =  False ;

head (Cons x xs)  =  x ;

tail (Cons x xs)  =  xs ;

append Nil         ys  =  ys ;
append (Cons x xs) ys  =  Cons x (append xs ys) ;

map f Nil          =  Nil ;
map f (Cons x xs)  =  Cons (f x) (map f xs) ;

concatMap f Nil          =  Nil ;
concatMap f (Cons x xs)  =  append (f x) (concatMap f xs) ;

elemBy e x Nil          =  False ;
elemBy e x (Cons y ys)  =  case e x y of { True -> True ; False -> elemBy e x ys ; } ;

elemAt (Cons x xs) n  =  case (==) n 0 of {
                         True  -> x ;
                         False -> elemAt xs (dec n) ;
                         } ;

filter p Nil          =  Nil ;
filter p (Cons x xs)  =  case p x of {
                         True  -> Cons x (filter p xs) ;
                         False -> filter p xs ;
                         } ;

foldr f z Nil         = z ;
foldr f z (Cons x xs) = f x (foldr f z xs) ;

zipWith f Nil         Nil          =  Nil ;
zipWith f Nil         (Cons y ys)  =  Nil ;
zipWith f (Cons x xs) Nil          =  Nil ;
zipWith f (Cons x xs) (Cons y ys)  =  Cons (f x y) (zipWith f xs ys) ;

zip  =  zipWith Pair ;

unlines Nil          =  "" ;
unlines (Cons s ss)  =  append s (Cons '\n' (unlines ss)) ;

emitStr Nil         k  =  k ;
emitStr (Cons c cs) k  =  emit c (emitStr cs k) ;

length Nil          =  0 ;
length (Cons x xs)  =  inc (length xs) ;

repeat x  =  Cons x (repeat x) ;

last (Cons x xs)  =  case null xs of {
                     True  -> x ;
                     False -> last xs ;
                     } ;

enumFromTo m n  =  case (<=) m n of {
                   True  -> Cons m (enumFromTo (inc m) n) ;
                   False -> Nil ;
                   } ;

plus a b = (+) a b;

sum xs  =  foldr plus 0 xs ;

concatStrings ss  =  foldr append "" ss ;

and xs  =  foldr con True xs ;

all p xs  =  and (map p xs) ; 

unionBy e xs ys  =  foldr (insBy e) xs ys ;

insBy e x ys  =  case elemBy e x ys of { True -> ys ; False -> Cons x ys ; } ;

intersperse i Nil                   =  Nil ;
intersperse i (Cons x Nil)          =  Cons x Nil ;
intersperse i (Cons x (Cons y ys))  =  Cons x (Cons i (intersperse i (Cons y ys))) ;

nubBy e xs  =  nubBySans Nil e xs ;

nubBySans ss e Nil          =  Nil ;
nubBySans ss e (Cons x xs)  =  case elemBy e x ss of {
                               True  -> nubBySans ss e xs ;
                               False -> Cons x (nubBySans (Cons x ss) e xs) ;
                               } ; 

lookUpBy e x Nil                    =  Nothing ;
lookUpBy e x (Cons (Pair y v) yvs)  =  case e x y of {
                                       True  -> Just v ;
                                       False -> lookUpBy e x yvs ;
                                       } ;
 
equalStrings Nil         Nil          =  True ;
equalStrings Nil         (Cons y ys)  =  False ;
equalStrings (Cons x xs) Nil          =  False ;
equalStrings (Cons x xs) (Cons y ys)  =  con ((==) x y) (equalStrings xs ys) ;

showResult (Just trs) sig  =  append "\nSuccess\n" (showRules sig (map rn trs)) ;
showResult Nothing    sig  =  "\nFailure\n" ;

varWeight (Weights vw fws)  =  vw ;

funWeight (Weights vw fws) f  =  fromJust (lookUpBy equalStrings f fws) ;

funSequence (Weights vw fws)  =  map fst fws ;

weights  =  Weights 1 (Cons (Pair "0" 1) (Cons (Pair "+" 0) (Cons (Pair "-" 0) Nil))) ;

order = kbGreaterThan weights ;

arity sig f  =  maybe ((-) 0 1) fst (lookUpBy equalStrings f sig) ;

isInfix sig f  =  maybe False snd (lookUpBy equalStrings f sig) ;
       

main  =  
  let {
    sig  =  Cons (Pair "+" (Pair 2 True))  (
            Cons (Pair "-" (Pair 1 False)) (
            Cons (Pair "0" (Pair 0 False)) Nil )) ;

    eqns  =  Cons (Pair (Fun "+" (Cons (Fun "0" Nil) (Cons (Var "X") Nil)))
                        (Var "X")) (
             Cons (Pair (Fun "+" (Cons (Fun "-" (Cons (Var "X") Nil))
                                 (Cons (Var "X") Nil) ))
                        (Fun "0" Nil)) (
             Cons (Pair (Fun "+" (Cons (Fun "+" (Cons (Var "X")
                                                (Cons (Var "Y") Nil)))
                                 (Cons (Var "Z") Nil) ))
                        (Fun "+" (Cons (Var "X")
                                 (Cons (Fun "+" (Cons (Var "Y")
                                                (Cons (Var "Z") Nil))) Nil) )))
             Nil )) ;
    } in
  case checkEquations eqns sig of {
  	 True  -> emitStr "Using equations:\n"
                (emitStr (showEqns sig eqns)
                   (emitStr (showResult (complete sig eqns) sig) 0)) ;
  	 False -> emitStr "Ill-formed equation\n" 0 ;
         } ;

checkEquations es sig  =  all (both (checkTerm sig)) es ;

checkTerm sig (Var v)     =  True ;
checkTerm sig (Fun f ts)  =  con ((==) (length ts) (arity sig f)) 
                                 (all (checkTerm sig) ts) ;

rn (Pair l r)  =  let { sub = zip (vars l) (map Var variables); }
                  in  Pair (subst sub l) (subst sub r) ;

zipWithRemRight f Nil         Nil          =  Pair Nil Nil ;
zipWithRemRight f Nil         (Cons y ys)  =  Pair Nil (Cons y ys) ;
zipWithRemRight f (Cons x xs) Nil          =  Pair Nil Nil ;
zipWithRemRight f (Cons x xs) (Cons y ys)  =  
  case zipWithRemRight f xs ys of {
  Pair rest rem -> Pair (Cons (f x y) rest) rem ;
  } ;

renameNew vs (Pair l r)  =  
  case zipWithRemRight (curry (cross id Var)) (vars l) vs of {
  Pair sub rest -> Pair (Pair (subst sub l) (subst sub r)) rest ;
  } ;

renameNewList vs Nil          =  Pair Nil vs ;
renameNewList vs (Cons r rs)  =  
  case renameNew vs r of {
  Pair renamedr remainingvs ->
    case renameNewList remainingvs rs of {
    Pair rest remainder -> Pair (Cons renamedr rest) remainder ;
    } ;
  } ;

complete sig trs = completionLoop sig variables 0 trs Nil ;

completionLoop sig vs n Nil           rules  =  Just rules ;
completionLoop sig vs n (Cons p rest) rules  =
  case (==) n 1000 of {
  True  -> Nothing ;
  False -> case renameNew vs p of {
           Pair newEqn ws ->
             case orient newEqn of {
               Nothing      -> Nothing ;
               Just newRule ->
                 completionWith sig ws (inc n) rest newRule rules ;
             } ;
           } ;
  } ;

completionWith sig vs n rest newRule rules =
  let { cps  =  getCriticalPairs (Cons newRule rules) newRule ; }
  in  case simplifyRules sig newRule rules of {
	    Pair eqs rls -> completionLoop sig vs n 
      	                (simplifyEquations sig
                                           (Cons newRule rules)
                                           (append rest (append eqs cps)))
                        (uniqueRules rls) ;
      } ;

orient (Pair t1 t2)  =  case order t1 t2 of {
                        True  -> Just (Pair t1 t2) ;
		                    False -> case order t2 t1 of {
                                 True  -> Just (Pair t2 t1) ;
		                             False -> Nothing ;
                                 } ;
                        } ;
		
getCriticalPairs trs r  =
		append (selfCriticalPairs trs r) (concatMap (duoCriticalPairs trs r) trs) ;

simplifyRules sig rule trs  = 
  case reduceSplit sig rule trs of {
  Pair newEqns remainingRules -> 
	  Pair newEqns
         (uniqueRules (map (normRhs (Cons rule remainingRules))
                           (Cons rule remainingRules) )) ;
  } ;

normRhs trs (Pair l r)  =  Pair l (norm trs r) ;

simplifyEquations sig trs eqns  = 
  filter (non (uncurry equalTerms))
         (uniqueRules (map (normEqn trs) eqns)) ;

normEqn trs (Pair l r) = Pair (norm trs l) (norm trs r) ;
		
reduceSplit sig rule Nil                   =  Pair Nil Nil  ;
reduceSplit sig rule (Cons (Pair l r) rs)  = 
	let { reducedl  =  reduce (Cons rule Nil) l ; } in
  case reduceSplit sig rule rs of {
  Pair eqns rules -> 
  	case dis (null reducedl)
             (reducible (Cons (Pair l r) Nil) (left rule)) of {
  	True  -> Pair eqns
                  (Cons (Pair l r) rules) ;
  	False -> Pair (Cons (Pair (head reducedl) r) eqns)
                  rules ;
    } ;
  } ;

selfCriticalPairs trs (Pair l1 r1) = 
  criticalPairs trs False (Pair l1 r1) (rename (Pair l1 r1)) ;

duoCriticalPairs trs (Pair l1 r1) (Pair l2 r2)  = 
  append (criticalPairs trs True  (Pair l1 r1) (Pair l2 r2))
         (criticalPairs trs False (Pair l2 r2) (Pair l1 r1)) ;

criticalPairs trs allowRootPos (Pair l1 r1) (Pair l2 r2) =
  let { ps = case allowRootPos of {
             True  -> positions l1 ;
             False -> filter (non null) (positions l1) ;
             } ;
      }
  in  concatMap (criticalPairsAt trs (Pair l1 r1) (Pair l2 r2)) ps ;

criticalPairsAt trs (Pair l1 r1) (Pair l2 r2) p =
  let { t  =  subterm l1 p ; }
  in  case isVar t of {
      True  -> Nil ;
      False -> case unify t l2 of {
               Nothing  -> Nil ;
               Just sub -> criticalPairsFrom
                             (norm trs (subst sub r1))
                             (norm trs (placeAt (subst sub r2) p (subst sub l1))) ;
               } ;
      } ;

criticalPairsFrom cpl cpr  =  case equalTerms cpl cpr of {
                              True  -> Nil ;
                              False -> Cons (Pair cpl cpr) Nil ;
                              } ;

rename (Pair l r)  =  
	let { oldVars  =  vars l ;
	      newVars  =  map Var (filter (non (flip (elemBy equalStrings) oldVars)) variables) ;
	      sub      =  zip oldVars newVars ; }
  in  Pair (subst sub l) (subst sub r) ;

leq a b = (<=) a b;

eq a b = (==) a b;

kbGreaterThan w t s  =
	let { vs  =  unionBy equalStrings (vars t) (vars s) ;
	      wt  =  termWeight w t ;
      	ws  =  termWeight w s ;
      	nt  =  map snd (varCounts t vs) ;
      	ns  =  map snd (varCounts s vs) ; }
  in dis (con (not ((<=) wt ws)) (compareAll leq ns nt))
	       (con (con ((==) wt ws) (compareAll eq nt ns))
		          (dis (powerOf (last (funSequence w)) t s)
                   (funcAfter w t s) )) ;

termWeight w (Var v)     =  varWeight w ;
termWeight w (Fun f ts)  =  (+) (funWeight w f) (sum (map (termWeight w) ts)) ;

powerOf fn (Var v)             u  =  False ;
powerOf fn (Fun f Nil)         u  =  False ;
powerOf fn (Fun f (Cons x xs)) u  =  con (con (isVar u) (equalStrings f fn))
                                         (con (null xs) (pow fn x)) ;

pow fn (Var v)              =  True ;
pow fn (Fun f (Cons z zs))  =  con (equalStrings f fn)
                                   (con (null zs) (pow fn z)) ;
	
funcAfter w (Var v)    (Var x)     =  False ;
funcAfter w (Var v)    (Fun g us)  =  False ;
funcAfter w (Fun f ts) (Var x)     =  False ;
funcAfter w (Fun f ts) (Fun g us)  = 
	case equalStrings f g of {
  True  -> orderLex (kbGreaterThan w) ts us ;
  False -> before (funSequence w) g f ;
  } ;
	
compareAll f xs ys  =  and (zipWith f xs ys) ;

varCounts t vs  =  foldr tally (zip vs (repeat 0)) (concatMap (varAt t) (positions t)) ;

varAt t p  =  case subterm t p of {
              Var v ->  Cons v Nil ;
              Fun f ts -> Nil ;
              } ;

tally k Nil                    =  Nil ;
tally k (Cons (Pair x nx) xs)  =  case equalStrings k x of {
                                  True  -> Cons (Pair x (inc nx)) xs ;
                                  False -> Cons (Pair x nx) (tally k xs) ;
                                  } ;

orderLex f Nil         Nil          =  False ;
orderLex f Nil         (Cons y ys)  =  False ;
orderLex f (Cons x xs) Nil          =  False ;
orderLex f (Cons x xs) (Cons y ys)  =  case equalTerms x y of {
                                       True  -> orderLex f xs ys ;
                                       False -> f x y ;
                                       } ;

before (Cons h t) x y  =  dis (equalStrings h x) 
	                            (con (not (equalStrings h y))
	                                 (before t x y)) ;

showTerm sig (Var v)     =  v ;
showTerm sig (Fun f ts)  = 
	case isInfix sig f of {
  True  -> append "("
           ( append (showTerm sig (elemAt ts 0))
           ( append " " (append f (append " "
           ( append (showTerm sig (elemAt ts 1))
           ")" ))))) ;
	False -> case null ts of {
           True  -> f ;
           False -> append f
                    ( append "("
                    ( append (concatStrings (intersperse "," (map (showTerm sig) ts)))
                    ")" )) ;
           } ;
  } ;

showEqn sig (Pair x y)  =  append (showTerm sig x) (append " = " (showTerm sig y)) ; 

showEqns sig eqns  =  unlines (map (showEqn sig) eqns) ;

showRule sig (Pair x y)  =  append (showTerm sig x) (append " -> " (showTerm sig y)) ; 

showRules sig rules  =  unlines (map (showRule sig) rules) ;

left  =  fst ;

right  =  snd ;

uniqueRules  =  nubBy equivPair ;

equivPair (Pair a b) (Pair c d)  = 
  dis (con (equiv a c) (equiv b d)) (con (equiv a d) (equiv b c)) ; 

unify t1 t2  =  unifyWith (Cons t1 Nil) (Cons t2 Nil) Nil ;

unifyWith Nil                Nil          sub  =  Just sub ;
unifyWith (Cons (Var v) ts1) (Cons t ts2) sub  =
	let { ti   =  subst sub t ;
        vi   =  subst sub (Var v) ;
        mgu  =  unify vi ti ; } in
	case equalTerms t (Var v) of {
	True  -> unifyWith ts1 ts2 sub ;
	False -> case equalTerms vi (Var v) of {
           True  ->  case elemBy equalStrings v (vars ti) of {
                     True  -> Nothing ;
                     False -> unifyWith ts1 ts2 (substAdd (Cons (Pair v ti) Nil) sub) ;
                     } ;
           False ->  case isJust mgu of {
                     True  -> unifyWith ts1 ts2 (substAdd (fromJust mgu) sub) ;
                     False -> Nothing ;
                     } ;
           } ;
  } ;
unifyWith (Cons (Fun f ts) ts1) (Cons t ts2) sub  =
  case t of {
  Var v    -> unifyWith (Cons (Var v) ts2) (Cons (Fun f ts) ts1) sub ;
  Fun g us -> case equalStrings f g of {
	            True  -> unifyWith (append ts ts1) (append us ts2) sub ;
             	False -> Nothing ;
              } ;
  } ;

substAdd subNew subOld  =  append subNew (map (cross id (subst subNew)) subOld) ;

match t1 t2  =  matchWith (Cons t1 Nil) (Cons t2 Nil) Nil ;

matchWith Nil                Nil          sub  =  Just sub ;
matchWith (Cons (Var v) ts1) (Cons t ts2) sub  =
	case elemBy equalStrings v (map fst sub) of {
  True  -> case equalTerms t (subst sub (Var v)) of {
           True  -> matchWith ts1 ts2 sub ;
           False -> Nothing ;
           } ;
	False -> matchWith ts1 ts2 (Cons (Pair v t) sub) ;
  } ;
matchWith (Cons (Fun f ts) ts1) (Cons t ts2) sub  =
  case t of {
  Var v    -> Nothing ;
  Fun g us -> case equalStrings f g of {
	            True  -> matchWith (append ts ts1) (append us ts2) sub ;
	            False -> Nothing ;
              } ;
  } ;

normalForms trs t  = 
	let { reduced = reduce trs t ; } in
	case null reduced of {
  True  -> Cons t Nil ;
	False -> concatMap (normalForms trs) reduced ;
  } ;

norm trs t  =  head (normalForms trs t) ;

reducible trs t  =  not (null (reduce trs t)) ;

reduce trs t  =  concatMap (reduceAt trs t) (positions t) ;

reduceAt trs t p  =
  case isVar (subterm t p) of {
  True  -> Nil ;
  False -> concatMap (reduceAtWith t p) trs ;
  } ;

reduceAtWith t p (Pair l r)  =
  case match l (subterm t p) of {
  Nothing -> Nil ;
  Just sub -> Cons (placeAt (subst sub r) p t) Nil ;
  } ;

isVar (Var v)     =  True ;
isVar (Fun f ts)  =  False ;

subst s (Var v)     =  case s of {
                       Nil                  -> Var v ;
                       Cons (Pair w t) rest -> case equalStrings v w of {
                                               True  -> t ;
                                               False -> subst rest (Var v) ;
                                               } ;
                       } ;
subst s (Fun f ts)  =  Fun f (map (subst s) ts) ; 

equalTerms (Var v)    (Var w)     =  equalStrings v w ;
equalTerms (Var v)    (Fun g us)  =  False ;
equalTerms (Fun f ts) (Var w)     =  False ;
equalTerms (Fun f ts) (Fun g us)  =  con (equalStrings f g)
                                         (and (zipWith equalTerms ts us)) ;

applyToIndex f n (Cons x xs)  =  case (==) n 0 of {
                                 True -> Cons (f x) xs ;
                                 False -> Cons x (applyToIndex f (dec n) xs) ;
                                 } ;

equiv t u  =  con (isJust (match t u)) (isJust (match u t)) ;

subterm t Nil              =  t ;
subterm t (Cons p1 pRest)  =  case t of {
                              Fun f ts -> subterm (elemAt ts (dec p1)) pRest ;
                              } ;

placeAt t Nil            u  =  t ;
placeAt t (Cons p pRest) u  =  case u of {
                               Fun f ts ->
                                 Fun f (applyToIndex (placeAt t pRest) (dec p) ts) ;
                               } ; 

vars (Var v)     =  Cons v Nil ;
vars (Fun f ts)  =  foldr (unionBy equalStrings) Nil (map vars ts) ;

positions (Var v)     =  Cons Nil Nil ;
positions (Fun f ts)  =  Cons Nil (concatMap (argPositions ts)
                                             (enumFromTo 1 (length ts)) ) ; 

argPositions ts n  =  map (Cons n) (positions (elemAt ts (dec n))) ;

variables  =  let { variety = Cons "" (concatMap variations variety) ; }
              in  tail variety ;

variations v  =  map (flip Cons v) "ABCDEFGHIJKLMNOPQRSTUVWXYZ" ;

}

