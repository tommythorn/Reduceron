{

id x = x ;

const c x = c ;

inc n = (+) n 1 ;

dec n = (-) n 1 ;

min x y = case (<=) x y of { True -> x ; False -> y ; } ;

max x y = case (<=) x y of { True -> y ; False -> x ; } ;

abs n = case (<=) 0 n of { True  -> n ; False -> (-) 0 n ; } ;

plus a b = (+) a b;

minus a b = (-) a b;

no Nothing = True ;
no (Just x) = False ;

maybe n j Nothing  = n ;
maybe n j (Just x) = j x ; 

con True  q = q ;
con False q = False ;

dis True  q = True ;
dis False q = q ;

fst (Pair x y) = x ;

snd (Pair x y) = y ;

cross (Pair f g) (Pair x y) = Pair (f x) (g y) ;

null Nil         = True ;
null (Cons x xs) = False ;

append Nil         ys = ys ;
append (Cons x xs) ys = Cons x (append xs ys) ;

elemAt (Cons x xs) n =
  case (==) n 0 of { True -> x ; False -> elemAt xs ((-) n 1) ; } ;

map f Nil = Nil ;
map f (Cons x xs) = Cons (f x) (map f xs) ;

concatMap f Nil = Nil ;
concatMap f (Cons x xs) = append (f x) (concatMap f xs) ;

any p Nil         = False ;
any p (Cons x xs) = dis (p x) (any p xs) ;

foldr f z Nil         = z ;
foldr f z (Cons x xs) = f x (foldr f z xs) ;

sum xs = foldr plus 0 xs ;

unzip Nil                   = Pair Nil Nil ;
unzip (Cons (Pair x y) xys) =
  let { u = unzip xys ; } in  Pair (Cons x (fst u)) (Cons y (snd u)) ;

kindToChar k =
	case k of {
	King	  -> 'K' ;
	Queen	  -> 'Q' ;
	Rook	  -> 'R' ;
	Bishop	-> 'B' ;
	Knight	-> 'N' ;
	Pawn	  -> 'P' ;
  } ;

isKing k = (==) (kindToChar k) 'K' ;

pieceAt (Board wkss bkss) sq =
  pieceAtWith sq White (pieceAtWith sq Black Nothing bkss) wkss ;

pieceAtWith sq c n Nil = n ;
pieceAtWith sq c n (Cons (Pair k s) xs) =
  case sameSquare s sq of {
  True -> Just (Pair c k) ;
  False -> pieceAtWith sq c n xs ;
  } ;

emptyAtAll (Board wkss bkss) e =
	emptyAtAllAnd e (emptyAtAllAnd e True bkss) wkss ;

emptyAtAllAnd e b Nil                  = b ;
emptyAtAllAnd e b (Cons (Pair k s) xs) =
  case e s of { True -> False ; False -> emptyAtAllAnd e b xs ; } ;

rmPieceAt White sq (Board wkss bkss) = Board (rPa sq wkss) bkss ;
rmPieceAt Black sq (Board wkss bkss) = Board wkss (rPa sq bkss) ;

rPa sq (Cons ks kss) = 
  case ks of {
  Pair k s ->
    case sameSquare s sq of { True -> kss ; False -> Cons ks (rPa sq kss) ; } ;
  } ;

putPieceAt sq (Pair c k) (Board wkss bkss) =
  case c of {
  White -> Board (Cons (Pair k sq) wkss) bkss ;
  Black -> Board wkss (Cons (Pair k sq) bkss) ;
  } ;

kingSquare c b = kSq (forcesColoured c b) ;

kSq (Cons (Pair k s) kss) =
  case isKing k of { True  -> s ; False -> kSq kss ; } ;

opponent Black = White ;
opponent White = Black ;

colourOf (Pair c k) = c ;
kindOf   (Pair c k) = k ;

sameColour White White = True ;
sameColour White Black = False ;
sameColour Black White = False ;
sameColour Black Black = True ;

rank (Pair f r) = r ;
file (Pair f r) = f ;

sameSquare (Pair f1 r1) (Pair f2 r2) = con ((==) f1 f2) ((==) r1 r2) ;

onboard (Pair p q) =
  con (con ((<=) 1 p) ((<=) p 8))
      (con ((<=) 1 q) ((<=) q 8)) ;

forcesColoured White (Board wkss bkss) = wkss ;
forcesColoured Black (Board wkss bkss) = bkss ;

problem =
  Pair
    ( Board
      (Cons (Pair Knight (Pair 7 8))
      (Cons (Pair Rook   (Pair 5 7))
      (Cons (Pair King   (Pair 8 7))
      (Cons (Pair Bishop (Pair 4 5))
      (Cons (Pair Pawn   (Pair 8 4))
      (Cons (Pair Pawn   (Pair 7 3))
      (Cons (Pair Pawn   (Pair 5 2))
      (Cons (Pair Pawn   (Pair 6 2))
      (Cons (Pair Queen  (Pair 5 1))
      Nil)))))))))
      (Cons (Pair Knight (Pair 2 8))
      (Cons (Pair Pawn   (Pair 7 7))
      (Cons (Pair Pawn   (Pair 4 6))
      (Cons (Pair Pawn   (Pair 3 5))
      (Cons (Pair King   (Pair 6 5))
      (Cons (Pair Pawn   (Pair 8 5))
      (Cons (Pair Pawn   (Pair 4 4))
      (Cons (Pair Pawn   (Pair 2 3))
      (Cons (Pair Pawn   (Pair 5 3))
      (Cons (Pair Pawn   (Pair 7 2))
      (Cons (Pair Queen  (Pair 1 1))
      (Cons (Pair Knight (Pair 2 1))
      (Cons (Pair Bishop (Pair 8 1))
      Nil)))))))))))))
    )
    (Pair White 3) ;

moveDetailsFor c bd =
  concatMap (movesForPiece c bd) (forcesColoured c bd) ;

movesForPiece c bd p =
  concatMap (tryMove c bd p) (rawmoves c p bd) ;

tryMove c bd (Pair k sqFrom) (Move sqTo mcp mpp) =
	let { p   = Pair c k ;
        bd1 =	rmPieceAt c sqFrom bd ;
        pp  =	maybe p id mpp ;
        bd2 =	maybe (putPieceAt sqTo pp bd1)
		                (const (putPieceAt sqTo pp
                             (rmPieceAt (opponent c) sqTo bd1)))
		                mcp ; }
 	in case kingincheck c bd2 of {
     False -> Cons (Pair (MoveInFull p sqFrom (Move sqTo mcp mpp)) bd2) Nil ;
	   True  -> Nil ;
     } ;

rawmoves c (Pair k sq) bd = 
	let { m = case k of {
	          King   -> kingmoves   ;
	          Queen  -> queenmoves  ;
	          Rook   -> rookmoves   ;
	          Bishop -> bishopmoves ;
	          Knight -> knightmoves ;
	          Pawn   -> pawnmoves   ;
            } ; }
  in m c sq bd ;

bishopmoves c sq bd =
	append (moveLine bd c sq (cross (Pair dec inc))) (
	append (moveLine bd c sq (cross (Pair inc inc))) (
	append (moveLine bd c sq (cross (Pair dec dec)))
	       (moveLine bd c sq (cross (Pair inc dec))) )) ;

rookmoves c sq bd =
	append (moveLine bd c sq (cross (Pair dec id))) (
	append (moveLine bd c sq (cross (Pair inc id))) (
	append (moveLine bd c sq (cross (Pair id dec))) 
	       (moveLine bd c sq (cross (Pair id inc))) )) ;

moveLine bd c sq inc = 
	let { incsq = inc sq ; } in
	case onboard incsq of {
  True -> case pieceAt bd incsq of {
		      Nothing -> Cons (Move incsq Nothing Nothing)
                          (moveLine bd c incsq inc) ;
		      Just p  -> case sameColour (colourOf p) c of {
				             False -> Cons (Move incsq (Just p) Nothing) Nil ;
                     True  -> Nil ;
                     } ;
          } ;
	False -> Nil ;
  } ;

kingmoves c (Pair p q) bd =
  let { pi = (+) p 1 ; pd = (-) p 1 ; qi = (+) q 1 ; qd = (-) q 1 ; }
	in sift c bd Nil
       (Cons (Pair pd qi) (Cons (Pair p qi) (Cons (Pair pi qi)
       (Cons (Pair pd q )                   (Cons (Pair pi q )
       (Cons (Pair pd qd) (Cons (Pair p qd) (Cons (Pair pi qd)
       Nil)))))))) ;

knightmoves c (Pair p q) bd =
  let {pi  = (+) p 1 ; pd  = (-) p 1 ; qi  = (+) q 1 ; qd  = (-) q 1 ;
       pi2 = (+) p 2 ; pd2 = (-) p 2 ; qi2 = (+) q 2 ; qd2 = (-) q 2 ; }
	in sift c bd Nil
                 (Cons (Pair pd qi2) (Cons (Pair pi qi2)
       (Cons (Pair pd2 qi)                     (Cons (Pair pi2 qi)
       (Cons (Pair pd2 qd)                     (Cons (Pair pi2 qd)
                 (Cons (Pair pd qd2) (Cons (Pair pi qd2)
       Nil)))))))) ;

sift c bd ms Nil           = ms ;
sift c bd ms (Cons sq sqs) =
	case onboard sq of {
	False -> sift c bd ms sqs ;
  True  ->
    case pieceAt bd sq of {
    Nothing -> sift c bd (Cons (Move sq Nothing Nothing) ms) sqs ;
    Just p  -> case sameColour (colourOf p) c of {
               True  -> sift c bd ms sqs ;
               False -> sift c bd (Cons (Move sq (Just p) Nothing) ms) sqs ;
               } ;
    } ;
  } ;

pawnmoves c (Pair p q) bd =
  let { fwd  = case c of {	White -> 1 ; Black -> (-) 0 1 ; } ; 
        on1  = Pair p ((+) q fwd) ;
        on2  = Pair p ((+) ((+) q fwd) fwd) ;
        mov2 = case con (secondRank c q) (no (pieceAt bd on2)) of {
               True -> Cons (Move on2 Nothing Nothing) Nil ; 
      			   False -> Nil ;
               } ;
      	movs = case no (pieceAt bd on1) of {
               True ->
            		 append
                   (promote c on1 Nothing)
        			     mov2 ;
            	 False -> 
                 Nil ;
               } ;
        dii  = Pair ((+) p 1) ((+) q fwd) ;
        did  = Pair ((-) p 1) ((+) q fwd) ;
        caps = append (promoteCap c dii bd) (promoteCap c did bd) ; }
  in append movs caps ;

promoteCap c sq bd =
  let { mcp = pieceAt bd sq ; } in
  case mcp of {
  Nothing  -> Nil;
  Just p   -> case sameColour (colourOf p) c of {
              False -> promote c sq mcp ;
              True  -> Nil ;
              } ;
  } ;

promote c sq mcp =  
	case lastRank c (rank sq) of {
  True  -> map (Move sq mcp)
		       (Cons (Just (Pair c Queen))
           (Cons (Just (Pair c Rook))
           (Cons (Just (Pair c Bishop))
           (Cons (Just (Pair c Knight)) Nil)))) ;
	False -> Cons (Move sq mcp Nothing) Nil ;
  } ;

secondRank White r = (==) r 2 ;
secondRank Black r = (==) r 7 ;

lastRank White r = (==) r 8 ;
lastRank Black r = (==) r 1 ;

queenmoves c sq bd = append (bishopmoves c sq bd) (rookmoves c sq bd) ;

kingincheck c bd =
	any (kingInCheckFrom c bd) (forcesColoured (opponent c) bd) ;

kingInCheckFrom c bd (Pair f (Pair x y)) =
  case kingSquare c bd of {
  Pair xk yk -> 
    case f of {
		King   -> con ((<=) (abs ((-) x xk)) 1)
                  ((<=) (abs ((-) y yk)) 1) ;
		Queen  -> dis (kingInCheckFrom c bd (Pair Rook   (Pair x y)))
                  (kingInCheckFrom c bd (Pair Bishop (Pair x y))) ;
		Rook   -> dis (con ((==) x xk)
                       (emptyAtAll bd (filePath xk y yk)))
                  (con ((==) y yk)
                       (emptyAtAll bd (rankPath yk x xk))) ;
		Bishop -> dis (con ((==) ((-) x y) ((-) xk yk))
                       (emptyAtAll bd (diagPath minus ((-) xk yk) x xk)))
                  (con ((==) ((+) x y) ((+) xk yk))
                       (emptyAtAll bd (diagPath plus ((+) xk yk) x xk))) ;
		Knight -> dis (con ((==) (abs ((-) x xk)) 2) ((==) (abs ((-) y yk)) 1))
                  (con ((==) (abs ((-) x xk)) 1) ((==) (abs ((-) y yk)) 2)) ;
		Pawn   -> con ((==) (abs ((-) x xk)) 1)
			            ((==) yk (onFor c y )) ;
    } ;
  } ;

onFor Black = inc ;
onFor White = dec ;

filePath xk yFrom yTo (Pair x y) =
  let { ylo = (+) (min yFrom yTo) 1 ; yhi = (-) (max yFrom yTo) 1 ; }
  in  con ((==) x xk) (con ((<=) ylo y) ((<=) y yhi)) ; 

rankPath yk xFrom xTo (Pair x y) =
  let { xlo = (+) (min xFrom xTo) 1 ; xhi = (-) (max xFrom xTo) 1 ; }
  in  con ((==) y yk) (con ((<=) xlo x) ((<=) x xhi)) ; 

diagPath op d xFrom xTo (Pair x y) =
  let { xlo = (+) (min xFrom xTo) 1 ; xhi = (-) (max xFrom xTo) 1 ; }
  in  con ((==) (op x y) d) (con ((<=) xlo x) ((<=) x xhi)) ; 

solve bd c n = showResult (solution bd c ((-) ((+) n n) 1)) ;

solution bd c n  = 
  let { mds = moveDetailsFor c bd ; } in
	foldr (solnOr c n) Nothing mds ;

solnOr c n (Pair mif b) other =
	case replies b (opponent c) ((-) n 1) of {
	Nothing -> other ;
	Just rs -> case null rs of {
             True -> case kingincheck (opponent c) b of {
                     True  -> Just (Solution mif Nil) ;
		                 False -> other ;
                     } ;
             False -> Just (Solution mif rs) ;
             } ;
  } ;

replies bd c n =
  let { mds = moveDetailsFor c bd ; } in
  case (==) n 0 of {
  True  -> case null mds of { True -> Just Nil ; False -> Nothing ; } ;
	False -> foldr (solnAnd c n) (Just Nil) mds ;
  } ;

solnAnd c n (Pair mif b) rest =
	case solution b (opponent c) ((-) n 1) of {
	Nothing -> Nothing ;
	Just s  -> case rest of {
			       Nothing -> Nothing ;
			       Just ms -> Just (Cons (Pair mif s) ms) ;
             } ;
  } ;

emitStr Nil k = k;
emitStr (Cons x xs) k = emit x (emitStr xs k);

showResult Nothing  = emitStr "No solution!\n" 0 ;
showResult (Just s) = emitStr "Solved!  Solution size = "
                             (emitInt (size s) (emit '\n' 0)) ;

size (Solution mif rs) = (+) 1 (sum (map size (snd (unzip rs)))) ;

main = solveProblem problem ;

solveProblem (Pair bd (Pair c n)) = solve bd c n ;

}
