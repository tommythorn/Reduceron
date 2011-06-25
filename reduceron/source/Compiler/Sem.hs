module Compiler.Sem where

import Compiler.Compile

data Result = Res { result :: Int
                  , heapUsage :: Int
                  }
              deriving Show

interp :: [CombNode] -> Result
interp prog = Res { result = red (prog, [FunInt 0], [0]), heapUsage = 0 }

-- Aux

true = FunInt 2
false = FunInt 4

prim Add a b = Int (a + b)
prim Sub a b = Int (a - b)
prim Eq a b = if a == b then true else false
prim NotEq a b = if a /= b then true else false
prim LessEq a b = if a <= b then true else false

upd h i x = zipWith (\y a -> if i == a then x else y) h [0..]

-- Semantics

{-
red :: [CombNode] -> [CombNode] -> [Int] -> Int
red h [Int i] a = i
red h (Int i:x:s) a = red h (x:Int i:s) a
red h (PrimOp p:Int x:Int y:s) (_:_:r:a) = red (upd h r (End z)) (z:s) (r:a)
  where
    z = prim p x y
red h (Addr i:s) (_:a) = unwind h i s a
red h (FunInt i:s) a = unwind h' spine (drop nargs s) a'
  where
    Start nargs size splen = (h !! i)
    nodes = take size (drop (i+1) h)
    hp = length h
    spine = hp + size - splen
    h' = upd h r (End (Addr spine)) ++ map (inst s hp) nodes
    (r:a') = drop nargs a


unwind h i s a = red h (reverse ap ++ s) (reverse as ++ a)
  where
    ap = getAp (drop i h)
    as = map (i +) [0 .. length ap - 1]
-}

red (h, [Int i], a) = i
red (h, s, a) = red (step (h, s, a))

type State = ([CombNode], [CombNode], [Int])

-- Should A-Stack always contain 1 less than stack?

step :: State -> State
step (h, Int i:x:s, a) =
     (h, x:Int i:s, a)
step (h, PrimOp p:Int x:Int y:s, _:_:r:a) =
     (upd h r (End z), z:s, r:a)
  where
     z = prim p x y
step (h, Addr i:s, _:a) = unwind i (h, s, a)
step (h, FunInt i:s, a) = unwind sa (h', s', a')
  where
    Start nargs size splen = h !! i   -- remove need for splen
    body   = take size (drop (i+1) h)
    hp     = length h
    sa     = hp + size - splen
    s'     = drop nargs s
    (r:a') = drop nargs a
    h'     = upd h r (End (Addr sa)) ++ map (inst s hp) body
    
inst :: [CombNode] -> Int -> CombNode -> CombNode
inst s b (Arg i) = s !! i
inst s b (Addr i) = Addr (b+i-1)
inst s b (End n) = End (inst s b n)
inst s b n = n

unwind i (h, s, a) = (h, reverse ap ++ s, reverse as ++ a)
  where
    ap = getAp (drop i h)
    as = map (i +) [0 .. length ap - 1]

getAp (End n:ns) = [n]
getAp (n:ns) = n : getAp ns

{-
unwind :: Int -> Eval [CombNode]
unwind i = do a <- readHeap i
              case a of
                End x -> return [x]
                _     -> liftM (a:) (unwind (i+1))

unfold :: [CombNode] -> Int -> Eval [CombNode]
unfold s loc = do st <- get
                  let base = hp st
                  Start numArgs size spineLen <- readHeap loc
                  copy size (loc+1) base
                  let lastAddr = base + size - spineLen
                  top <- unwind lastAddr
                  let s' = reverse top ++ drop numArgs s
                  hpSet lastAddr
                  return s'
  where
    copy 0 _ b = return ()
    copy n i b = do x <- readHeap i
                    case unEnd x of
                      Arg j  -> write (mkEnd x (s !! j))
                      Addr j -> write (mkEnd x (Addr (b + j - 1)))
                      _      -> write x
                    copy (n-1) (i+1) b

    mkEnd (End _) x = End x
    mkEnd _ x       = x

    unEnd (End x) = x
    unEnd x       = x

    isEnd (End x) = True
    isEnd _       = False
-}
