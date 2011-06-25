module Compiler.Interp where

import Compiler.Compile
import Compiler.State
import Control.Monad
import qualified Data.IntMap as IntMap

data Result = Res { result :: Int
                  , heapUsage :: Int
                  }
              deriving Show

type Eval a = State EvalState a

data EvalState = ES { heap :: IntMap.IntMap CombNode
                    , hp   :: Int
                    }

readHeap i = do s <- get ; return (heap s IntMap.! i)

writeHeap i x = modify (\s -> s { heap = IntMap.insert i x (heap s) })

hpGet = do s <- get ; return (hp s)

hpSet a = modify (\s -> s { hp = a })

hpInc = modify (\s -> s { hp = hp s + 1 })

write x = do s <- get ; writeHeap (hp s) x ; hpInc

interp :: [CombNode] -> Result
interp prog = Res { result = a, heapUsage = hp s }
  where
    base   = length prog + 1
    (s, a) = runState (eval [FunInt 0]) (ES mem base)
    mem    = IntMap.fromList (zip [0..] prog)

eval :: [CombNode] -> Eval Int
eval [Int i] = return i
eval (Int i:a:s) = eval (a:Int i:s)
eval (PrimOp Add:Int a:Int b:s) = eval (Int (a+b):s)
eval (PrimOp Sub:Int a:Int b:s) = eval (Int (a-b):s)
eval (PrimOp Eq:Int a:Int b:t:f:s)
  | a == b    = eval (t:s)
  | otherwise = eval (f:s)
eval (PrimOp NotEq:Int a:Int b:t:f:s)
  | a == b    = eval (f:s)
  | otherwise = eval (t:s)
eval (PrimOp LessEq:Int a:Int b:t:f:s)
  | a <= b    = eval (t:s)
  | otherwise = eval (f:s)
eval (Addr i:s) =  do top <- unwind i ; eval (reverse top ++ s)
eval (FunInt i:s) = do s' <- unfold s i ; eval s'
eval n = error (show n)

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
