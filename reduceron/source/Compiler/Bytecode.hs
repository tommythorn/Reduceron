module Compiler.Bytecode where

import Compiler.Compile
import Data.Bits

intNode    = 0
applyNode  = 1
funNode    = 2
argNode    = 3

endNode    = 4

mkIntNode x = intNode .|. (x `shiftL` 3)
mkApNode x  = applyNode .|. (x `shiftL` 3)
mkFunNode x = funNode .|. (x `shiftL` 4)
mkArgNode x = argNode .|. (x `shiftL` 3)

mkStartNode args sz spineLen = args .|. (sz `shiftL` 4)
                                    .|. (spineLen `shiftL` 14)

primNode   = 10

primEq     = primNode .|. (1 `shiftL` 4)
primNotEq  = primNode .|. (2 `shiftL` 4)
primLessEq = primNode .|. (4 `shiftL` 4)
primAdd    = primNode .|. (8 `shiftL` 4)
primSub    = primNode .|. (16 `shiftL` 4)

encode :: [CombNode] -> [Int]
encode =  map enc
  where
    enc (Start numArgs size spineLen) = mkStartNode numArgs size spineLen
    enc (Int i) = mkIntNode i
    enc (Addr i) = mkApNode i
    enc (End n) = enc n .|. endNode
    enc (PrimOp Add) = primAdd
    enc (PrimOp Sub) = primSub
    enc (PrimOp Eq) = primEq
    enc (PrimOp NotEq) = primNotEq
    enc (PrimOp LessEq) = primLessEq
    enc (FunInt i) = mkFunNode i
    enc (Arg i) = mkArgNode i
