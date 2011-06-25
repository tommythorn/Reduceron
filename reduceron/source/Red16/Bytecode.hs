module Red16.Bytecode where

import Lava
import CircLib.Common
import Red16.Widths

isInt x = inv (x !! 0) <&> inv (x !! 1)
isAp x  = (x !! 0) <&> inv (x !! 1)
isFun x = inv (x !! 0) <&> (x !! 1)
isArg x = (x !! 0) <&> (x !! 1) 
isGcNode x = (x !! 0) <&> (x !! 1) 

isFunc x = isFun x <&> inv (x !! 3)
isPrim x = isFun x <&> (x !! 3)

isEnd x = x !! 2
markEnd x y = take 2 y ++ x ++ drop 3 y
dropEnd n = reverse . drop n . reverse

getArg x = drop 3 x
getAp x = drop 3 x
apAddr x = drop 3 x
funAddr x = take 12 (drop 4 x)
intVal x = x `shre` 3
rootAddr x = take addrWidth (tail x)

trueAddr = mkFunNode (word 2)
falseAddr = mkFunNode (word 4)

mkIntNode x = dropEnd 3 ([low, low, low] ++ x)
mkBoolNode x = x ? (trueAddr, falseAddr)
mkApNode x  = [high, low, low] ++ x
mkApEndNode x  = [high, low, high] ++ x
mkFunNode x = dropEnd 4 ([low, high, low, low] ++ x)
mkAStkAddr x = [high] ++ x ++ [low, low]
mkGcNode x  = [high, high, low] ++ x

isEqOp x = x !! 4
isNeqOp x = x !! 5
isLeqOp x = x !! 6
isAddOp x = x !! 7
isSubOp x = x !! 8
isLogOp x = isEqOp x <|> isNeqOp x <|> isLeqOp x

extractStart start = (numArgs, spineLen, size)
  where
    spineLen = take 3 (drop 14 start)
    size = take 10 (drop 4 start)
    numArgs = take 4 start
