module Red7.Bytecode where

import Lava
import CircLib.Common

isIntNode x = inv (x !! 0) <&> inv (x !! 1)
isApNode x  = (x !! 0) <&> inv (x !! 1)
isFunNode x = inv (x !! 0) <&> (x !! 1)
isArgNode x = (x !! 0) <&> (x !! 1) 
isGcNode x = (x !! 0) <&> (x !! 1) 

isPrim x = x !! 3

isEnd x = x !! 2

markEnd x y = take 2 y ++ x ++ drop 3 y

unEnd x = take 2 x ++ [low] ++ drop 3 x

getArg x = drop 3 x

getAp x = drop 3 x

getAddr x = dropEnd 2 (tail x)

dropEnd n = reverse . drop n . reverse

mkIntNode x = dropEnd 3 ([low, low, low] ++ x)
mkIntEndNode x = dropEnd 3 ([low, low, high] ++ x)
mkApNode x  = [high, low, low] ++ x
mkApEndNode x  = [high, low, high] ++ x
mkFunNode x = dropEnd 4 ([low, high, low, low] ++ x)
mkAStkAddr x = [high] ++ x ++ [low, low]
mkGcNode x  = [high, high, low] ++ x


isEq x = x !! 4
isNotEq x = x !! 5
isLessEq x = x !! 6
isAdd x = x !! 7
isSub x = x !! 8
