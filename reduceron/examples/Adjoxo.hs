-- Adjudicator for noughts-and-crosses positions
-- See *.in for example inputs.
-- Colin Runciman

-- The argument to parsed is an input string, and its result
-- is the corresponding pair of position-lists for O's and X's.
-- Example:  O | O | 
--          ---+---+---
--             |   |      ==> ([1,2],[7,8])
--          ---+---+---
--           X | X | 

module Adjoxo where

data GameValue = Loss | Draw | Win

data Side = X | O

bestOf :: GameValue -> GameValue -> GameValue 
bestOf Win  v   = Win
bestOf Loss v   = v
bestOf Draw Win = Win
bestOf Draw v   = Draw

inverse :: GameValue -> GameValue
inverse Loss = Win
inverse Draw = Draw
inverse Win  = Loss

fromTo :: Int -> Int -> [Int]
fromTo a b = if a <= b then a : fromTo (a+1) b else []

cmp :: Int -> Int -> Ordering
cmp a b = if   a == b
          then EQ
          else if a <= b then LT else GT

-- Positions in the playing grid are numbered 1..9 with
-- top row 1,2,3; middle row 4,5,6; bottom row 7,8,9.
-- Ordered lists of such numbers represent O's or X's.

type Region = [Int]
type Player = Region

insert :: Int -> Region -> Region
insert x []                    = [x]
insert x xs@(y:ys) | x <= y    = x : xs
                   | True      = y : insert x ys

undef = undef

reduce f xs =
  case xs of
    [] -> undef
    x:xs' -> case xs' of
               [] -> x
               _:_ -> f x (reduce f xs')

dif :: Region -> Region -> Region
dif xs ys =
  case xs of
    [] -> []
    x:xs' -> case ys of
               [] -> xs
               y:ys' -> case cmp x y of
                          LT -> x : dif xs' ys
                          EQ ->     dif xs' ys'
                          GT ->     dif xs  ys'

{-
dif [] ys = []
dif xs [] = xs
dif xs@(x:xs') ys@(y:ys') =
  case cmp x y of
  LT -> x : dif xs' ys
  EQ ->     dif xs' ys'
  GT ->     dif xs  ys'
-}

subset :: Region -> Region -> Bool
subset xs ys = null (dif xs ys)

hasLine :: Player -> Bool
hasLine p = subset [1,2,3] p || subset [4,5,6] p || subset [7,8,9] p ||
            subset [1,4,7] p || subset [2,5,8] p || subset [3,6,9] p ||
                      subset [1,5,9] p || subset [3,5,7] p

gridFull :: Player -> Player -> Bool
gridFull ap pp = length ap + length pp == 9

-- The ap argument of analysis is a position list for the active player
-- (to move next); and the pp argument is the list for the passive player.
-- The result represents the outcome with best play on both sides.

analysis :: Player -> Player -> GameValue
analysis ap pp =
  if      hasLine pp then Loss
  else if gridFull ap pp then Draw
  else    reduce bestOf (map moveval ((fromTo 1 9 `dif` ap) `dif` pp))
  where
  moveval m = inverse (analysis pp (insert m ap))


-- The argument to adjudicate represents positions of O's and X's.
-- Its result is a short adjudicator's report.

adjudicate :: (Player, Player) -> Int
adjudicate (os,xs) =
  case cmp (length os) (length xs) of
  GT -> report (analysis xs os) X
  EQ -> if hasLine xs then report Win X
        else if hasLine os then report Win O
        else report (analysis xs os) X
  LT -> report (analysis os xs) O

report :: GameValue -> Side -> Int
report Loss p = 3 
report Win  X = 1
report Win  O = 2
report Draw _ = 0

opp :: Side -> Side
opp O = X
opp X = O

main :: Int
main = adjudicate ([],[])
