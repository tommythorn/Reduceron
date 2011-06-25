module Retime
  ( timeTransform
  )
 where

import Signal
import Generic
import Sequent
import Netlist

import List
  ( isPrefixOf
  )

----------------------------------------------------------------
-- time transformation

timeTransform :: (Generic a, Generic b) => (a -> b) -> ([a] -> [b])
timeTransform circ []           = []
timeTransform circ inps@(inp:_) =
    map construct
  . transStruct
  . netlist phi
  . struct
  . circ
  . symbolize tag
  $ inp
 where
  n = length inps

  phi (DelayBool ini next) =
    delay DelayBool ini next
  
  phi (DelayInt ini next) =
    delay DelayInt ini next
  
  phi (VarBool s) | tag `isPrefixOf` s =
    var (drop (length tag) s)

  phi (VarInt s) | tag `isPrefixOf` s =
    var (drop (length tag) s)

  phi s =
    take n (cycle (map symbol (zips s)))
  
  delay del ~(ini0:_) next =
    (symbol (del ini0 (last next)) : list (n-1) (init next))
  
  var s =
    map (pickSymbol s) inps

  list 0 _       = []
  list n ~(x:xs) = x : list (n-1) xs

  tag = "#retime#"

----------------------------------------------------------------
-- the end.

