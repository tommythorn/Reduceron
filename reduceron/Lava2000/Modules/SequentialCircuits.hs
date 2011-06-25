module SequentialCircuits where

import Lava

----------------------------------------------------------------
-- Sequential Circuits

edge inp = change
  where
    inp'   = delay low inp
    change = xor2 (inp, inp')

toggle change = out
  where
    out' = delay low out
    out  = xor2 (change, out')

delayClk init (clk, inp) = out
  where
    out = delay init val
    val = mux (clk, (out, inp))

delayN 0 init inp = inp
delayN n init inp = out
  where
    out  = delay init rest
    rest = delayN (n-1) init inp

always inp = ok
  where
    sofar = delay high ok
    ok    = and2 (inp, sofar)

constant x = ok
  where
    init = delay high low
    same = x <==> delay zero x
    ok   = always (init <|> same)

puls n () = out
  where
    out  = delayN (n-1) low last
    last = delay high out

outputList sigs () = out
  where
    out = foldr (|->) out sigs
    
rowSeq circ inp = out
  where
    carryIn         = delay zero carryOut
    (out, carryOut) = circ (carryIn, inp)

rowSeqReset circ (reset,inp) = out
  where
    carryIn         = delay zero carry
    carry           = mux (reset, (carryOut, zero))
    (out, carryOut) = circ (carryIn, inp)

rowSeqPeriod n circ inp = out
  where
    reset = puls n ()
    out   = rowSeqReset circ (reset, inp)

----------------------------------------------------------------
-- the end.


