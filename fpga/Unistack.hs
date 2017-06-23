module Unistack
  ( Unistack     -- data Unistack n m
  , newUnistack  -- :: RamAlgorithm -> New (Unistack n m)
  , size         -- :: Unistack n m -> Size
  , newSize      -- :: Unistack n m -> Size
  , setSize      -- :: Word n -> Unistack n m -> Recipe
  , topElem      -- :: Unistack n m -> [Word m]
  , newTopElem   -- :: Unistack n m -> Word m
  , push         -- :: Word m -> Unistack n m -> Recipe
  , pop          -- :: Unistack n m -> Recipe
  , index        -- :: Word n -> Unistack n m -> Recipe
  , modify       -- :: Word n -> Word m -> Unistack n m -> Recipe
  , output       -- :: Unistack n m -> Word m
  ) where

-- A value of type 'Unistack n m' is a stack supporting pushing and
-- popping of a single element of type 'Word m' per clock-cycle.
-- Pushing and popping can be done in parallel.  The maximum number of
-- elements that can be stored on the stack is '2^n'.

import Prelude hiding (Word, (.))
import Lava
import Recipe

-- Implementation

data Unistack n m =
  Unistack {
    size        :: Word n
  , newSize     :: Word n
  , doSetSize   :: Sig N1
  , setSizeVal  :: Sig n
  , pushTrigger :: Sig N1
  , popTrigger  :: Sig N1
  , input       :: Sig m
  , topElem     :: Word m
  , newTopElem  :: Word m
  , write       :: Sig N1
  , address     :: Sig n
  , datIn       :: Sig m
  , output      :: Word m
  }

newUnistack ramAlgorithm =
      do push <- newSig
         pop  <- newSig
         inp  <- newSig

         doSetSizeSig <- newSig
         setSizeValSig <- newSig

         writeSig   <- newSig
         addressSig <- newSig
         dataSig   <- newSig

         let (nt, t, sz, nsz, out) = unistack ramAlgorithm
                                              (push.val.vhead)
                                              (pop.val.vhead)
                                              (inp.val)
                                              (writeSig.val.vhead)
                                              (addressSig.val)
                                              (dataSig.val)
                                              (doSetSizeSig.val.vhead)
                                              (setSizeValSig.val)

         return $ Unistack {
                    size        = sz
                  , newSize     = nsz
                  , doSetSize   = doSetSizeSig
                  , setSizeVal  = setSizeValSig
                  , pushTrigger = push
                  , popTrigger  = pop
                  , input       = inp
                  , topElem     = t
                  , newTopElem  = nt
                  , write       = writeSig
                  , address     = addressSig
                  , datIn       = dataSig
                  , output      = out
                  }

unistack ramAlgorithm push pop input ramWr ramAddr ramDatIn
  doSetSize setSizeVal =
  (newTop, top, size, newSize, ramOutsB)
  where
    grow     = push <&> inv pop
    shrink   = pop <&> inv push
    incr     = (grow <|> shrink) +> vecOf shrink
    newSize  = size + incr
    size     = delay 0 (doSetSize ? (setSizeVal, newSize))
    ramInsA  = RamInputs {
                 ramData    = ramOuts'
               , ramAddress = newSize
               , ramWrite   = grow
               }
    ramInsB  = RamInputs {
                 ramData    = ramDatIn
               , ramAddress = ramAddr
               , ramWrite   = ramWr
               }
    (ramOutsA, ramOutsB) = dualRam [] "unistack_" ramAlgorithm (ramInsA, ramInsB)
    ramOuts' = delayEn 0 (grow <|> shrink) $
                 pickG [ shrink --> ramOutsA
                       , grow --> top
                       ]
    newTop   = pickG [ shrink --> ramOuts'
                     , push --> input
                     , inv (shrink <|> push) --> top
                     ]
    top      = delay 0 newTop

push :: Word m -> Unistack n m -> Recipe
push a s = do s.input <== a; s.pushTrigger <== 1

pop :: Unistack n m -> Recipe
pop s = s.popTrigger <== 1

index :: Word n -> Unistack n m -> Recipe
index addr s = s.address <== addr

modify :: Word n -> Word m -> Unistack n m -> Recipe
modify addr val s = do s.address <== addr; s.datIn <== val; s.write <== 1

setSize :: Word n -> Unistack n m -> Recipe
setSize sz s = do s.doSetSize <== 1; s.setSizeVal <== sz

-- Example usage

example :: Unistack N10 N18 -> Recipe
example s = do s.push 10
               tick
               tick
               s.push 2
               tick
               tick
               tick
               s.push 3
               tick
               tick
               s.pop
               tick
               tick
               s.push 5
               tick
               tick
               s.pop
               tick
               s.pop
               tick


simExample = simRecipe (newUnistack Width18) example newTopElem
