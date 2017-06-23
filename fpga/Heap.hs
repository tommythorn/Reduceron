module Heap
  ( Heap       -- data Heap n m
  , newHeap    -- :: RamAlgorithm -> New (Heap n m)
  , size       -- :: Heap n m -> Word n
  , size1      -- :: Heap n m -> Word n
  , lookupA    -- :: Word n -> Heap n m -> Recipe
  , lookupB    -- :: Word n -> Heap n m -> Recipe
  , outputA    -- :: Heap n m -> Word m
  , outputB    -- :: Heap n m -> Word m
  , updateA    -- :: Word n -> Word m -> Heap n m -> Recipe
  , updateB    -- :: Word n -> Word m -> Heap n m -> Recipe
  , snocA      -- :: Word m -> Heap n m -> Recipe
  , snocB      -- :: Word m -> Heap n m -> Recipe
  , snocA'     -- :: Word m -> Heap n m -> Recipe
  , snocB'     -- :: Word m -> Heap n m -> Recipe
  , advanceA   -- :: Heap n m -> Recipe
  , advanceB   -- :: Heap n m -> Recipe
  , reset      -- :: Heap n m -> Recipe
  ) where

-- A value of type 'Heap n m' is a dual-port random-access memory
-- storing '2^n' elements elements of type 'Word m'.  A heap also has
-- a size associated with it, and provides 'snoc' operations for
-- appending elements onto the end.  Two snocs (one on each port) can
-- be performed in one clock-cycle, with the effect that the 'snocA'
-- occurs before the 'snocB'.  Currently, a 'snocB' must only ever be
-- performed in parallel with a 'snocA', never alone.

import Prelude hiding (Word, (.))
import Lava
import Recipe

data Heap n m =
  Heap {
    outputA  :: Word m
  , outputB  :: Word m
  , inputA   :: Sig m
  , inputB   :: Sig m
  , addressA :: Sig n
  , addressB :: Sig n
  , writeA   :: Sig N1
  , writeB   :: Sig N1
  , incA     :: Sig N1
  , incB     :: Sig N1
  , size     :: Word n
  , size1    :: Word n
  , doReset  :: Sig N1
  }

newHeap :: (N n, N m) => RamAlgorithm -> String -> New (Heap (S (S n)) m)
newHeap ramAlgorithm annotation =
  do sigInputA   <- newSig
     sigInputB   <- newSig
     sigAddressA <- newSig
     sigAddressB <- newSig
     sigWriteA   <- newSig
     sigWriteB   <- newSig
     sigIncA     <- newSig
     sigIncB     <- newSig
     sigReset    <- newSig


     let ramInsA = RamInputs {
                     ramData    = val sigInputA
                   , ramAddress = val sigAddressA
                   , ramWrite   = vhead (val sigWriteA)
                   }

     let ramInsB = RamInputs {
                     ramData    = val sigInputB
                   , ramAddress = val sigAddressB
                   , ramWrite   = vhead (val sigWriteB)
                   }

     let (ramOutsA, ramOutsB) = dualRam [] ("heap_" ++ annotation) ramAlgorithm (ramInsA, ramInsB)

     let incA = vhead (val sigIncA)
     let incB = vhead (val sigIncB)
     let inc  = (incA <#> incB) +> (incA <&> incB) +> vecOf low

     let sz   = zeroIf (sigReset.val.vhead) (sz' + inc)
         sz'  = delay 0 sz

     let sz1  = oneIf (sigReset.val.vhead) (sz1' + inc)
         sz1' = delay 1 sz1

     return $ Heap {
                outputA  = ramOutsA
              , outputB  = ramOutsB
              , inputA   = sigInputA
              , inputB   = sigInputB
              , addressA = sigAddressA
              , addressB = sigAddressB
              , writeA   = sigWriteA
              , writeB   = sigWriteB
              , incA     = sigIncA
              , incB     = sigIncB
              , size     = sz'
              , size1    = sz1'
              , doReset  = sigReset
              }

lookupA :: Word n -> Heap n m -> Recipe
lookupA a h = h.addressA <== a

lookupB :: Word n -> Heap n m -> Recipe
lookupB a h = h.addressB <== a

updateA :: Word n -> Word m -> Heap n m -> Recipe
updateA a x h = do h.addressA <== a
                   h.inputA <== x
                   h.writeA <== 1

updateB :: Word n -> Word m -> Heap n m -> Recipe
updateB a x h = do h.addressB <== a
                   h.inputB <== x
                   h.writeB <== 1

snocA :: Word m -> Heap n m -> Recipe
snocA x h = do h.incA <== 1
               h.updateA (h.size) x

snocB :: Word m -> Heap n m -> Recipe
snocB x h = do h.incB <== 1
               h.updateB (h.size1) x

snocA' :: Word m -> Heap n m -> Recipe
snocA' x h = h.updateA (h.size) x

snocB' :: Word m -> Heap n m -> Recipe
snocB' x h = h.updateB (h.size1) x

advanceA :: Heap n m -> Recipe
advanceA h = h.incA <== 1

advanceB :: Heap n m -> Recipe
advanceB h = h.incB <== 1

reset :: Heap n m -> Recipe
reset h = h.doReset <== 1

-- Axuliaries

zeroIf :: Bit -> Word n -> Word n
zeroIf b = vmap (inv b <&>)

oneIf :: Bit -> Word (S n) -> Word (S n)
oneIf b x = (b <|> vhead x) +> zeroIf b (vtail x)
