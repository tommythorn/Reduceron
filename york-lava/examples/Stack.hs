module Stack where

import Lava
import Recipe
import Monad
import List

data Stack n m =
  Stack {
    top        :: Word n
  , size       :: Word m
  , stackPush  :: Sig N1
  , stackInput :: Sig n
  , stackPop   :: Sig N1
  }

newStack init =
  do pushSig <- newSig
     popSig <- newSig
     inputSig <- newSig

     let (topElem, size) = server init (pushSig!val!vhead)
                                       (popSig!val!vhead)
                                       (inputSig!val)

     return $ Stack {
                top        = topElem
              , size       = size
              , stackPush  = pushSig
              , stackPop   = popSig
              , stackInput = inputSig
              }

server initial push pop input = (top, addr')
  where
    topInit  = if null initial then 0 else fromInteger (last initial)
    restInit = if null initial then [] else init initial

    incr     = (push <#> pop) +> vrepeat (pop <&> inv push)

    addrInit = if null restInit then 0 else genericLength restInit - 1

    addr     = addr' + incr
    addr'    = delay (fromInteger addrInit) addr

    ramIns   = RamInputs {
                   ramAddress = addr
                 , ramData    = top
                 , ramWrite   = push <&> inv pop
                 }

    ramOuts  = ram restInit Width18 ramIns

    top      = delayEn topInit (push <|> pop) (push ? (input, ramOuts))

push :: Word n -> Stack n m -> Recipe
push a s = Seq [ stackInput s <== a, stackPush s <== 1 ]

pop :: Stack n m -> Recipe
pop s = stackPop s <== 1

example :: Stack N8 N10 -> Recipe
example s =
  Seq [ Tick
      , s!pop
      , Tick
      , s!push 10
      , Tick
      , s!pop
      , Tick
      , s!pop
      , Tick
      ]

simExample = simRecipe (newStack [1,2,3]) example top
