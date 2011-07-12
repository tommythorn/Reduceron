{-# OPTIONS_GHC -fcontext-stack=1024 #-}

module Bytecode where

import Lava
import Data.Bits

{-

Pointer tagging scheme:

  +------------------+-----------------------------------------+
  | Tag (LSB first)  | Contents                                |
  +------------------+-----------------------------------------+
  | 000              | Function arity and address              |
  | 001              | Primitive function arity and identifier |
  | 010              | Pointer to application                  |
  | 011              | Pointer to shared application           |
  | 100              | Primitive integer                       |
  | 101              | Constructor arity (+1) and index        |
  | 110              | Reference to function argument          |
  | 111              | Reference to register                   |
  +------------------+-----------------------------------------+

Assumptions:

  * Max function arity = 7
  * Max constructor arity = 6

-}

type HeapAddrN    = N15
type HeapAddr     = Word HeapAddrN
type StackAddrN   = N13
type StackAddr    = Word StackAddrN
type ArityN       = N3
type Arity        = Word ArityN
type FunAddrN     = N10
type FunAddr      = Word FunAddrN
type ToSpaceAddrN = N14
type ToSpaceAddr  = Word ToSpaceAddrN

-- Atoms

atomN      = 18 :: Int
type AtomN = N18
type Atom  = Word AtomN

isFUN :: Atom -> Bit
isFUN a = inv (a `vat` n0) <&> inv (a `vat` n1)

funArity :: Atom -> Arity
funArity = vtake n3 . vdrop n3

funAddr :: Atom -> FunAddr
funAddr = vtake n10 . vdrop n6

funFirst :: Atom -> Bit
funFirst = vlast

makeFUN :: Bit -> Word N3 -> FunAddr -> Atom
makeFUN b n a = low +> low +> low +> (n <++> a <++> (low +> b +> vempty))

encodeFUN :: Bool -> Integer -> Integer -> Integer
encodeFUN b n a
  | n >= 0 && n <= 7 =
     (n `shiftL` 3) .|. (a `shiftL` 6) .|. (boolToNum b `shiftL` 17)
  | otherwise = error "encodeFUN: invalid arguments"

getSwapBit :: Atom -> Bit
getSwapBit a = a `vat` n6

invSwapBit :: Atom -> Atom
invSwapBit a = vtake n6 a <++> vsingle (inv (a `vat` n6)) <++> vdrop n7 a

isADD, isSUB, isEQ, isNEQ, isLEQ :: Atom -> Bit
isADD a = a `vat` n7
isSUB a = a `vat` n8
isEQ  a = a `vat` n9
isNEQ a = a `vat` n10
isLEQ a = a `vat` n11

b2i :: Bool -> Integer
b2i False = 0
b2i True = 1

primADD, primSUB, primEQ, primNEQ, primLEQ :: Bool -> Integer
primADD s = 4 .|. (2 `shiftL` 3) .|. ((b2i s) `shiftL` 6) .|. (1 `shiftL` 7)
primSUB s = 4 .|. (2 `shiftL` 3) .|. ((b2i s) `shiftL` 6) .|. (1 `shiftL` 8)
primEQ  s = 4 .|. (2 `shiftL` 3) .|. ((b2i s) `shiftL` 6) .|. (1 `shiftL` 9)
primNEQ s = 4 .|. (2 `shiftL` 3) .|. ((b2i s) `shiftL` 6) .|. (1 `shiftL` 10)
primLEQ s = 4 .|. (2 `shiftL` 3) .|. ((b2i s) `shiftL` 6) .|. (1 `shiftL` 11)

isAP :: Atom -> Bit
isAP a = inv (a `vat` n0) <&> (a `vat` n1)

isShared :: Atom -> Bit
isShared a = a `vat` n2

pointer :: Atom -> HeapAddr
pointer a = vdrop n3 a

makeAP :: Bit -> HeapAddr -> Atom
makeAP s a = low +> high +> s +> a

encodeAP :: Bool -> Integer -> Integer
encodeAP s a = 2 .|. (boolToNum s `shiftL` 2) .|. (b `shiftL` 3)
  where b = if a < 0 then a+(2^15) else a

dash :: Bit -> Atom -> Atom
dash s a = vtake n2 a <++> vsingle shared <++> vdrop n3 a
  where shared = (isAP a <&> s) <|> (a `vat` n2)

isINT :: Atom -> Bit
isINT a = (a `vat` n0) <&> inv (a `vat` n1) <&> inv (a `vat` n2)

intValue :: Atom -> HeapAddr
intValue a = vdrop n3 a

makeINT :: HeapAddr -> Atom
makeINT a = high +> low +> low +> a

encodeINT :: Integer -> Integer
encodeINT a = 1 .|. (b `shiftL` 3)
  where b = if a < 0 then a + (2^15) else a

isCON :: Atom -> Bit
isCON a = (a `vat` n0) <&> inv (a `vat` n1) <&> (a `vat` n2)

conArity :: Atom -> Arity
conArity = vtake n3 . vdrop n3

conIndex :: Atom -> FunAddr
conIndex = vtake n10 . vdrop n6

makeCON :: Arity -> FunAddr -> Atom
makeCON n a = high +> low +> high +> (n <++> a <++> (low +> low +> vempty))

encodeCON :: Integer -> Integer -> Integer
encodeCON n a
  | n >= 0 && n <= 6 = 5 .|. ((n+1) `shiftL` 3) .|. (a `shiftL` 6)
  | otherwise = error "encodeCON: invalid arguments"

isARG :: Atom -> Bit
isARG a = (a `vat` n0) <&> (a `vat` n1) <&> inv (a `vat` n2)

isArgShared :: Atom -> Bit
isArgShared a = a `vat` n3

argIndex :: Atom -> Word N8
argIndex = vtake n8 . vdrop n4

makeARG :: Bit -> Word N8 -> Atom
makeARG s n = high +> high +> low +> s +> (n <++> vreplicate n6 low)

encodeARG :: Bool -> Int -> Integer
encodeARG s n
  | n >= 0 && n <= 7 = 3 .|. (boolToNum s `shiftL` 3) .|. (1 `shiftL` (n+4))
  | otherwise = error "encodeARG: invalid arguments"

isREG :: Atom -> Bit
isREG a = (a `vat` n0) <&> (a `vat` n1) <&> (a `vat` n2)

isRegShared :: Atom -> Bit
isRegShared a = a `vat` n3

regIndex :: Atom -> Word N8
regIndex = vtake n8 . vdrop n4

makeREG :: Bit -> Word N8 -> Atom
makeREG s n = high +> high +> high +> s +> (n <++> vreplicate n6 low)

encodeREG :: Bool -> Int -> Integer
encodeREG s n
  | n >= 0 && n <= 7 = 7 .|. (boolToNum s `shiftL` 3) .|. (1 `shiftL` (n+4))
  | otherwise = error "encodeREG: invalid arguments"

-- Arity calculation

arity :: Atom -> Arity
arity a = pickG [ (isINT a, 1), (inv (isINT a), funArity a) ]

-- Applications

{-

Application structure:

  +------------+---------------+
  | Field      | Size (bits)   |
  +------------+---------------+
  | Arity      | 2             |
  | NF         | 1             |
  | HasAlts    | 1             |
  | Collected  | 1             |
  | Atoms      | 4x18          |
  +------------+---------------+

Total width of application = 77 bits

-}

appN      = 77 :: Int
type AppN = N77
type App  = Word AppN

appArity :: App -> Word N2
appArity app = vtake n2 app

isNF :: App -> Bit
isNF app = app `vat` n2

hasAlts :: App -> Bit
hasAlts app = app `vat` n3

isCollected :: App -> Bit
isCollected app = app `vat` n4

atoms :: App -> Vec N4 Atom
atoms = vrigid . vgroup n18 . vdrop n5

alts :: App -> FunAddr
alts app = vtake n10 (vdrop n3 (vlast (atoms app)))

makeApp :: Word N2 -> Bit -> Bit -> Vec N4 Atom -> App
makeApp arity n c as =
  arity <++> vsingle n <++> vsingle c <++> vsingle low <++> vconcat as

setCollected :: Bit -> App -> App
setCollected b app = vtake n4 app <++> vsingle b <++> vdrop n5 app

makeCollected :: HeapAddr -> App
makeCollected addr =
  setCollected high (makeApp 0 low low (makeAP high addr +> vecOf 0))

relocatedAddr :: App -> HeapAddr
relocatedAddr = pointer . vhead . atoms

encodeApp :: Integer -> Bool -> Bool -> [Integer] -> Integer
encodeApp arity n c as
   | arity >= 0 && arity <= 3 && length as <= 4 =
           arity
       .|. (boolToNum n `shiftL` 2)
       .|. (boolToNum c `shiftL` 3)
       .|. (join atomN as `shiftL` 5)
   | otherwise = error "encodeApp: invalid parameters"

mapApp :: (Atom -> Atom) -> App -> App
mapApp f a = vtake n5 a <++> vconcat (vmap f (atoms a))

-- Updates

type UpdateN = N28
type Update  = Word UpdateN

makeUpdate :: StackAddr -> HeapAddr -> Update
makeUpdate sa ha = sa <++> ha

splitUpdate :: Update -> (StackAddr, HeapAddr)
splitUpdate u = vsplitAt n13 u

-- Templates

{-

Template structure:

  +------------+---------------+
  | Field      | Size (bits)   |
  +------------+---------------+
  | Offset     | 4             |
  | Top        | 18            |
  | PushAlts   | 1             |
  | Alts       | 10            |
  | InstAtoms2 | 1             |
  | App2Header | 5             |
  | PushMask   | 5             |
  | App2Atoms  | 5x18          |
  | InstApp1   | 1             |
  | App1       | 5 + 4x18      |
  | App1Prim   | 1             |
  | App2Prim   | 1             |
  | DestReg1   | 4             |
  | DestReg2   | 4             |
  +------------+---------------+

Total width of template = 222 bits

-}

type TemplateN = N234
type Template = Word TemplateN

templateOffset :: Template -> Word N4
templateOffset t = vtake n4 t

templateTop :: Template -> Atom
templateTop = vtake n18 . vdrop n4

templatePushAlts :: Template -> Bit
templatePushAlts t = vhead (vdrop n22 t)

templateAlts :: Template -> FunAddr
templateAlts = vtake n10 . vdrop n23

templateInstAtoms2 :: Template -> Bit
templateInstAtoms2 t = vhead (vdrop n33 t)

templateApp2Header :: Template -> Word N5
templateApp2Header t = vtake n5 (vdrop n34 t)

templatePushMask :: Template -> Word N5
templatePushMask t = vtake n5 (vdrop n39 t)

templateApp2Atoms :: Template -> Vec N5 Atom
templateApp2Atoms = vrigid . vgroup n18 . vtake n90 . vdrop n44

templateInstApp1 :: Template -> Bit
templateInstApp1 t = vhead (vdrop n134 t)

templateApp1 :: Template -> App
templateApp1 = vtake n77 . vdrop n135

templateApp2 :: Template -> App
templateApp2 t =
  templateApp2Header t <++> vconcat (vtake n4 (templateApp2Atoms t))

templateIsApp1Prim :: Template -> Bit
templateIsApp1Prim = vhead . vdrop n212

templateIsApp2Prim :: Template -> Bit
templateIsApp2Prim = vhead . vdrop n213

templateDestReg1 :: Template -> Word N4
templateDestReg1 = vrigid . vtake n4 . vdrop n214

templateDestReg2 :: Template -> Word N4
templateDestReg2 = vrigid . vtake n4 . vdrop n218

mapTemplate :: (Atom -> Atom) -> Template -> Template
mapTemplate f t =
       templateOffset t
  <++> f (templateTop t)
  <++> vsingle (templatePushAlts t)
  <++> templateAlts t
  <++> vsingle (templateInstAtoms2 t)
  <++> templateApp2Header t
  <++> templatePushMask t
  <++> vconcat (vmap f (templateApp2Atoms t))
  <++> vsingle (templateInstApp1 t)
  <++> mapApp f (templateApp1 t)
  <++> vsingle (templateIsApp1Prim t)
  <++> vsingle (templateIsApp2Prim t)
  <++> templateDestReg1 t
  <++> templateDestReg2 t
  <++> vreplicate n12 low

encodeTemplate
  :: Integer -- Offset
  -> Integer -- Top
  -> Bool    -- Push alts?
  -> Integer -- Alts
  -> Bool    -- Instantiate Atoms2?
  -> Integer -- App2 Header
  -> Integer -- Push Mask
  -> Integer -- App2 Atoms
  -> Bool    -- Instantiate App1?
  -> Integer -- App1
  -> Bool    -- Is App1 prim?
  -> Bool    -- Is App2 prim?
  -> Integer -- Dest reg 1
  -> Integer -- Dest reg 2
  -> Integer
encodeTemplate offset top pushAlts alts
               instAtoms2 app2Header
               pushMask app2Atoms instApp1 app1
               app1Prim app2Prim
               destReg1 destReg2 = fst encoding
  where
    offset'  = if offset < 0 then offset + 16 else offset
    encoding = (offset'             , 4       )
            <> (top                 , 18      )
            <> (boolToNum pushAlts  , 1       )
            <> (alts                , 10      )
            <> (boolToNum instAtoms2, 1       )
            <> (app2Header          , 5       )
            <> (pushMask            , 5       )
            <> (app2Atoms           , 5*18    )
            <> (boolToNum instApp1  , 1       )
            <> (app1                , 5 + 4*18)
            <> (boolToNum app1Prim  , 1       )
            <> (boolToNum app2Prim  , 1       )
            <> (destReg1            , 4       )
            <> (destReg2            , 4       )

-- Constants

mainAtom :: Atom
mainAtom = makeFUN high 0 0

falseAtom :: Atom
falseAtom = makeCON 1 0

trueAtom :: Atom
trueAtom = makeCON 1 1

bool :: Bit -> Atom
bool b = makeCON 1 (b +> 0)

-- Auxiliaries

boolToNum :: Num a => Bool -> a
boolToNum False = 0
boolToNum True = 1

join :: Int -> [Integer] -> Integer
join n [] = 0
join n (x:xs) = x .|. join n (map (`shiftL` n) xs)

infixl 5 <>
(x0, w0) <> (x1, w1) = (x0 .|. x1 `shiftL` w0, w0+w1)
