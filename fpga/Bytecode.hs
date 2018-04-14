module Bytecode where

import Prelude hiding (Word, (.), (<>))
import Lava
import Data.Bits

{-

Pointer tagging scheme:

  +------------------+-----------------------------------------+
  | Tag (LSB first)  | Contents                                |
  +------------------+-----------------------------------------+
  | 000              | Function arity and address              | isFUN
                       arity:3 addr:10 .. first:1
  | 001              | Primitive function arity and identifier | isADD, isSUB, ...
                       arity:3 swap:1 add:1 sub:1 eq:1 neq:1 leq:1 and:1 ...
  | 010              | Pointer to application                  | isAP
                       pointer:...
  | 011              | Pointer to shared application           | isAP isShared
                       pointer:...
  | 100              | Primitive integer                       | isINT
  | 101              | Constructor arity (+1) and index        | isCON
                       arity:3 index:10
  | 110              | Reference to function argument          | isARG
                       shared:1 index:8
  | 111              | Reference to register                   | isREG
                       shared:1 index:8
  +------------------+-----------------------------------------+

Assumptions:

  * Max function arity = 7
  * Max constructor arity = 6

-}

type HeapAddrN    = S ToSpaceAddrN
type HeapAddr     = Word HeapAddrN
type StackAddrN   = N10 -- KnuthBendix needs more than 512
type StackAddr    = Word StackAddrN
type ArityN       = N3
type Arity        = Word ArityN
type FunAddrN     = N10
type FunAddr      = Word FunAddrN
type ToSpaceAddrN = N13  -- half of heap. HERE IT IS, THE MAIN PARAMETER.
type ToSpaceAddr  = Word ToSpaceAddrN
type UStackAddrN  = N9 -- N6 should be enough
type LStackAddrN  = N9 -- N9 should be enough
type UpdateN      = N24 -- ew (UStackAddrN + HeapAddrN)

-- In order to enlargen the integer range without affecting the heap
-- size we break the previous assumption that pointers and integers
-- are the same size.
type NumberN      = S HeapAddrN
type Number       = Word NumberN

heapAddrN         = undefined :: HeapAddrN
heapAddrWidth     = value heapAddrN

funAddrN          = undefined :: FunAddrN
funAddrWidth      = value funAddrN

stackAddrN        = undefined :: StackAddrN
stackAddrWidth    = value stackAddrN

numberN           = undefined :: NumberN
numberWidth       = value numberN

-- Atoms

type AtomN        = S (S (S NumberN))
type Atom         = Word AtomN

atomN             = undefined :: AtomN
atomWidth         = value atomN :: Int
atomWidth5        = n90 -- = 5 * atomN. Ew, is there a better way?

isFUN :: Atom -> Bit
isFUN a = inv (a `vat` n0) <&> inv (a `vat` n1)

splitAtom :: Atom -> (Word N3, Number)
splitAtom = vsplitAt n3

splitFunAtom = vsplitAt n3 `o` snd `o` splitAtom

funArity :: Atom -> Arity
funArity = fst `o` splitFunAtom

funAddr :: Atom -> FunAddr
funAddr = vtake funAddrN `o` snd `o` splitFunAtom

funFirst :: Atom -> Bit
funFirst = vlast `o` snd `o` splitFunAtom

funTag = low +> low +> low +> vempty

makeFUN :: Bit -> Word N3 -> FunAddr -> Atom
makeFUN b n a = funTag <++> (n <++> a <++> (low +> b +> vempty)) -- Ew!

encodeFUN :: Bool -> Integer -> Integer -> Integer
encodeFUN b n a
  | n >= 0 && n <= 7 =
     (n `shiftL` 3) .|. (a `shiftL` 6) .|. (boolToNum b `shiftL` (atomWidth - 1))
  | otherwise = error "encodeFUN: invalid arguments"

getSwapBit :: Atom -> Bit
getSwapBit a = a `vat` n6

invSwapBit :: Atom -> Atom
invSwapBit a = vtake n6 a <++> vsingle (inv (a `vat` n6)) <++> vdrop n7 a

isADD, isSUB, isEQ, isNEQ, isLEQ, isAND, isST32, isLD32 :: Atom -> Bit
isADD a = a `vat` n7
isSUB a = a `vat` n8
isEQ  a = a `vat` n9
isNEQ a = a `vat` n10
isLEQ a = a `vat` n11
isAND a = a `vat` n12
isST32 a = a `vat` n13
isLD32 a = a `vat` n14

b2i :: Bool -> Integer
b2i False = 0
b2i True = 1

primADD, primSUB, primEQ, primNEQ, primLEQ, primAND, primST32 :: Bool -> Integer
primADD s = 4 .|. (2 `shiftL` 3) .|. ((b2i s) `shiftL` 6) .|. (1 `shiftL` 7)
primSUB s = 4 .|. (2 `shiftL` 3) .|. ((b2i s) `shiftL` 6) .|. (1 `shiftL` 8)
primEQ  s = 4 .|. (2 `shiftL` 3) .|. ((b2i s) `shiftL` 6) .|. (1 `shiftL` 9)
primNEQ s = 4 .|. (2 `shiftL` 3) .|. ((b2i s) `shiftL` 6) .|. (1 `shiftL` 10)
primLEQ s = 4 .|. (2 `shiftL` 3) .|. ((b2i s) `shiftL` 6) .|. (1 `shiftL` 11)
primAND s = 4 .|. (2 `shiftL` 3) .|. ((b2i s) `shiftL` 6) .|. (1 `shiftL` 12)
primST32 s = 4 .|. (2 `shiftL` 3) .|. ((b2i s) `shiftL` 6) .|. (1 `shiftL` 13)
primLD32 s = 4 .|. (2 `shiftL` 3) .|. ((b2i s) `shiftL` 6) .|. (1 `shiftL` 14)

isAP :: Atom -> Bit
isAP a = inv (a `vat` n0) <&> (a `vat` n1)

isShared :: Atom -> Bit
isShared a = a `vat` n2

pointer :: Atom -> HeapAddr
pointer = vtake heapAddrN `o` snd `o` splitAtom

makeAP :: Bit -> HeapAddr -> Atom
makeAP s a = low +> high +> s +> (a <++> low +> vempty)

encodeAP :: Bool -> Integer -> Integer
encodeAP s a = 2 .|. (boolToNum s `shiftL` 2) .|. (b `shiftL` 3)
  where b = if a < 0 then a+(2^heapAddrWidth) else a

dash :: Bit -> Atom -> Atom
dash s a = vtake n2 a <++> vsingle shared <++> vdrop n3 a
  where shared = (isAP a <&> s) <|> (a `vat` n2)

isINT :: Atom -> Bit
isINT a = (a `vat` n0) <&> inv (a `vat` n1) <&> inv (a `vat` n2)

intValue :: Atom -> Number
intValue = snd `o` splitAtom

makeINT :: Number -> Atom
makeINT a = high +> low +> low +> a

encodeINT :: Integer -> Integer
encodeINT a = 1 .|. (b `shiftL` 3)
  where b = if a < 0 then a + (2^numberWidth) else a

isCON :: Atom -> Bit
isCON a = (a `vat` n0) <&> inv (a `vat` n1) <&> (a `vat` n2)

conArity :: Atom -> Arity
conArity = vtake n3 `o` vdrop n3

conIndex :: Atom -> FunAddr
conIndex = vtake funAddrN `o` vdrop n6

makeCON :: Arity -> FunAddr -> Atom
makeCON n a = high +> low +> high +> (n <++> a <++> low +> low +> vempty) -- ew

encodeCON :: Integer -> Integer -> Integer
encodeCON n a
  | n >= 0 && n <= 6 = 5 .|. ((n+1) `shiftL` 3) .|. (a `shiftL` 6)
  | otherwise = error "encodeCON: invalid arguments"

isARG :: Atom -> Bit
isARG a = (a `vat` n0) <&> (a `vat` n1) <&> inv (a `vat` n2)

isArgShared :: Atom -> Bit
isArgShared a = a `vat` n3

argIndex :: Atom -> Word N8
argIndex = vtake n8 `o` vdrop n4

makeARG :: Bit -> Word N8 -> Atom
makeARG s n = high +> high +> low +> s +> (n <++> vreplicate n6 low) -- ew

encodeARG :: Bool -> Int -> Integer
encodeARG s n
  | n >= 0 && n <= 7 = 3 .|. (boolToNum s `shiftL` 3) .|. (1 `shiftL` (n+4))
  | otherwise = error "encodeARG: invalid arguments"

isREG :: Atom -> Bit
isREG a = (a `vat` n0) <&> (a `vat` n1) <&> (a `vat` n2)

isRegShared :: Atom -> Bit
isRegShared a = a `vat` n3

regIndex :: Atom -> Word N8
regIndex = vtake n8 `o` vdrop n4

makeREG :: Bit -> Word N8 -> Atom
makeREG s n = high +> high +> high +> s +> (n <++> vreplicate n6 low) -- ew

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

type AppN         = N77 -- 5 + 4 * atomN ew
type App          = Word AppN
appN              = undefined :: AppN
appWidth          = value appN

appArity :: App -> Word N2
appArity app = vtake n2 app

isNF :: App -> Bit
isNF app = app `vat` n2

hasAlts :: App -> Bit
hasAlts app = app `vat` n3

isCollected :: App -> Bit
isCollected app = app `vat` n4

splitApp = vsplitAt n5

atoms :: App -> Vec N4 Atom
atoms = vrigid `o` vgroup atomN `o` snd `o` splitApp

alts :: App -> FunAddr
alts = vtake funAddrN `o` vdrop n3 `o` vlast `o` atoms

makeApp :: Word N2 -> Bit -> Bit -> Vec N4 Atom -> App
makeApp arity n c as =
  arity <++> vsingle n <++> vsingle c <++> vsingle low <++> vconcat as

setCollected :: Bit -> App -> App
setCollected b app = vtake n4 app <++> vsingle b <++> vdrop n5 app

makeCollected :: HeapAddr -> App
makeCollected addr =
  setCollected high (makeApp 0 low low (makeAP high addr +> vecOf 0))

relocatedAddr :: App -> HeapAddr
relocatedAddr = pointer `o` vhead `o` atoms

encodeApp :: Integer -> Bool -> Bool -> [Integer] -> Integer
encodeApp arity n c as
   | arity >= 0 && arity <= 3 && length as <= 4 =
           arity
       .|. (boolToNum n `shiftL` 2)
       .|. (boolToNum c `shiftL` 3)
       .|. (joinIntegers atomWidth as `shiftL` 5)
   | otherwise = error "encodeApp: invalid parameters"

mapApp :: (Atom -> Atom) -> App -> App
mapApp f a = vtake n5 a <++> vconcat (vmap f (atoms a))

-- Updates

type Update  = Word UpdateN

makeUpdate :: StackAddr -> HeapAddr -> Update
makeUpdate sa ha = sa <++> ha

splitUpdate :: Update -> (StackAddr, HeapAddr)
splitUpdate u = vsplitAt stackAddrN u

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

NB: The actual values are different - these are just illustrations

-}

type TemplateN = N222 -- 10 * atomN + 42  ew
type Template  = Word TemplateN

tempOffset = vsplitAt n4

templateOffset :: Template -> Word N4
templateOffset = fst `o` tempOffset

tempTop = vsplitAt atomN `o` snd `o` tempOffset

templateTop :: Template -> Atom
templateTop = fst `o` tempTop

tempPushAlts = vsplitAt n1 `o` snd `o` tempTop

templatePushAlts :: Template -> Bit
templatePushAlts = vhead `o` fst `o` tempPushAlts

tempAlts = vsplitAt funAddrN `o` snd `o` tempPushAlts

templateAlts :: Template -> FunAddr
templateAlts = fst `o` tempAlts

tempInstAtoms2 = vsplitAt n1 `o` snd `o` tempAlts

templateInstAtoms2 :: Template -> Bit
templateInstAtoms2 = vhead `o` fst `o` tempInstAtoms2

tempApp2Header = vsplitAt n5 `o` snd `o` tempInstAtoms2

templateApp2Header :: Template -> Word N5
templateApp2Header = fst `o` tempApp2Header

tempPushMask = vsplitAt n5 `o` snd `o` tempApp2Header

templatePushMask :: Template -> Word N5
templatePushMask = fst `o` tempPushMask

tempApp2Atoms = vsplitAt atomWidth5 `o` snd `o` tempPushMask

templateApp2Atoms :: Template -> Vec N5 Atom
templateApp2Atoms = vrigid `o` vgroup atomN `o` fst `o` tempApp2Atoms

tempInstApp1 = vsplitAt n1 `o` snd `o` tempApp2Atoms

templateInstApp1 :: Template -> Bit
templateInstApp1 = vhead `o` fst `o` tempInstApp1

tempApp1 = vsplitAt appN `o` snd `o` tempInstApp1

templateApp1 :: Template -> App
templateApp1 = fst `o` tempApp1

templateApp2 :: Template -> App
templateApp2 t =
  templateApp2Header t <++> vconcat (vtake n4 (templateApp2Atoms t))

tempIsApp1Prim = vsplitAt n1 `o` snd `o` tempApp1

templateIsApp1Prim :: Template -> Bit
templateIsApp1Prim = vhead `o` fst `o` tempIsApp1Prim

tempIsApp2Prim = vsplitAt n1 `o` snd `o` tempIsApp1Prim
tempDestReg1 = vsplitAt n4 `o` snd `o` tempIsApp2Prim
tempDestReg2 = vsplitAt n4 `o` snd `o` tempDestReg1

templateIsApp2Prim :: Template -> Bit
templateIsApp2Prim = vhead `o` fst `o` tempIsApp2Prim

templateDestReg1 :: Template -> Word N4
templateDestReg1 = vrigid `o` fst `o` tempDestReg1

templateDestReg2 :: Template -> Word N4
templateDestReg2 = vrigid `o` fst `o` tempDestReg2

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
    encoding = (offset'             , 4)
            <> (top                 , atomWidth)
            <> (boolToNum pushAlts  , 1)
            <> (alts                , funAddrWidth)
            <> (boolToNum instAtoms2, 1)
            <> (app2Header          , 5)
            <> (pushMask            , 5)
            <> (app2Atoms           , 5*atomWidth)
            <> (boolToNum instApp1  , 1)
            <> (app1                , 5 + 4*atomWidth)
            <> (boolToNum app1Prim  , 1)
            <> (boolToNum app2Prim  , 1)
            <> (destReg1            , 4)
            <> (destReg2            , 4)

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

joinIntegers :: Int -> [Integer] -> Integer
joinIntegers n [] = 0
joinIntegers n (x:xs) = x .|. joinIntegers n (map (`shiftL` n) xs)

infixl 5 <>
(x0, w0) <> (x1, w1) = (x0 .|. x1 `shiftL` w0, w0+w1)
