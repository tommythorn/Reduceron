{-# OPTIONS_GHC -fno-cse -fno-full-laziness #-}

{- |

Defines what a 'Bit' is, the central abstract data type of the whole
library, along with lots of things you might like to do with bits.

-}

module Lava.Bit
  ( Bit

    -- * Standard logic gates
  , low
  , high
  , inv
  , delayBit
  , delayBitEn
  , and2
  , (<&>)
  , or2
  , (<|>)
  , xor2
  , (<#>)
  , xorcy
  , eq2
  , (<=>)
  , muxBit
  , muxcy
  , name

    -- * RAMs
  , RamInps(..)
  , RamAlgorithm(..)
  , primRam
  , primDualRam

    -- * Generic operations over structures of bits
  , Generic(..)
  , BitContainer
  , cons
  , (><)
  , structure
  , bits
  , mapG
  , zipWithG
  , lazyZipWithG

    -- * Simulation
  , boolToBit
  , simulate
  , simulateN
  , simulateSeq
  , bitToBool

    -- * Netlist generation
  , Net(..)
  , Netlist(..)
  , netlist

    -- * Primitive component creation
  , makeComponent
  , Signal
  , InstanceId
  , OutputNumber
  , Wire
  , Parameter(..)
  , lookupParam

  ) where

-- To implement Observable Sharing.
import Data.IORef
import System.IO.Unsafe(unsafePerformIO)

-- Data Structures: lists, join-lits and int-maps.
import Data.List
import qualified Lava.JList as JL
import qualified Data.IntMap as IM

-- Conversion functions between integers a bool-lists.
import Lava.Binary

------------------------------------ Bits -------------------------------------
-- | Every primitive component instance has a unique number.
type InstanceId = Int

-- | Each output from a primitive component is numbered.
type OutputNumber = Int

-- | A wire is uniquely identified by a instance id and an output number.
type Wire = (InstanceId, OutputNumber)

-- | A time-varying binary signal is clasically a list of booleans.
type Signal = [Bool]

-- | Components may have compile-time parameters, for example whether
-- a flip-flop initialises to high or low.  A parameter has a
-- name and a value, both represented as strings.
data Parameter = String :-> String deriving Show

-- | The central ADT of Lava.
data Bit =
  Symbol { componentName :: String
         , numOutputs    :: Int
         , parameters    :: [Parameter]
         , inputs        :: [Bit]
         , instanceRef   :: IORef (Maybe InstanceId)
         , outputNumber  :: OutputNumber
         , outputSignal  :: Signal
         }

lookupParam :: [Parameter] -> String -> String
lookupParam ps p = case [v | (k :-> v) <- ps, p == k] of
                     [] -> error ("Unrecognised parameter '" ++ p ++ "'")
                     v:vs -> v
-------------------------------------------------------------------------------

------------------------- Generic structures of bits --------------------------

-- | Generic structures of bits
class Generic a where
  generic :: a -> BitContainer a

{- |

Basic idea pinched from Uniplate.  To use generics, you really
don't need to know the internals of this type as we provide 'cons'
and '><'. For example:

> instance Generic a => Generic [a] where
>    generic [] = cons []
>    generic (a:as) = cons (:) >< a >< as

-}
type BitContainer a = (JL.JList Bit, JL.JList Bit -> a)

cons :: a -> BitContainer a
cons a = (JL.Zero, \JL.Zero -> a)

(><) :: Generic a => BitContainer (a -> b) -> a -> BitContainer b
(a, f) >< x = (a JL.:+: b, \(a JL.:+: b) -> f a (g b))
  where (b, g) = generic x

instance Generic () where
  generic = cons

instance Generic Bit where
  generic a = (JL.One a, \(JL.One a) -> a)

instance Generic a => Generic (Maybe a) where
  generic Nothing = cons Nothing
  generic (Just a) = cons Just >< a

instance Generic a => Generic [a] where
  generic [] = cons []
  generic (a:as) = cons (:) >< a >< as

instance (Generic a, Generic b) => Generic (Either a b) where
  generic (Left a) = cons Left >< a
  generic (Right b) = cons Right >< b

instance (Generic a, Generic b) => Generic (a, b) where
  generic (a, b) = cons (,) >< a >< b

instance (Generic a, Generic b, Generic c) => Generic (a, b, c) where
  generic (a, b, c) = cons (,,) >< a >< b >< c

instance (Generic a, Generic b, Generic c,
          Generic d) => Generic (a, b, c, d) where
  generic (a, b, c, d) = cons (,,,) >< a >< b >< c >< d

instance (Generic a, Generic b, Generic c,
          Generic d, Generic e) => Generic (a, b, c, d, e) where
  generic (a, b, c, d, e) = cons (,,,,) >< a >< b >< c >< d >< e

instance (Generic a, Generic b, Generic c,
          Generic d, Generic e, Generic f) => Generic (a, b, c, d, e, f) where
  generic (a, b, c, d, e, f) = cons (,,,,,) >< a >< b >< c >< d >< e >< f

instance (Generic a, Generic b, Generic c,
          Generic d, Generic e, Generic f,
          Generic g) => Generic (a, b, c, d, e, f, g) where
  generic (a, b, c, d, e, f, g) =
    cons (,,,,,,) >< a >< b >< c >< d >< e >< f >< g

instance (Generic a, Generic b, Generic c,
          Generic d, Generic e, Generic f,
          Generic g, Generic h) => Generic (a, b, c, d, e, f, g, h) where
  generic (a, b, c, d, e, f, g, h) =
    cons (,,,,,,,) >< a >< b >< c >< d >< e >< f >< g >< h

-- | Extract a join-list of bits from any structure of bits.
structure :: Generic a => a -> JL.JList Bit
structure = fst . generic

-- | Extract a list of bits from any structure of bits.
bits :: Generic a => a -> [Bit]
bits = JL.toList . structure

-- | Map a function over bits across any structure of bits.
mapG :: Generic a => (Bit -> Bit) -> a -> a
mapG f a = ca (JL.map f sa)
  where
    (sa, ca) = generic a

-- | Zip a function over bits across any two structures of bits.
-- Assumes that the two structures have the same shape.
zipWithG :: Generic a => (Bit -> Bit -> Bit) -> a -> a -> a
zipWithG f a b = ca (JL.zipWith f sa sb)
  where
    (sa, ca) = generic a
    (sb, cb) = generic b

-- | A lazier but less-defined version of 'zipWithG'.
lazyZipWithG :: Generic a => (Bit -> Bit -> Bit) -> a -> a -> a
lazyZipWithG f a b = ca (JL.lazyZipWith f sa sb)
  where
    (sa, ca) = generic a
    (sb, cb) = generic b
-------------------------------------------------------------------------------

---------------------------- Primitive components -----------------------------
-- | The programmer\'s interface to creating new primitive components.
-- See definition of 'and2' for an example.
{-# NOINLINE makeComponent #-}
makeComponent ::
     String                 -- ^ Component name
  -> [Bit]                  -- ^ Inputs
  -> Int                    -- ^ Number of outputs
  -> ([Signal] -> [Signal]) -- ^ Simulation function
  -> [Parameter]            -- ^ Component parameters
  -> ([Bit] -> a)           -- ^ Continuation (passed output bits)
  -> a                      -- ^ Result
makeComponent comp inps numOuts sim params k = k outBits
  where
    outBits = map (\i -> Symbol {
                           componentName = comp
                         , numOutputs    = numOuts
                         , parameters    = params
                         , inputs        = inps
                         , instanceRef   = ref
                         , outputNumber  = i
                         , outputSignal  = outSigs !! i
                         }
                  ) [0 .. numOuts-1]

    -- Output signals
    outSigs = sim (map outputSignal inps)

    -- For observable sharing.  (Pass "outSigs" to prevent full-laziness.)
    {-# NOINLINE ref #-}
    ref = newRef outSigs Nothing

-- Use of unsafePerformIO to implement Observable Sharing
{-# NOINLINE newRef #-}
newRef :: a -> Maybe InstanceId -> IORef (Maybe InstanceId)
newRef s x = unsafePerformIO (newIORef x)

-- | Logic '0'.
low :: Bit
low = makeComponent "low"
    {-   Inputs: -} []
    {-  Outputs: -} 1
    {- Simulate: -} (\[] -> [repeat False])
    {-   Params: -} []
    {- Continue: -} (\[o] -> o)

-- | Logic '1'.
high :: Bit
high = makeComponent "high"
     {-   Inputs: -} []
     {-  Outputs: -} 1
     {- Simulate: -} (\[] -> [repeat True])
     {-   Params: -} []
     {- Continue: -} (\[o] -> o)

-- | Named input - for synthesis.
name :: String -> Bit
name s = makeComponent "name"
       {-   Inputs: -} []
       {-  Outputs: -} 1
       {- Simulate: -} (\[] -> [error msg])
       {-   Params: -} ["name" :-> s]
       {- Continue: -} (\[o] -> o)
  where msg = "Can't simulate circuit containing a name ('" ++ s ++ "')"

-- | Inverter.
inv :: Bit -> Bit
inv a = makeComponent "inv"
      {-   Inputs: -} [a]
      {-  Outputs: -} 1
      {- Simulate: -} (\[a] -> [map not a])
      {-   Params: -} []
      {- Continue: -} (\[o] -> o)

-- | D-type flip-flop, with initialiser (first argument).
delayBit :: Bit -> Bit -> Bit
delayBit init a =
    makeComponent "delay"
  {-   Inputs: -} [init, a]
  {-  Outputs: -} 1
  {- Simulate: -} (\[init, a] -> [head init:a])
  {-   Params: -} ["init" :-> getConst (componentName init)]
  {- Continue: -} (\[o] -> o)

getConst "low"  = "0"
getConst "high" = "1"
getConst _ = error "'delayBit' must have constant initialiser"

-- | D-type flip-flop with input-enable (first argument).
delayBitEn :: Bit -> Bit -> Bit -> Bit
delayBitEn en init a =
    makeComponent "delayEn"
  {-   Inputs: -} [init, en, a]
  {-  Outputs: -} 1
  {- Simulate: -} (\[init, en, a] -> [simDelayEn (head init) en a])
  {-   Params: -} ["init" :-> getConst (componentName init)]
  {- Continue: -} (\[o] -> o)

logic2 :: String -> (Bool -> Bool -> Bool) -> (Bit, Bit) -> Bit
logic2 name f (a, b) =
    makeComponent name
  {-   Inputs: -} [a, b]
  {-  Outputs: -} 1
  {- Simulate: -} (\[a, b] -> [zipWith f a b])
  {-   Params: -} []
  {- Continue: -} (\[o] -> o)

-- | AND gate.
and2 :: (Bit, Bit) -> Bit
and2 = logic2 "and2" (&&)

infixr 3 <&>
(<&>) :: Bit -> Bit -> Bit
a <&> b = and2 (a, b)

-- | OR gate.
or2 :: (Bit, Bit) -> Bit
or2 = logic2 "or2" (||)

infixr 2 <|>
(<|>) :: Bit -> Bit -> Bit
a <|> b = or2 (a, b)

-- | XOR gate.
xor2 :: (Bit, Bit) -> Bit
xor2 = logic2 "xor2" (/=)

infixr 2 <#>
(<#>) :: Bit -> Bit -> Bit
a <#> b = xor2 (a, b)

-- | XOR gate, specifically a /Xilinx Carry Logic/ XOR gate.
xorcy :: (Bit, Bit) -> Bit
xorcy = logic2 "xorcy" (/=)

-- | EQ gate.
eq2 :: (Bit, Bit) -> Bit
eq2 = logic2 "eq2" (==)

infixr 4 <=>
(<=>) :: Bit -> Bit -> Bit
a <=> b = eq2 (a, b)

-- | Bit multiplexer.
muxBit :: Bit -> Bit -> Bit -> Bit
muxBit sel a b = (sel <&> b) <|> (inv sel <&> a)

-- | Bit multiplexer - specifically a /Xilinx Carry Logic/ bit multiplexer.
muxcy :: Bit -> (Bit, Bit) -> Bit
muxcy sel (a, b) =
    makeComponent "muxcy"
  {-   Inputs: -} [b, a, sel]
  {-  Outputs: -} 1
  {- Simulate: -} (\[b, a, sel] -> [zipWith3 f sel a b])
  {-   Params: -} []
  {- Continue: -} (\[o] -> o)
  where f sel a b = if sel then b else a

-- | Block RAM inputs; data-bus and address-bus can be of any width!
-- Use 'Lava.Prelude.RamInputs' for stronger type-safety.
data RamInps =
  RamInps {
      dataBus :: [Bit]
    , addressBus :: [Bit]
    , writeEnable :: Bit
  }

-- | How should the RAM be built?  Used by the Xilinx Core Generator -
-- see Xilinx docs for details.
data RamAlgorithm =
  MinArea | Width1 | Width2 | Width4 | Width9 | Width18 | Width36

encodeRamAlgorithm :: RamAlgorithm -> String
encodeRamAlgorithm MinArea = ""
encodeRamAlgorithm Width1 = "16kx1"
encodeRamAlgorithm Width2 = "8kx2"
encodeRamAlgorithm Width4 = "4kx4"
encodeRamAlgorithm Width9 = "2kx9"
encodeRamAlgorithm Width18 = "1kx18"
encodeRamAlgorithm Width36 = "512x36"

-- | Single-port RAM with initialiser.  Use 'Lava.Prelude.ram' for
-- stronger type-safety.
primRam :: [Integer] -> String -> RamAlgorithm -> RamInps -> [Bit]
primRam init annotation ramAlg ins =
    makeComponent "ram"
  {-   Inputs: -} ([writeEnable ins] ++ dataBus ins ++ addressBus ins)
  {-  Outputs: -} dwidth
  {- Simulate: -} (simRam dwidth awidth init)
  {-   Params: -} [ "init" :-> show init
                  , "dwidth" :-> show dwidth
                  , "awidth" :-> show awidth
                  , "primtype" :-> pt
                  , "annotation" :-> annotation
                  ]
  {- Continue: -} id
  where
    pt     = encodeRamAlgorithm ramAlg
    dwidth = length (dataBus ins)
    awidth = length (addressBus ins)

-- | Dual-port RAM with initialiser.  Use 'Lava.Prelude.dualRam' for
-- stronger type-safety.
primDualRam :: [Integer] -> String -> RamAlgorithm -> (RamInps, RamInps) -> ([Bit], [Bit])
primDualRam init annotation ramAlg (ins1, ins2) =
    makeComponent "dualRam"
  {-   Inputs: -} ([writeEnable ins1] ++ [writeEnable ins2] ++
                   dataBus ins1       ++ dataBus ins2       ++
                   addressBus ins1    ++ addressBus ins2    )
  {-  Outputs: -} (2*dwidth)
  {- Simulate: -} (simDualRam dwidth awidth init)
  {-   Params: -} [ "init" :-> show init
                  , "dwidth" :-> show dwidth
                  , "awidth" :-> show awidth
                  , "primtype" :-> pt
                  , "annotation" :-> annotation
                  ]
  {- Continue: -} (splitAt dwidth)
  where
    pt     = encodeRamAlgorithm ramAlg
    dwidth = sameLength (dataBus ins1)    (dataBus ins2)
    awidth = sameLength (addressBus ins1) (addressBus ins2)
    sameLength xs ys = if length xs == length ys then length xs else
                         error "BlockRam ports must have same bus-widths"
-------------------------------------------------------------------------------

---------------------------------- Simulation ---------------------------------
-- | Convert 'False' to 'low' and 'True' to 'high'.
boolToBit :: Bool -> Bit
boolToBit False = low
boolToBit True = high

-- | Simulate a circuit returning a single bit, and convert result to
-- a boolean.
bitToBool :: Bit -> Bool
bitToBool s = head $ outputSignal s

instance Show Bit where
  show b = if bitToBool b then "high" else "low"

-- | Simulate a circuit, giving the output on each clock-cycle.
-- Returns an infinite list.
simulate :: Generic a => a -> [a]
simulate a = a : simulate (step a)
  where
    step a = mapG rest a
    rest b = b { outputSignal = tail (outputSignal b) }

-- | Simulate a circuit for N clock-cycles.
simulateN :: Generic a => Int -> a -> [a]
simulateN n a = take n (simulate a)

-- | Simulate a circuit with a constant waveform specified as input.
simulateSeq :: (Generic a, Generic b) => (a -> b) -> [a] -> [b]
simulateSeq f as = simulateN (length as) (f $ trans as)
  where
    trans [x] = x
    trans (x:xs) = zipWithG cons x (trans xs)
    cons x xs = x { outputSignal = head (outputSignal x) : outputSignal xs }

-- Simulation function for flip-flops with input-enable.
simDelayEn :: Bool -> [Bool] -> [Bool] -> [Bool]
simDelayEn init en d = unfoldr step (init, en, d)
  where
    step (x, ens, ds) = Just (x, (y, tail ens, tail ds))
      where y = if head ens then head ds else x

-- Simulation function for RAMs.
simRam :: Int -> Int -> [Integer] -> [[Bool]] -> [[Bool]]
simRam dwidth awidth init (we:sigs) =
  trans $ unfoldr step (zero, initialMap, we, dbus, abus)
  where
    (dbus, abus)   = splitAt dwidth sigs
    init'          = map (\x -> natToSizedBin x dwidth) init
    initialMap     = IM.fromList $ zip [0..2^awidth-1] init'
    zero           = replicate dwidth False
    step (o, m, we, dbus, abus) = Just (o, next)
      where i      = binToNat (map head abus)
            m'     = if head we then IM.insert i (map head dbus) m else m
            output = IM.findWithDefault zero i m'
            next   = (output, m', tail we, map tail dbus, map tail abus)

-- Simulation function for dual-port RAMs.
simDualRam :: Int -> Int -> [Integer] -> [[Bool]] -> [[Bool]]
simDualRam dwidth awidth init (we1:we2:sigs) = trans $
    unfoldr step (zero, zero, initial, we1, we2, dbus1, dbus2, abus1, abus2)
  where
    (dbus, abus)    = splitAt (2*dwidth) sigs
    (abus1, abus2)  = splitAt awidth abus
    (dbus1, dbus2)  = splitAt dwidth dbus
    init'           = map (\x -> natToSizedBin x dwidth) init
    initial         = IM.fromList $ zip [0..2^awidth-1] init'
    zero            = replicate dwidth False
    step (o1, o2, m, we1, we2, dbus1, dbus2, abus1, abus2) =
        Just (o1 ++ o2, next)
      where i       = binToNat (map head abus1)
            j       = binToNat (map head abus2)
            output1 = IM.findWithDefault zero i m''
            output2 = IM.findWithDefault zero j m''
            m'      = if head we1 then IM.insert i (map head dbus1) m else m
            m''     = if head we2 then IM.insert j (map head dbus2) m' else m'
            next    = (output1, output2,
                       m'',
                       tail we1, tail we2,
                       map tail dbus1, map tail dbus2,
                       map tail abus1, map tail abus2)
-------------------------------------------------------------------------------

------------------------------ Netlist synthesis ------------------------------
data Net =
  Net { netName    :: String
      , netParams  :: [Parameter]
      , netId      :: InstanceId
      , netNumOuts :: Int
      , netInputs  :: [Wire]
      } deriving Show

data Netlist =
  Netlist { namedOutputs :: [(String, Wire)]
          , nets         :: [Net]
          } deriving Show

bitToNetlist :: IORef Int -> Bit -> IO (JL.JList Net, Wire)
bitToNetlist i bit =
  do val <- readIORef (instanceRef bit)
     num <- readIORef i
     case val of
       Nothing ->
         do writeIORef (instanceRef bit) (Just num)
            writeIORef i (num+1)
            rest <- Prelude.mapM (bitToNetlist i) (inputs bit)
            let (nls, wires) = unzip rest
            let net = Net { netName    = componentName bit
                          , netParams  = parameters bit
                          , netId      = num
                          , netNumOuts = numOutputs bit
                          , netInputs  = wires
                          }
            return (foldr (JL.:+:) (JL.One net) nls, (num, outputNumber bit))
       Just j -> return (JL.Zero, (j, outputNumber bit))

-- | Turn any circuit into a netlist.
netlist :: Generic a => a -> a -> IO Netlist
netlist a b =
  do i <- newIORef (0 :: Int)
     result   <- JL.mapM (bitToNetlist i) sa
     let nls   = JL.map fst result
     let wires = JL.map snd result
     let outs  = JL.zipWith (\w b -> (getName b, w)) wires sb
     return (Netlist { namedOutputs = JL.toList outs
                     , nets = JL.toList (JL.concat nls)
                     })
  where
    sa = structure a
    sb = structure b
    getName b | componentName b == "name" = lookupParam (parameters b) "name"
    getName _ = error $ "Blarney.Netlist: only names expected in "
                     ++ "second argument to 'netlist'"
-------------------------------------------------------------------------------

----------------------------- Auxiliary functions -----------------------------
lazyZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
lazyZipWith f [] bs = []
lazyZipWith f (a:as) bs = f a (hd bs) : lazyZipWith f as (tail bs)
  where
    hd [] = error "lazyZipWith: incompatible structures"
    hd (a:as) = a

trans :: [[a]] -> [[a]]
trans (x:xs) = lazyZipWith (:) x (trans xs)

groupN :: Int -> [a] -> [[a]]
groupN n [] = []
groupN n xs = take n xs : groupN n (drop n xs)
-------------------------------------------------------------------------------
