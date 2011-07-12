{- |

A library for writing behavioural descriptions in York Lava, inspired
by Page and Luk's \"Compiling Occam into Field-Programmable Gate
Arrays\", Oxford Workshop on Field Programmable Logic and
Applications, 1991.  Features explicit clocking, signals as well as
registers, shared procedure calls, an optimiser, and scope for other
static analyses.  The implementation is short and sweet!  Used in the
implementation of the Reduceron, a graph reduction machine for Xilinx
FPGAs.

See @REDUCERON MEMO 23@ - included in the package and available at
<http://www.cs.york.ac.uk/fp/reduceron/> - for further details and
examples.

-}

module Recipe
  ( -- * Recipe constructs
    Recipe ( Skip, Tick, Seq, Par, While, Do )
  , (|>)
  , call
  , Var ( val, (<==) )
  , (!)
  , (-->)

    -- * The /New/ monad
  , New

    -- * Mutable variables: registers and signals
  , Reg
  , newReg
  , newRegInit
  , Sig
  , newSig
  , newSigDef

    -- * Shared procedures
  , Proc()
  , newProc

    -- * Running recipes
  , recipe

    -- * Simulating recipes
  , simRecipe
  ) where

import Lava
import List
import Maybe

type VarId = Int

data Recipe
  = Skip             -- ^ The most basic recipe; does nothing.
  | Tick             -- ^ Does nothing, but takes one clock-cycle to do it.
  | VarId := [Bit]
  | Seq [Recipe]     -- ^ Sequential composition of recipes.
  | Par [Recipe]     -- ^ Fork-Join parallel composition of recipes.
  | Cond Bit Recipe
  | While Bit Recipe -- ^ Run a recipe while a condition holds.
  | Do Recipe Bit    -- ^ Like 'While', but condition is checked
                     --   at the end of each iteration.

infixr 1 |>
-- | Run a recipe only if a condition holds.
(|>) :: Bit -> Recipe -> Recipe
b |> r = Cond b r

time :: Recipe -> Maybe Int
time Skip = Just 0
time Tick = Just 1
time (v := e) = Just 0
time (Seq rs) = sum `fmap` mapM time rs
time (Par rs) = foldr max 0 `fmap` mapM time rs
time (Cond b r)
  | time r == Just 0 = Just 0
  | otherwise = Nothing
time (While b r) = Nothing
time (Do r b) = Nothing

finite :: Recipe -> Bool
finite r = isJust (time r)

slowest :: [Recipe] -> Int
slowest = snd . maximum . flip zip [0..] . map time

type Schedule = [(Bit, VarId, [Bit])]

sched :: Bit -> Recipe -> (Bit, Schedule)
sched go Skip = (go, [])
sched go Tick = (delay low go, [])
sched go (v := e) = (go, [(go, v, e)])
sched go (Seq rs) = (done, concat ss)
  where (done, ss) = mapAccumL sched go rs
sched go (Par rs)
  | all finite rs = (dones !! slowest rs, concat ss)
  | otherwise = (sync dones, concat ss)
  where (dones, ss) = unzip (map (sched go) rs)
sched go (Cond c r)
  | time r == Just 0 = (go, s)
  | otherwise = (done <|> (go <&> inv c), s)
  where (done, s) = sched (go <&> c) r
sched go (While c r) = (ready <&> inv c, s)
  where ready = go <|> done
        (done, s) = sched (ready <&> c) r
sched go (Do r c) = (done <&> inv c, s)
  where ready = go <|> (done <&> c)
        (done, s) = sched ready r

sync :: [Bit] -> Bit
sync [x] = x
sync xs = let done = andG [setReset x done | x <- xs] in done

setReset :: Bit -> Bit -> Bit
setReset s r = let out = s <|> delay low (out <&> inv r) in out

infix 5 <==

-- | Mutable variables; named locations that can be read from and assigned to.
class Var v where
  -- | Return the value of a variable of width /n/.
  val :: v n -> Word n
  -- | Assign a value to a variable of width /n/.
  (<==) :: v n -> Word n -> Recipe

-- | /Signal variables/: assignments to a signal come into effect in the
-- current clock-cycle, but last only for the duration of that
-- clock-cycle; if a signal not assigned to in a clock-cycle
-- then its value will be its /default/ value which is zero unless
-- otherwise specified.
data Sig n = Sig { sigId :: VarId, sigVal :: Word n }

-- | /Register variables/: assignments to a register come into effect in
-- the clock-cycle /after/ the assignment is performed; the initial
-- value of a register is zero unless otherwise specified.
data Reg n = Reg { regId :: VarId, regVal :: Word n }

instance Generic (Sig n) where
  generic (Sig v x) = cons (Sig v) >< x

instance Show (Sig n) where
  show (Sig v x) = show x

instance Generic (Reg n) where
  generic (Reg v x) = cons (Reg v) >< x

instance Show (Reg n) where
  show (Reg v x) = show x

instance Var Sig where
  val s = sigVal s
  s <== x = sigId s := velems x

instance Var Reg where
  val r = regVal r
  r <== x = regId r := velems x

-- | It's a monad; that's all you need to know.
type New a = RWS Schedule (Bit, Recipe) VarId a

fresh :: New VarId
fresh = do { v <- get ; set (v+1) ; return v }

newSig :: N n => New (Sig n)
newSig = do { v <- fresh ; s <- ask ; return $ sig v $ assigns v s }

newSigDef :: N n => Word n -> New (Sig n)
newSigDef d = do { v <- fresh ; s <- ask ; return $ sigDef d v $ assigns v s }

assigns :: VarId -> Schedule -> [(Bit, [Bit])]
assigns v s = [(b, e) | (b, w, e) <- s, v == w]

sig :: N n => VarId -> [(Bit, [Bit])] -> Sig n
sig v as = Sig v (if null as then 0 else Vec $ pick as)

sigDef :: N n => Word n -> VarId -> [(Bit, [Bit])] -> Sig n
sigDef d v as = Sig v (if null as then d else Vec $ pickDef (velems d) as)

pickDef :: [Bit] -> [(Bit,[Bit])] -> [Bit]
pickDef def ps = pick ((sel, def):ps)
  where sel = inv (orG (map fst ps))

newRegInit :: N n => Word n -> New (Reg n)
newRegInit i = do { v <- fresh ; s <- ask ; return $ reg v i $ assigns v s }

newReg :: N n => New (Reg n)
newReg = newRegInit 0

reg :: N n => VarId -> Word n -> [(Bit, [Bit])] -> Reg n
reg v i as = Reg v (if null as then i else Vec out)
  where out = delayEn (velems i) (orG $ map fst as) (pick as)

recipe :: New a          -- ^ A state creator
       -> (a -> Recipe)  -- ^ A recipe which manipulates the state
       -> Bit            -- ^ A start pulse
       -> (a, Bit)       -- ^ A finish pulse and the resulting state
recipe n f go =
  let (_, rs, a) = runRWS n (s ++ concat ss) 0
      ss = map (snd . uncurry sched) rs
      (done, s) = sched go (f a)
  in  (a, done)

simRecipe :: Generic b
          => New a          -- ^ A state creator
          -> (a -> Recipe)  -- ^ A recipe which manipulates the state
          -> (a -> b)       -- ^ A selector over the state
          -> b              -- ^ The part of the state you selected
simRecipe n f k = fst
                $ head
                $ dropWhile (not . bitToBool . snd)
                $ simulate
                $ first k
                $ recipe n f (delay high low)
  where first f (a, b) = (f a, b)

infixl 9 !
-- | Reverse function application.
(!) :: a -> (a -> b) -> b
x ! f = f x

infix 1 -->
-- | Infix constructor for pairs.
(-->) :: a -> b -> (a, b)
a --> b = (a, b)

-- Shared procedures

data Proc = Proc { procGo :: Sig N1, procDone :: Bit }

-- | Capture a recipe as shared procedure that can be called whenever
-- desired; needless to say, the programmer should avoid parallel
-- calls to the same shared procedure!
newProc :: Recipe -> New Proc
newProc r =
  do { go <- newSig
     ; done <- newSig
     ; write (go!val!vhead, Seq [ r, done <== vsingle high ])
     ; return (Proc go (done!val!vhead))
     }

-- | Call a procedure.
call :: Proc -> Recipe
call p = Seq [ p!procGo <== 1, While (p!procDone!inv) Tick ]

-- Standard reader/writer/state monad

data RWS r w s a = RWS { runRWS :: r -> s -> (s, [w], a) }

instance Monad (RWS r w s) where
  return a = RWS (\r s -> (s, [], a))
  m >>= f = RWS (\r s -> let (s0, w0, a) = runRWS m r s
                             (s1, w1, b) = runRWS (f a) r s0
                         in  (s1, w0 ++ w1, b))

get :: RWS r w s s
get = RWS (\r s -> (s, [], s))

set :: s -> RWS r w s ()
set s' = RWS (\r s -> (s', [], ()))

ask :: RWS r w s r
ask = RWS (\r s -> (s, [], r))

write :: w -> RWS r w s ()
write w = RWS (\r s -> (s, [w], ()))
