module CachingOctostack
  ( Octostack     -- data Octostack n
  , newOctostack  -- :: New (Octostack n)
  , size          -- :: Octostack n -> Size
  , newSize       -- :: Octostack n -> Size
  , tops          -- :: Octostack n -> Vec N8 (Word n)
  , tops2         -- :: Octostack n -> Vec N8 (Word n)
  , update        -- :: Word N4       {- Offset, in range -8 to +7 -}
                  -- -> Word N8       {- Write mask -}
                  -- -> [Word n]      {- Values to write -}
                  -- -> Octostack n   {- Stack to update -}
                  -- -> Recipe
  ) where

-- A caching octostack is like an octostack except that the top 8
-- elements are cached in registers to reduce propagation delay; there
-- is no delay when reading from the stack, but there is still a small
-- delay when writing.  For good performance, compute the 'offset'
-- parameter to the 'update' operation quickly.  See Memo 27 for
-- details.

import Prelude hiding (Word, (.))
import Lava
import Recipe
import Data.List
import Bytecode
import Control.Monad (zipWithM_)

-- Parameters

type Size = StackAddr -- Stores the number of elements on the stack
ramAlgorithm = Width18

-- Implementation

data Octostack n =
  Octostack {
    offset  :: Sig N4
  , writeEn :: Sig N8
  , dataIn  :: Vec N8 (Sig n)
  , size    :: Size
  , newSize :: Size
  , tops    :: Vec N8 (Word n)
  , tops2   :: Vec N8 (Word n)
  }

newOctostack f annotation =
  do offsetSig <- newSig
     writeEnSig <- newSig
     dataInSigs <- vsequence (vreplicate n8 newSig)

     let (sz, nsz, ts, ts2) = cachingOctostack f annotation
                                (val offsetSig)
                                (velems $ val writeEnSig)
                                (velems $ vmap val $ dataInSigs)

     return $ Octostack {
                offset  = offsetSig
              , writeEn = writeEnSig
              , dataIn  = dataInSigs
              , size    = sz
              , newSize = nsz
              , tops    = Vec ts
              , tops2   = Vec ts2
              }

octostack' annotation offset writes pushes = (size, newSize, outputs)
  where
    size       = delay 0 newSize
    newSize    = size + extend offset
    loSize     = vtake n3 size
    loNewSize  = vtake n3 newSize
    rotVal     = decode (velems loNewSize)
    rotVal'    = delay (replicate 8 low) rotVal

    addrs      = [ delay (fromIntegral (- i)) (newAddrs !! (i-1))
                 | i <- [1..8] ]
    newAddrs   = [(a + extend offset) `ofSize` vsize size | a <- addrs]
    hiNewAddrs = map (vdrop n3) newAddrs

    ramIns     = reverse (rotLeft rotVal' pushes)
    ramWrites  = reverse (rotl rotVal writes)
    ramOuts    = map (ram [] ("octostack_" ++ annotation) ramAlgorithm)
                     (zipWith3 RamInputs ramIns hiNewAddrs ramWrites)

    outputs    = rotRight rotVal (reverse ramOuts)

cachingOctostack :: N n => String -> (Vec N8 (Word n) -> Vec N8 (Word n))
  -> Word N4 -> [Bit] -> [Word n] -> (Size, Size, [Word n], [Word n])
cachingOctostack annotation f offset writes pushes =
    (sz, nsz, map Vec regOuts, map Vec regOuts2)
  where
    vec xs = Vec xs `sameSize` (head pushes)

    (sz, nsz, ramOuts) = octostack' annotation offset pushMask (map vec regOuts)

    (o1, o2)      = halve $ decode (velems offset)
    right         = zipWith (<|>) o1 o2
    popMask       = reverse $ tal' $ reverse o2
    pushMask      = tal o1

    rotated       = rotateRight right regOuts
    regIns        = zipWith5 choose
                             (map velems $ velems $ f $ Vec $ map Vec rotated)
                             (map velems ramOuts)
                             (map velems pushes)
                             popMask
                             writes
    regOuts       = delay zeroes regIns
    regOuts2      = delay zeroes regIns

    zeroes        = replicate 8 (replicate elemWidth low)
    elemWidth     = value (vsize (head pushes))

choose :: [Bit] -> [Bit] -> [Bit] -> Bit -> Bit -> [Bit]
choose rotOut ramOut toPush pop write =
  pick [ (pop, ramOut)
       , (write, toPush)
       , (inv pop <&> inv write, rotOut)
       ]

rotRight :: [Bit] -> [Word n] -> [Word n]
rotRight n xs = map Vec $ rotateRight n $ map velems xs

rotLeft :: [Bit] -> [Word n] -> [Word n]
rotLeft n xs = map Vec $ rotateLeft n $ map velems xs

update :: Word N4 -> Word N8 -> [Word n] -> Octostack n -> Recipe
update o en xs s = do s.offset <== o
                      s.writeEn <== en
                      zipWithM_ (<==) (s.dataIn.velems) xs

-- Example usage

example :: Octostack N18 -> Recipe
example s = do s.update 3 (high +> high +> high +> 0) [4, 5, 6]
               tick -- PUSH [4, 5, 6]
               s.update 2 (high +> high +> 0) [8, 9]
               tick -- PUSH [8, 9]
               s.update (-1) 0 []
               tick -- POP 1
               s.update 4 (high +> high +> high +> high +> 0) [1, 2, 1, 2]
               tick -- PUSH [1, 2, 1, 2]
               s.update 4 (high +> high +> high +> high +> 0) [6, 7, 6, 7]
               tick -- PUSH [6, 7, 6, 7]
               s.update (-3) 0 []
               tick -- POP 3
               s.update (-2) (high +> 0) [10]
               tick -- POP 3, PUSH [10]
               s.update 2 (high +> high +> 0) [8, 9]
               tick -- PUSH [8, 9]
               s.update 0 (high +> 0) [55]
               tick -- PUSH [8, 9]
               tick
               tick


simExample = simRecipe (newOctostack "example_" id) example tops
