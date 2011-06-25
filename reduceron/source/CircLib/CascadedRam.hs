module CircLib.CascadedRam where

import Lava
import Data.List
import CircLib.Bit
import CircLib.BlockRam
import CircLib.Common

data Mem_Inps =
  Mem_Inps { mem_addr  :: [Bit]
           , mem_datIn :: [Bit]
           , mem_we    :: Bit
           }

mem4k :: [Int] -> Mem_Inps -> [Bit]
mem4k =  mem18 4

mem32k :: [Int] -> Mem_Inps -> [Bit]
mem32k =  mem18 32

mem16k :: [Int] -> Mem_Inps -> [Bit]
mem16k =  mem18 16

mem12k :: [Int] -> Mem_Inps -> [Bit]
mem12k =  mem18 12

mem18 :: Int -> [Int] -> Mem_Inps -> [Bit]
mem18 n init inps = select sels' brams
  where
    mkBRam i en = bram18 i $ BRam18_Inps {
                                 bram18_addr = loAddr
                               , bram18_datIn = mem_datIn inps
                               , bram18_we = mem_we inps <&> en
                             }

    brams = zipWith mkBRam (chunks 1024 n init) sels 

    sels = decode hiAddr
    sels' = delay (replicate (length sels) low) sels

    (loAddr, hiAddr) = splitAt 10 (mem_addr inps)


chunks :: Int -> Int -> [a] -> [[a]]
chunks n m xs = ys ++ replicate (m - length ys) []
  where
    ys = groupN n xs

-- Dual-port

type DualMem_Inps = (Mem_Inps, Mem_Inps)

dualMem18 :: Int -> [Int] -> DualMem_Inps -> ([Bit], [Bit])
dualMem18 n init (inpsA, inpsB) = (select selsA' ramsA', select selsB' ramsB')
  where
    mkRam i (enA, enB) = dual18_bram i $
                           Dual18_Inps {
                             dual18_addrA = loAddrA
                           , dual18_addrB = loAddrB
                           , dual18_datInA = mem_datIn inpsA
                           , dual18_datInB = mem_datIn inpsB
                           , dual18_weA = mem_we inpsA <&> enA
                           , dual18_weB = mem_we inpsB <&> enB
                           }

    extra = replicate (length (zip selsA selsB) - n) (replicate 18 low)

    ramsA' = ramsA ++ extra
    ramsB' = ramsB ++ extra

    (ramsA, ramsB) = unzip $ zipWith mkRam (chunks 1024 n init)
                                           (zip selsA selsB)

    (loAddrA, hiAddrA) = splitAt 10 (mem_addr inpsA)
    (loAddrB, hiAddrB) = splitAt 10 (mem_addr inpsB)

    selsA = decode hiAddrA
    selsA' = delay (replicate (length selsA) low) selsA

    selsB = decode hiAddrB
    selsB' = delay (replicate (length selsB) low) selsB


-- Dual-port, wide memories

data Wide_Inps = Wide_Inps {
                   wide_addr  :: [Bit]
                 , wide_datIn :: [Bit] -- 72 bits
                 , wide_we    :: [Bit] -- 4 bit write mask
                 }

type DualWide_Inps = (Wide_Inps, Wide_Inps)

quad :: Int -> [Int] -> DualWide_Inps -> ([Bit], [Bit])
quad m init (inps1, inps2) = foldr combine ([], []) $ map mkRam [0..3]
  where
    inits = [ every 4 init
            , every 4 (drop 1 init)
            , every 4 (drop 2 init)
            , every 4 (drop 3 init)
            ]

    m' = m `div` 4
    addrLen = 10 + ulog2 m'

    addr1 = groupN addrLen (wide_addr inps1)
    addr2 = groupN addrLen (wide_addr inps2)
    dat1 = groupN 18 (wide_datIn inps1)
    dat2 = groupN 18 (wide_datIn inps2)
    we1 = wide_we inps1
    we2 = wide_we inps2

    mkRam n = dualMem18 m' (inits !! n) $
                ( Mem_Inps {
                    mem_addr = addr1 !! n
                  , mem_datIn = dat1 !! n
                  , mem_we = we1 !! n
                  }
                , Mem_Inps {
                    mem_addr = addr2 !! n
                  , mem_datIn = dat2 !! n
                  , mem_we = we2 !! n
                  }
                )

    combine (a, b) (c, d) = (a ++ c, b ++ d)

every n xs = every' 0 n xs

every' n m [] = []
every' n m (x:xs)
  | n == 0 = x : every' (m-1) m xs
  | otherwise = every' (n-1) m xs



log2 n = if n == 1 then 0 else 1 + log2 (n `div` 2)

ulog2 n = log2 (2*n - 1)

qalignInps :: Wide_Inps -> Wide_Inps
qalignInps inps =
  inps { wide_addr = newAddr
       , wide_datIn = concat newDatIn
       , wide_we = newWe
       }
  where
    addr = wide_addr inps
    newAddr = concat $ wordRep (take 2 addr) (drop 2 addr)
    newDatIn = rotateRight (take 2 addr) $ groupN 18 (wide_datIn inps)
    newWe = concat $ rotateRight (take 2 addr) (map (:[]) (wide_we inps))

qalignOuts :: [Bit] -> [Bit] -> [Bit]
qalignOuts lsbs out = concat (rotateLeft lsbs outs)
  where
    outs = groupN 18 out

qalign f (inps1, inps2) =
  (qalignOuts lsbs1 out1, qalignOuts lsbs2 out2)
  where
   (out1, out2) = f (qalignInps inps1, qalignInps inps2)
   lsbs1 = delay [low, low] $ take 2 (wide_addr inps1)
   lsbs2 = delay [low, low] $ take 2 (wide_addr inps2)

dualQuad :: Int -> [Int] -> DualWide_Inps -> ([Bit], [Bit])
dualQuad n init = qalign (quad n init)

dualWide4k :: [Int] -> DualWide_Inps -> ([Bit], [Bit])
dualWide4k = dualQuad 4

dualWide8k :: [Int] -> DualWide_Inps -> ([Bit], [Bit])
dualWide8k = dualQuad 8

dualWide12k :: [Int] -> DualWide_Inps -> ([Bit], [Bit])
dualWide12k = dualQuad 12

dualWide32k :: [Int] -> DualWide_Inps -> ([Bit], [Bit])
dualWide32k = dualQuad 32

{-
--

dwBlock :: [Int] -> DualWide_Inps -> ([Bit], [Bit])
dwBlock init (inps1, inps2) = foldr combine ([], []) $ map mkBRam [0..3]
  where
    inits = [ every 4 init
            , every 4 (drop 1 init)
            , every 4 (drop 2 init)
            , every 4 (drop 3 init)
            ]
    addr1 = groupN 10 (wide_addr inps1)
    addr2 = groupN 10 (wide_addr inps2)
    dat1 = groupN 18 (wide_datIn inps1)
    dat2 = groupN 18 (wide_datIn inps2)
    we1 = wide_we inps1
    we2 = wide_we inps2

    mkBRam n = dual18_bram (inits !! n) $
                 Dual18_Inps {
                   dual18_addrA = addr1 !! n
                 , dual18_addrB = addr2 !! n
                 , dual18_datInA = dat1 !! n
                 , dual18_datInB = dat2 !! n
                 , dual18_weA = we1 !! n
                 , dual18_weB = we2 !! n
                 }

    combine (a, b) (c, d) = (a ++ c, b ++ d)

--every n xs = every' 0 n xs

--every' n m [] = []
--every' n m (x:xs)
--  | n == 0 = x : every' (m-1) m xs
--  | otherwise = every' (n-1) m xs


alignInps :: Wide_Inps -> Wide_Inps
alignInps inps =
  inps { wide_addr = newLoAddr ++ hiAddr
       , wide_datIn = concat newDatIn
       , wide_we = newWe
       }
  where
    (loAddr, hiAddr) = splitAt 12 (wide_addr inps)

    newLoAddr = concat $ wordRep (take 2 loAddr) (drop 2 loAddr)

    newDatIn = rotateRight (take 2 loAddr) $ groupN 18 (wide_datIn inps)

    newWe = concat $ rotateRight (take 2 loAddr) (map (:[]) (wide_we inps))

alignOuts :: [Bit] -> [Bit] -> [Bit]
alignOuts lsbs out = concat (rotateLeft lsbs outs)
  where
    outs = groupN 18 out

align f (inps1, inps2) =
  (alignOuts lsbs1 out1, alignOuts lsbs2 out2)
  where
   (out1, out2) = f (alignInps inps1, alignInps inps2)
   lsbs1 = delay [low, low] $ take 2 (wide_addr inps1)
   lsbs2 = delay [low, low] $ take 2 (wide_addr inps2)

--dualWide4k :: [Int] -> DualWide_Inps -> ([Bit], [Bit])
--dualWide4k init = align (dwBlock init)

dualWidePipeMem :: Int -> [Int] -> DualWide_Inps -> ([Bit], [Bit])
dualWidePipeMem n init (inps1, inps2) =
  (select sels1' rams1, select sels2' rams2)
  where
    mkRam i (en1, en2) = dwBlock i $
                           (  Wide_Inps {
                                wide_addr = loAddr1
                              , wide_datIn = wide_datIn inps1
                              , wide_we = map (<&> en1) (wide_we inps1)
                              }
                           ,  Wide_Inps {
                                wide_addr = loAddr2
                              , wide_datIn = wide_datIn inps2
                              , wide_we = map (<&> en2) (wide_we inps2)
                              }
                           )

    (loAddr1, hiAddr1) = splitAt 40 (wide_addr inps1)
    (loAddr2, hiAddr2) = splitAt 40 (wide_addr inps2)

    (rams1, rams2) = unzip $
                       zipWith mkRam (chunks 4096 n init) (zip sels1 sels2)

    sels1 = decode hiAddr1
    sels1' = delay (replicate (length sels1) low) sels1

    sels2 = decode hiAddr2
    sels2' = delay (replicate (length sels2) low) sels2

--dualWide32k :: [Int] -> DualWide_Inps -> ([Bit], [Bit])
--dualWide32k init = align (dualWidePipeMem 8 init)

--dualWide12k :: [Int] -> DualWide_Inps -> ([Bit], [Bit])
--dualWide12k init = align (dualWidePipeMem 3 init)
-}
